module ASM
( assemble
, Config (..)
, safePlus
, safeMinus
, safeDowncast
, safeAlign -- exported for tests
) where

import Common

import ASM.Types

import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Numeric.Decimal.BoundedArithmetic as B
import qualified Data.Either.Extra as Either
import qualified Control.Exception as Exception

-- | An assembler that produces one object, without imported/exported
-- references. There are three passes (see assemble function):
--  1. collect a Map from label to location information, (scanLabels)
--  2. replace labels with the required references by using the map from (1)
--     (solveReferences)
--  3. encode solved references as ByteString
--
-- The module is polymorphic on a data type typically denotated with 'op', which
-- implements type classes 'Encodable' and 'ByteSized'. Values of this datatype
-- in a sequence are elements that can be converted to a binary format. The
-- module facilitates outputting several types of references to other 'op'
-- elements in the sequence by means of the 'Reference' type.

boundedBinopMapEx
  :: (Address a)
  => (Exception.SomeException -> AssemblyError)
  -> (a -> a -> Either Exception.SomeException a)
  -> a
  -> a
  -> Either AssemblyError a
boundedBinopMapEx ex op o1 o2
  = Either.mapLeft ex $ op o1 o2

addOffsets
  :: (ByteSized op, Address address)
  => Config address
  -> PositionInfo address
  -> op a
  -> Either AssemblyError (PositionInfo address)
addOffsets Config {..} a@PositionInfo {..} op
  = do
    newIA  <- (fromIntegral . sizeIA)  op `safePlus` piIA
    newRVA <- (fromIntegral . sizeRVA) op `safePlus` piRelativeVA
    newVA  <- acVirtualBaseAddress        `safePlus` newRVA
    pure $ a
      { piIA         = newIA
      , piRelativeVA = newRVA
      , piVA         = newVA
      }

_alignIA
  :: (Address address)
  => Natural
  -> PositionInfo address
  -> Either AssemblyError (PositionInfo address)
_alignIA n a@PositionInfo {..}
  = do
    toAddNat <- piIA `safeAlign` n
    toAdd    <- safeDowncast (fromIntegral toAddNat)
    newIA    <- toAdd `safePlus` piIA
    pure $ a { piIA = newIA }

-- | How many bytes to add to reach a multiple of n
safeAlign :: Address a => a -> Natural -> Either AssemblyError Natural
safeAlign _       0 = Left AlignTo0
safeAlign address n = Right $
    assert (n >= exceed) $
      if exceed == 0
        then 0
        else n - exceed
  where
    exceed :: Natural
    exceed = fromIntegral address `mod` n

-- | Extract all labels in the sequence in a Map. The key is the label and the
-- value is positional information (PositionInfo)
scanLabels
  :: (Address address, Functor op, ByteSized op)
  => Config address
  -> Seq.Seq (Atom (op (Reference LabelText)))
  -> Either AssemblyError (Map.Map LabelText (PositionInfo address))
scanLabels c@Config {..} atoms = aslsLabels <$> foldM scan initialState atoms
  where
    initialState = StateLabelScan
      (PositionInfo minBound minBound acVirtualBaseAddress) Map.empty

    scan s@StateLabelScan {..} (AOp op) = do
      newPosition <- addOffsets c asPosition op
      pure s { asPosition = newPosition }
    scan s@StateLabelScan {..} (ALabel labelText) =
      pure s { aslsLabels = Map.insert labelText asPosition aslsLabels }
    -- scan s@StateLabelScan {..} (AAlignIA n) = do
    --   newPosition <- alignIA asPosition
    --   pure s {  asPosition = newPosition}

safePlus :: (Address a) => a -> a -> Either AssemblyError a
safePlus = boundedBinopMapEx (Arithmetic . ExceptionWrap) B.plusBounded

safeMinus :: (Address a) => a -> a -> Either AssemblyError a
safeMinus = boundedBinopMapEx (Arithmetic . ExceptionWrap) B.minusBounded

safeDowncast :: (Integral a, Bounded a) => Integer -> Either AssemblyError a
safeDowncast = Either.mapLeft (Arithmetic . ExceptionWrap)
             . B.fromIntegerBounded

-- | Solve label references to dictionary addresses.
solveReferences
  :: (Traversable op, Address address, ByteSized op)
  => Config address
  -> Map.Map LabelText (PositionInfo address)
  -> Seq.Seq (Atom (op (Reference LabelText)))
  -> Either AssemblyError (Seq.Seq (Atom (op (Reference address))))
solveReferences c labelDictionary atoms
    = asrsAtoms <$> foldM (solveAtomReferences c labelDictionary) initialState
        atoms
  where
    initialState = StateReferenceSolve Seq.empty

-- | Solve references possibly present in an Atom
solveAtomReferences
  :: (Traversable op, Address address, ByteSized op)
  => Config address
  -> Map.Map LabelText (PositionInfo address)
  -> StateReferenceSolve op address
  -> Atom (op (Reference LabelText))
  -> Either AssemblyError (StateReferenceSolve op address)
solveAtomReferences _ labelDictionary s@StateReferenceSolve {..} = go
  where
    go (AOp opUnsolved) = do
      opSolved <- AOp <$> Prelude.mapM solveReference opUnsolved
      pure s
        { asrsAtoms = asrsAtoms Seq.|> opSolved
        }
    go (ALabel _) = pure s -- self-solve labels? no need, discard them...

    query labelText
      = Either.maybeToEither (ReferenceMissing labelText)
          (Map.lookup labelText labelDictionary)

    solveReference (RefVA labelText) =
      RefVA . piVA <$> query labelText
    solveReference (RefRelativeVA labelText) =
      RefRelativeVA . piRelativeVA <$> query labelText
    solveReference (RefIA labelText) =
      RefIA . piIA <$> query labelText

-- | Encode solved references to ByteString. Keeps track of current positions
encodeSolved
  :: forall op address
  .  (Address address, ByteSized op, Encodable op)
  => Config address
  -> Seq.Seq (Atom (op (Reference address)))
  -> Either AssemblyError BS.ByteString
encodeSolved c@Config {..} atoms
    = sesEncoded <$> foldM encodeAtom initialState atoms
  where
    initialState = StateEncodeSolved
      (PositionInfo minBound minBound acVirtualBaseAddress) ""

    encodeAtom s (ALabel _) = pure s
    encodeAtom s@StateEncodeSolved {..} (AOp op)
      = do
        encodedOp   <- encode sesPosition op
        opLength    <- safeDowncast $ fromIntegral $ BS.length encodedOp
        newPosition <- assert (sizeRVA op == opLength) $
          addOffsets c sesPosition op
        pure s
          { sesPosition = newPosition
          , sesEncoded = sesEncoded <> encodedOp
          }

assemble
  :: (Address address, Traversable op, ByteSized op, Encodable op)
  => Config address
  -> Seq.Seq (Atom (op (Reference LabelText)))
  -> Either AssemblyError BS.ByteString
assemble cfg input
  = do
    labelMap <- scanLabels cfg input
    solveReferences cfg labelMap input >>= encodeSolved cfg
