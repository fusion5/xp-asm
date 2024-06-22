module ASM
( assemble
, Config (..)
) where

import Common

import ASM.Types

import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Either.Extra as Either

-- | An assembler that produces one object, without imported/exported
-- references. There are three passes (see assemble function):
--  1. collect a Map from label to location information, (scanLabels)
--  2. replace labels with the required references by using the map from (1)
--     (solveReferences)
--  3. encode solved references as ByteString
--
-- The module is polymorphic on a data type typically denotated with 'op', which
-- implements type class 'Encodable'. Values of this datatype
-- in a sequence are elements that can be converted to a binary format. The
-- module facilitates outputting several types of references to other 'op'
-- elements in the sequence by means of the 'Reference' type.

addOffsets
  :: (Encodable op, Address address)
  => Config address
  -> PositionInfo
  -> op a
  -> Either AssemblyError PositionInfo
addOffsets Config {..} a@PositionInfo {..} op
  = do
    basePosition <- integralToPosition acVirtualBaseAddress
    addImage     <- integralToPosition $ sizeIA  op -- redundant really
    addMemory    <- integralToPosition $ sizeRVA op -- redundant really
    pure $ a
      { piIA         = piIA `add` addImage
      , piRelativeVA = piRelativeVA `add` addMemory
      , piVA         = piRelativeVA `add` addMemory `add` basePosition
      }

_alignIA
  :: Natural
  -> PositionInfo
  -> Either AssemblyError PositionInfo
_alignIA n a@PositionInfo {..}
  = do
    delta <- piIA `align` n
    pure $ a { piIA = piIA `add` delta }

-- | Extract all labels in the sequence in a Map. The key is the label and the
-- value is positional information (PositionInfo)
scanLabels
  :: (Address address, Functor op, Encodable op)
  => Config address
  -> Seq.Seq (Atom (op (Reference LabelText)))
  -> Either AssemblyError (Map.Map LabelText PositionInfo)
scanLabels c@Config {..} atoms = do
  basePosition <- integralToPosition acVirtualBaseAddress
  aslsLabels <$> foldM scan (initialState basePosition) atoms
  where
    initialState basePosition = StateLabelScan
      (PositionInfo zero zero basePosition) Map.empty

    scan s@StateLabelScan {..} (AOp op) = do
      newPosition <- addOffsets c asPosition op
      pure s { asPosition = newPosition }
    scan s@StateLabelScan {..} (ALabel labelText) =
      pure s { aslsLabels = Map.insert labelText asPosition aslsLabels }
    -- scan s@StateLabelScan {..} (AAlignIA n) = do
    --   newPosition <- alignIA asPosition
    --   pure s {  asPosition = newPosition}

-- | Solve label references to dictionary addresses.
solveReferences
  :: (Traversable op, Address address, Encodable op)
  => Config address
  -> Map.Map LabelText PositionInfo
  -> Seq.Seq (Atom (op (Reference LabelText)))
  -> Either AssemblyError (Seq.Seq (Atom (op (Reference address))))
solveReferences c labelDictionary atoms
    = asrsAtoms <$> foldM (solveAtomReferences c labelDictionary) initialState
        atoms
  where
    initialState = StateReferenceSolve Seq.empty

-- | Solve references possibly present in an Atom
solveAtomReferences
  :: forall address op
  .  (Traversable op, Address address, Encodable op)
  => Config address
  -> Map.Map LabelText PositionInfo
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

    query labelText = Either.maybeToEither (ReferenceMissing labelText)
                        (Map.lookup labelText labelDictionary)

    addressOf labelText f = query labelText >>= positionDowncast . f

    solveReference
      :: Reference LabelText -> Either AssemblyError (Reference address)
    solveReference (RefVA labelText) =
      RefVA         <$> addressOf labelText piVA
    solveReference (RefRelativeVA labelText) =
      RefRelativeVA <$> addressOf labelText piRelativeVA
    solveReference (RefIA labelText) =
      RefIA         <$> addressOf labelText piIA

-- | Encode solved references to ByteString. Keeps track of current positions
encodeSolved
  :: forall op address
  .  (Address address, Encodable op)
  => Config address
  -> Seq.Seq (Atom (op (Reference address)))
  -> Either AssemblyError BS.ByteString
encodeSolved c@Config {..} atoms
    = do
      basePosition <- integralToPosition acVirtualBaseAddress
      sesEncoded <$> foldM encodeAtom (initialState basePosition) atoms
  where
    initialState basePosition = StateEncodeSolved
      (PositionInfo zero zero basePosition) ""

    encodeAtom s (ALabel _) = pure s
    encodeAtom s@StateEncodeSolved {..} (AOp op)
      = do
        encodedOp   <- encode sesPosition op
        let opLength = fromIntegral $ BS.length encodedOp
        newPosition <-
          assert (opLength == sizeRVA op) $
            assert (opLength == sizeIA op) $
              addOffsets c sesPosition op
        pure s
          { sesPosition = newPosition
          , sesEncoded = sesEncoded <> encodedOp
          }

assemble
  :: (Address address, Traversable op, Encodable op)
  => Config address
  -> Seq.Seq (Atom (op (Reference LabelText)))
  -> Either AssemblyError BS.ByteString
assemble cfg input
  = do
    labelMap <- scanLabels cfg input
    solveReferences cfg labelMap input >>= encodeSolved cfg
