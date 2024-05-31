{-# LANGUAGE ScopedTypeVariables #-}

module ASM
( assemble
, Config (..)
, safePlus
, safeMinus
, safeDowncast
) where

import Common

import ASM.Types

import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Numeric.Decimal.BoundedArithmetic as B
import qualified Data.Either.Extra as Either
import qualified Control.Exception as Exception
import qualified Data.Foldable as F

-- | A simple assembler that produces one object, without imported/exported
-- references. There are three passes:
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
  => AddressInfo address
  -> op a
  -> Either AssemblyError (AddressInfo address)
addOffsets a@AddressInfo {..} op
  = do
    newIA  <- (fromIntegral . sizeIA)  op `safePlus` aiIA
    newRVA <- (fromIntegral . sizeRVA) op `safePlus` aiRelativeVA
    newVA  <- (fromIntegral . sizeRVA) op `safePlus` aiVA
    pure $ a
      { aiIA = newIA
      , aiRelativeVA = newRVA
      , aiVA = newVA
      }

-- | Extract all labels in the sequence in a Map
scanLabels
  ::
  ( Address address
  , Functor op
  , ByteSized op
  )
  => Config address
  -> Seq.Seq (Atom (op Reference))
  -> Either AssemblyError (Map.Map LabelText (AddressInfo address))
scanLabels Config {..} atoms = aslsLabels <$> foldM scan initialState atoms
  where
    initialState = StateLabelScan
      { asPosition = AddressInfo minBound minBound acVirtualBaseAddress
      , aslsLabels = Map.empty
      }
    scan s@StateLabelScan {..} (AOp op) = do
      newPosition <- addOffsets asPosition op
      pure s
        { asPosition = newPosition
        }
    scan s@StateLabelScan {..} (ALabel labelText) =
      pure s
        { aslsLabels = Map.insert labelText asPosition aslsLabels
        }

safePlus :: (Address a) => a -> a -> Either AssemblyError a
safePlus = boundedBinopMapEx (Arithmetic . SEW) B.plusBounded

safeMinus :: (Address a) => a -> a -> Either AssemblyError a
safeMinus = boundedBinopMapEx (Arithmetic . SEW) B.minusBounded

safeDowncast :: (Integral a, Bounded a) => Integer -> Either AssemblyError a
safeDowncast = Either.mapLeft (Arithmetic . SEW) . B.fromIntegerBounded

-- | Solve label references to dictionary addresses. Again it keeps track of
-- the offset it is at like in scanLabels. Perhaps this duplicate operation
-- could be factored out, but it shouldn't be too expensive...
solveReferences
  :: (Traversable op, Address address, ByteSized op)
  => Config address
  -> Map.Map LabelText (AddressInfo address)
  -> Seq.Seq (Atom (op Reference))
  -> Either AssemblyError (Seq.Seq (Atom (op (SolvedReference address))))
solveReferences c labelDictionary atoms
    = asrsAtoms <$> foldM (solveAtomReferences c labelDictionary) initialState
        atoms
  where
    initialState = StateReferenceSolve
      { asrsAtoms = Seq.empty
      , asrsRelativeVAOffset = 0
      }

-- | Solve references possibly present in an Atom
solveAtomReferences
  :: (Traversable op, Address address, ByteSized op)
  => Config address
  -> Map.Map LabelText (AddressInfo address)
  -> StateReferenceSolve op address
  -> Atom (op Reference)
  -> Either AssemblyError (StateReferenceSolve op address)
solveAtomReferences Config {..} labelDictionary s@StateReferenceSolve {..} = go
  where
    go (AOp opUnsolved) = do
      opSolved <- AOp <$> Prelude.mapM (solveReference asrsRelativeVAOffset)
        opUnsolved
      newRVA <- (fromIntegral . sizeRVA) opUnsolved `safePlus` asrsRelativeVAOffset
      pure s
        { asrsAtoms = asrsAtoms Seq.|> opSolved
        , asrsRelativeVAOffset = newRVA
        }
    go (ALabel _) = pure s -- self-solve labels? no need, discard them...

    query labelText
      = Either.maybeToEither
          (ReferenceMissing labelText)
          (Map.lookup labelText labelDictionary)

    -- TODO: Save the first parameter somewhere...
    solveReference _ (RefVA labelText) = do
      rva <- aiRelativeVA <$> query labelText
      SolvedVA <$> rva `safePlus` acVirtualBaseAddress
    solveReference _ (RefRelativeVA labelText) =
      SolvedRelativeVA . aiRelativeVA <$> query labelText
    solveReference _ (RefIA labelText) =
      SolvedIA . aiIA <$> query labelText

-- | Encode solved references to ByteString
encodeSolved
  :: forall op address
  .  ( Address address
     , ByteSized op
     , Encodable (op (SolvedReference address)))
  => Config address
  -> Seq.Seq (Atom (op (SolvedReference address)))
  -> Either AssemblyError BS.ByteString
encodeSolved Config {..} atoms
    = sesEncoded <$> foldM encodeAtom initialState atoms
  where
    initialState :: StateEncodeSolved address
    initialState = StateEncodeSolved
      (AddressInfo minBound minBound acVirtualBaseAddress) ""

    encodeAtom s (ALabel _) = pure s
    encodeAtom s@StateEncodeSolved {..} (AOp op)
      = do
        encodedOp   <- BS.pack . F.toList <$> encode sesPosition op
        opLength    <- safeDowncast $ fromIntegral $ BS.length encodedOp
        newPosition <- assert (sizeRVA op == opLength) $
          addOffsets sesPosition op
        pure s
          { sesPosition = newPosition
          , sesEncoded = sesEncoded <> encodedOp
          }

assemble
  ::
  ( Address address
  , Traversable op
  , ByteSized op
  , Encodable (op (SolvedReference address))
  )
  => Config address
  -> Seq.Seq (Atom (op Reference))
  -> Either AssemblyError BS.ByteString
assemble cfg input
  = do
    labelMap <- scanLabels cfg input
    solveReferences cfg labelMap input >>= encodeSolved cfg
