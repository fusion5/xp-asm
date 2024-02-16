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

-- | A simple assembler that produces one object, without imported/exported references.
-- There are two passes:
--  1. collect a Map from label to location information, (scanLabels)
--  2. replace labels with the required references by using the map from (1) (solveReferences)

boundedBinopMapEx
  :: (Ord a, Num a, Bounded a)
  => (Exception.SomeException -> AssemblyError)
  -> (a -> a -> Either Exception.SomeException a)
  -> a
  -> a
  -> Either AssemblyError a
boundedBinopMapEx ex op o1 o2
  = Either.mapLeft ex $ op o1 o2

-- | Extract all labels in the sequence in a Map
scanLabels
  ::
  ( Num address
  , Functor op
  , ByteSized (op (Reference LabelText))
  , Ord address
  , Bounded address
  )
  => Seq.Seq (Atom (op (Reference LabelText)))
  -> Either AssemblyError (Map.Map LabelText (AddressInfo address))
scanLabels s = aslsLabels <$> foldM scanAtom initialState s
  where
    initialState = StateLabelScan
      { aslsIAOffset = 0
      , aslsRelativeVAOffset = 0
      , aslsLabels = Map.empty
      }

safePlus :: (Ord a, Num a, Bounded a) => a -> a -> Either AssemblyError a
safePlus = boundedBinopMapEx (Arithmetic . SEW) B.plusBounded

safeMinus :: (Ord a, Num a, Bounded a) => a -> a -> Either AssemblyError a
safeMinus = boundedBinopMapEx (Arithmetic . SEW) B.plusBounded

safeDowncast :: (Integral a, Bounded a) => Integer -> Either AssemblyError a
safeDowncast = Either.mapLeft (Arithmetic . SEW) . B.fromIntegerBounded

-- This might be expensive...
operationWidth :: (Num address, ByteSized op) => op -> address
operationWidth = fromIntegral . sizeof

scanAtom
  :: (ByteSized ops, Num address, Ord address, Bounded address)
  => StateLabelScan address
  -> Atom ops
  -> Either AssemblyError (StateLabelScan address)
scanAtom s@StateLabelScan {..} = go
  where
    go (AOp op) = do
        newIA  <- operationWidth op `safePlus` aslsIAOffset
        newRVA <- operationWidth op `safePlus` aslsRelativeVAOffset
        pure s {
          aslsIAOffset  = newIA
        , aslsRelativeVAOffset = newRVA
        }
    go (ALabel labelText) =
      let
        aiIA  = aslsIAOffset
        aiRelativeVA = aslsRelativeVAOffset
      in pure s {
        aslsLabels = Map.insert labelText (AddressInfo {..}) aslsLabels
      }

-- Solve label references to dictionary addresses. Again we keep track of the offset we are at
-- like in scanLabels. Perhaps this duplicate operation could be factored out, but it shouldn't
-- be too expensive...
solveReferences
  :: (Traversable op, Address address, ByteSized (op (Reference LabelText)))
  => Config address
  -> Map.Map LabelText (AddressInfo address)
  -> Seq.Seq (Atom (op (Reference LabelText)))
  -> Either AssemblyError (Seq.Seq (Atom (op (Reference address))))
solveReferences c labelDictionary s
  = asrsAtoms <$> foldM (solveAtomReference c labelDictionary) initialState s
  where
    initialState = StateReferenceSolve
      { asrsAtoms = Seq.empty
      , asrsRelativeVAOffset = 0
      }

solveAtomReference
  :: (Traversable op, Address address, ByteSized (op (Reference LabelText)))
  => Config address
  -> Map.Map LabelText (AddressInfo address)
  -> StateReferenceSolve op address
  -> Atom (op (Reference LabelText))
  -> Either AssemblyError (StateReferenceSolve op address)
solveAtomReference Config {..} labelDictionary s@StateReferenceSolve {..} = go -- (AOp op1)
  where
    go (AOp op1) = do
      let width = operationWidth op1
      op2 <- AOp <$> Prelude.mapM (solveReference asrsRelativeVAOffset) op1
      newRVA <- width `safePlus` asrsRelativeVAOffset
      pure s {
        asrsAtoms = asrsAtoms Seq.|> op2
      , asrsRelativeVAOffset = newRVA
      }
    go (ALabel _) = pure s -- self-solve labels? no need, discard them...

    -- minus = boundedMapEx ReferenceArithmetic B.minusBounded
    query labelText
      = Either.maybeToEither
          (ReferenceMissing labelText)
          (Map.lookup labelText labelDictionary)

    -- solveReference
    --  :: add -> Reference LabelText -> Either AssemblyError (Reference add)
    solveReference _ (RefVA labelText) = do
      rva <- aiRelativeVA <$> query labelText
      RefVA <$> rva `safePlus` acVirtualBaseAddress
    solveReference _ (RefRelativeVA labelText) =
      RefRelativeVA . aiRelativeVA <$> query labelText
    solveReference _ (RefIA labelText) =
      RefIA . aiIA <$> query labelText
    solveReference currentRVA (RefForwardOffsetVA labelText) = do
      targetRVA <- aiRelativeVA <$> query labelText
      pure $ RefForwardOffsetVASolved currentRVA targetRVA
    solveReference _ (RefLabelDifferenceIA {..}) = do
      aFrom <- aiIA <$> query difiaFrom
      aTo   <- aiIA <$> query difiaTo
      when (aFrom > aTo) $ Left FromLabelAfterTo
      pure $ RefLabelDifferenceIA aFrom aTo
    solveReference _ (RefForwardOffsetVASolved {}) =
      Left $ InternalError "Received already solved VA to solve"

assemble
  ::
  ( Address address
  , Traversable op
  , ByteSized (op (Reference LabelText))
  , ToWord8s (op (Reference address))
  )
  => Config address
  -> Seq.Seq (Atom (op (Reference LabelText)))
  -> Either AssemblyError BS.ByteString
assemble cfg input = do
  labelMap <- scanLabels input
  solvedReferences <- solveReferences cfg labelMap input
  inlined <- toWord8s solvedReferences
  pure $ toByteString inlined
  where
    toByteString = BS.pack . F.toList
