{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module ASM
  ( assemble
  , Config (..)
  , safePlus
  , safeMinus
  , safeDowncast
  ) where

import Common

import ASM.Types

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Numeric.Decimal.BoundedArithmetic as B
import qualified Data.Either.Extra as Either
import qualified Control.Exception as Exception
import qualified Data.Vector.Sized as Vec

-- | A simple assembler that produces one object, without imported/exported references.
-- There are two passes:
--  1. collect a Map from label to location information, (scanLabels)
--  2. replace labels with the required references by using the map from (1) (solveReferences)
--
-- The module is polymorphic on a data type typically denotated with 'op', which implements
-- type classes 'ToWord8s' and 'ByteSized'. Values of this datatype in a sequence are elements that
-- can be converted to a binary format. The module facilitates outputting several types of
-- references to other 'op' elements in the sequence by means of the 'Reference' type.

boundedBinopMapEx
  :: (Address a)
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
  ( Address address
  , ToWord8s (op (Reference LabelText))
  , KnownNat n
  )
  => Container (Atom (op (Reference LabelText))) n
  -> Either AssemblyError (Map.Map LabelText (AddressInfo address))
scanLabels s
    = aslsLabels <$> foldMNats (FoldCallback scanAtom) initialState s
  where
    -- initialState :: Address address => StateLabelScan address 0
    initialState = StateLabelScan
      { aslsIAOffset = 0
      , aslsRelativeVAOffset = 0
      , aslsLabels = Map.empty
      }

safePlus :: (Address a) => a -> a -> Either AssemblyError a
safePlus = boundedBinopMapEx (Arithmetic . SEW) B.plusBounded

safeMinus :: (Address a) => a -> a -> Either AssemblyError a
safeMinus = boundedBinopMapEx (Arithmetic . SEW) B.plusBounded

safeDowncast :: (Integral a, Bounded a) => Integer -> Either AssemblyError a
safeDowncast = Either.mapLeft (Arithmetic . SEW) . B.fromIntegerBounded

-- This might be expensive...
operationWidth :: (Address address, ToWord8s opcode, KnownNat n) => opcode n -> address
operationWidth = fromIntegral . Vec.length . safe

scanAtom
  :: (Address address, ToWord8s opcode, KnownNat n1, KnownNat n2)
  => StateLabelScan address n1
  -> Atom opcode n2
  -> Either AssemblyError (StateLabelScan address (n2 + n1))
scanAtom s@StateLabelScan {..} = go
  where
    go (Atom op) = do
        newIA  <- operationWidth op `safePlus` aslsIAOffset
        newRVA <- operationWidth op `safePlus` aslsRelativeVAOffset
        pure s {
          aslsIAOffset  = newIA
        , aslsRelativeVAOffset = newRVA
        }
    go (Label labelText) =
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
  :: (Address address, ToWord8s (op (Reference LabelText)))
  => Config address
  -> Map.Map LabelText (AddressInfo address)
  -> Container (Atom (op (Reference LabelText))) n
  -> Either AssemblyError (Container (Atom (op (Reference address))) n)
solveReferences c labelDictionary s
    = asrsAtoms <$> foldMNats (FoldCallback $ solveAtomReference c labelDictionary) initialState s
  where
    initialState = StateReferenceSolve
      { asrsAtoms = Nil
      , asrsRelativeVAOffset = 0
      }

solveAtomReference
  :: (Address address, ToWord8s (op (Reference LabelText)))
  => Config address
  -> Map.Map LabelText (AddressInfo address)
  -> StateReferenceSolve op address n1
  -> Atom (op (Reference LabelText)) n2
  -> Either AssemblyError (StateReferenceSolve op address (n2 + n1))
solveAtomReference Config {..} labelDictionary s@StateReferenceSolve {..}
  = go
  where
    go (Atom op) = do
      newOp <- Atom <$> mapMNats (MapMFunction $ solveReference asrsRelativeVAOffset) op
      newRVA <- operationWidth op `safePlus` asrsRelativeVAOffset
      pure s
        { asrsAtoms = newOp `Cons` asrsAtoms
        , asrsRelativeVAOffset = newRVA
        }
    go label@(Label _) =
      pure s
        { asrsAtoms = undefined -- label `Cons` asrsAtoms
        }

    query labelText
      = Either.maybeToEither
          (ReferenceMissing labelText)
          (Map.lookup labelText labelDictionary)

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
  , ToWord8s (op (Reference LabelText))
  , ToWord8s (op (Reference address))
  , KnownNat n
  )
  => Config address
  -> Container (Atom (op (Reference LabelText))) n
  -> Either AssemblyError BS.ByteString
assemble cfg input = do
  labelMap <- scanLabels input
  solvedReferences <- solveReferences cfg labelMap input
  let inlined = safe solvedReferences
  pure $ toByteString inlined
  where
    toByteString = BS.pack . Vec.toList
