{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

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
-- type class 'Binary'. Values of this datatype in a sequence are elements that
-- can be converted to a binary format. The module facilitates outputting several types of
-- references to other 'op' elements in the sequence by means of the 'Reference' typ.

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
  forall op n address
  .
  ( Address address
  , KnownNat n
  )
  => Container (Atom (op (Reference LabelText))) n
  -> Either AssemblyError (Map.Map LabelText (AddressInfo address))
scanLabels s
    = aslsLabels <$> foldMContainer (FoldCallback scanAtom) initialState s
  where
    initialState :: StateLabelScan address 0
    initialState = StateLabelScan
      { aslsIAOffset = 0
      , aslsRelativeVAOffset = 0
      , aslsLabels = Map.empty
      }

-- TODO: Consider doing this without the external dependency (BoundedArithmetic)
safePlus :: (Address a) => a -> a -> Either AssemblyError a
safePlus = boundedBinopMapEx (Arithmetic . SEW) B.plusBounded

safeMinus :: (Address a) => a -> a -> Either AssemblyError a
safeMinus = boundedBinopMapEx (Arithmetic . SEW) B.plusBounded

safeDowncast :: (Integral a, Bounded a) => Integer -> Either AssemblyError a
safeDowncast = Either.mapLeft (Arithmetic . SEW) . B.fromIntegerBounded

scanAtom
  ::
  ( Address address
  , KnownNat n2
  )
  => StateLabelScan address n1
  -> Atom opcode n2
  -> Either AssemblyError (StateLabelScan address (n2 + n1))
scanAtom s@StateLabelScan {..} = go
  where
    go (Atom op) = do
        let width = natVal op -- Equals the KnownNat n2
        newIA  <- fromIntegral width `safePlus` aslsIAOffset
        newRVA <- fromIntegral width `safePlus` aslsRelativeVAOffset
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
  ::
  ( Address address
  , FunctorMSized op
  , KnownNat n
  )
  => Config address
  -> Map.Map LabelText (AddressInfo address)
  -> Container (Atom (op (Reference LabelText))) n
  -> Either AssemblyError (Container (Atom (op (Reference address))) n)
solveReferences c labelDictionary s
    = asrsAtoms <$> foldMContainer (FoldCallback $ solveAtomReference c labelDictionary) initialState s
  where
    initialState = StateReferenceSolve
      { asrsAtoms = Leaf
      , asrsRelativeVAOffset = 0
      }

solveAtomReference
  :: ( Address address
     , KnownNat n1
     , KnownNat n2
     , FunctorMSized op
     )
  => Config address
  -> Map.Map LabelText (AddressInfo address)
  -> StateReferenceSolve op address n1
  -> Atom (op (Reference LabelText)) n2
  -> Either AssemblyError (StateReferenceSolve op address (n1 + n2))
solveAtomReference Config {..} labelDictionary s@StateReferenceSolve {..}
  = go
  where
    go (Atom op) = do
      newOp  <- Atom <$> mapMSized (solveReference asrsRelativeVAOffset) op
      let width = natVal op
      newRVA <- fromIntegral width `safePlus` asrsRelativeVAOffset
      pure s
        { asrsAtoms = Tree newOp asrsAtoms Leaf
        , asrsRelativeVAOffset = newRVA
        }
    go _label@(Label _) = pure s

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
  , Binary (op (Reference address))
  , KnownNat n
  , FunctorMSized op
  )
  => Config address
  -> Container (Atom (op (Reference LabelText))) n
  -> Either AssemblyError BS.ByteString
assemble cfg input = do
  labelMap <- scanLabels input
  solvedReferences <- solveReferences cfg labelMap input
  inlined <- encode solvedReferences
  pure $ toByteString inlined
  where
    toByteString = BS.pack . Vec.toList
