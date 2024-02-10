{-# LANGUAGE FlexibleContexts #-}

module ASM
  ( assemble
  , Config (..)
  , safePlus
  , safeMinus
  , safeDowncast
  ) where

import ASM.Types
import Control.Monad

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
  , Sized (op Reference)
  , Ord address
  , Bounded address
  )
  => Seq.Seq (Atom op Reference)
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
operationWidth :: (Num address, Sized op) => op -> address
operationWidth = fromIntegral . sizeof

scanAtom
  :: (Sized (op Reference), Num address, Ord address, Bounded address)
  => StateLabelScan address
  -> Atom op Reference
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
    go (AData bytes)
      = do
        let len = fromIntegral (BS.length bytes)
        newIA  <- len `safePlus` aslsIAOffset
        newRVA <- len `safePlus` aslsRelativeVAOffset
        pure s {
          aslsIAOffset         = newIA
        , aslsRelativeVAOffset = newRVA
        }

-- Solve label references to dictionary addresses. Again we keep track of the offset we are at
-- like in scanLabels. Perhaps this duplicate operation could be factored out, but it shouldn't
-- be too expensive...
solveReferences
  :: (Traversable op, Bounded address, Ord address, Num address, Sized (op Reference))
  => Config address
  -> Map.Map LabelText (AddressInfo address)
  -> Seq.Seq (Atom op Reference)
  -> Either AssemblyError (Seq.Seq (Atom op (SolvedReference address)))
solveReferences c labelDictionary s
  = asrsAtoms <$> foldM (solveAtomReference c labelDictionary) initialState s
  where
    initialState = StateReferenceSolve
      { asrsAtoms = Seq.empty
      , asrsRelativeVAOffset = 0
      }

solveAtomReference
  :: (Traversable op, Num address, Ord address, Bounded address, Sized (op Reference))
  => Config address
  -> Map.Map LabelText (AddressInfo address)
  -> StateReferenceSolve op address
  -> Atom op Reference
  -> Either AssemblyError (StateReferenceSolve op address)
solveAtomReference Config {..} labelDictionary s@StateReferenceSolve {..} = go -- (AOp op1)
  where
    go (AOp op1) = do
      let width = operationWidth op1
      op2 <- AOp <$> mapM (solveReference asrsRelativeVAOffset) op1
      newRVA <- width `safePlus` asrsRelativeVAOffset
      pure s {
        asrsAtoms = asrsAtoms Seq.|> op2
      , asrsRelativeVAOffset = newRVA
      }
    go (ALabel _) = pure s -- self-solve labels? no need, discard them...
    go (AData bytes) = do
      newRVA <- fromIntegral (BS.length bytes) `safePlus` asrsRelativeVAOffset
      pure s {
        asrsRelativeVAOffset = newRVA
      }
    -- minus = boundedMapEx ReferenceArithmetic B.minusBounded
    query labelText
      = Either.maybeToEither
          (ReferenceMissing labelText)
          (Map.lookup labelText labelDictionary)

    -- solveReference
    --  :: address -> Reference -> Either AssemblyError (SolvedReference address)
    solveReference _ (RefVA labelText) = do
      rva <- aiRelativeVA <$> query labelText
      SolvedRefVA <$> rva `safePlus` acVirtualBaseAddress
    solveReference _ (RefRelativeVA labelText) =
      SolvedRefRelativeVA . aiRelativeVA <$> query labelText
    solveReference _ (RefIA labelText) =
      SolvedRefIA . aiIA <$> query labelText
    solveReference currentRVA (RefForwardOffsetVA labelText) = do
      targetRVA <- aiRelativeVA <$> query labelText
      pure $ SolvedRefForwardOffsetVA currentRVA targetRVA
    solveReference _ (RefLabelDifferenceIA {..}) = do
      from <- aiIA <$> query difiaFrom
      to   <- aiIA <$> query difiaTo
      when (from > to) $ Left FromLabelAfterTo
      pure $ SolvedRefLabelDifferenceIA from to

assemble
  ::
  ( Num address
  , Ord address
  , Bounded address
  , Traversable op
  , Sized (op Reference)
  , ToBS (op (SolvedReference address))
  )
  => Config address
  -> Seq.Seq (Atom op Reference)
  -> Either AssemblyError BS.ByteString
assemble cfg input = do
  labelMap <- scanLabels input
  solvedReferences <- solveReferences cfg labelMap input
  F.fold <$> mapM (Either.mapLeft OpcodeToByteString . asmToBin) solvedReferences
