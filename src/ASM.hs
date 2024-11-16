module ASM
( assemble
, Config (..)
) where

import Common

import ASM.Types

import Data.Sequence
import Data.Int

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Either.Extra as Either
import qualified Data.Binary.Put as Bin

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

instance Address Word8
instance Address Word32
instance Address Int8

addOffsets
  :: Config -> PositionInfo -> Natural -> Either AssemblyError PositionInfo
addOffsets Config {..} a@PositionInfo {..} n
  = do
    basePosition <- integralToPosition acVirtualBaseAddress
    opSize       <- integralToPosition n
    pure $ a
      { piIA         = piIA `add` opSize
      , piRelativeVA = piRelativeVA `add` opSize
      , piVA         = piRelativeVA `add` opSize `add` basePosition
      }

-- atomize
--   :: Encodable op
--   => Config address
--   -> op --  (Reference LabelText)
--   -> Either AssemblyError (Seq Atom)
-- atomize = undefined

-- | Extract all labels in the sequence in a Map. The key is the label and the
-- value is positional information (PositionInfo)
scanLabels
  :: Config
  -> Seq Atom
  -> Either AssemblyError (Map.Map LabelText PositionInfo)
scanLabels c@Config{..} atoms = do
  basePosition <- integralToPosition acVirtualBaseAddress
  aslsLabels <$> foldM scan (initialState basePosition) atoms
  where
    initialState basePosition = StateLabelScan
      (PositionInfo zero zero basePosition) Map.empty

    scan s@StateLabelScan {asPosition = p@PositionInfo {..}, ..} = \case
      ALabel label -> do
        newLabels <- insertLabel label p aslsLabels
        pure s { aslsLabels = newLabels }
      AAlignIA n -> do
        newIA  <- fst <$> alignHelper piIA n
        pure s { asPosition = p { piIA = newIA }}
      AAlignVA n -> do
        newVA  <- fst <$> alignHelper piVA n
        newRVA <- fst <$> alignHelper piRelativeVA n
        pure s { asPosition = p { piVA = newVA, piRelativeVA = newRVA }}
      AAddrW8 _ -> do
        newPosition <- addOffsets c p 1
        pure s { asPosition = newPosition }
      AAddrOffsetI8 _ -> do
        newPosition <- addOffsets c p 1
        pure s { asPosition = newPosition }
      AAddrW32 _ -> do
        newPosition <- addOffsets c p 4
        pure s { asPosition = newPosition }
      ABytes bs -> do
        newPosition <- addOffsets c p (fromIntegral $ BS.length bs)
        pure s { asPosition = newPosition }


    -- scan s@StateLabelScan{..} (ALabel label) = do
    --   newLabels <- insertLabel label asPosition aslsLabels
    --   pure s { aslsLabels = newLabels }
    -- scan s@StateLabelScan {asPosition = p@PositionInfo {..}} (AAlignIA n) = do
    --   newIA  <- fst <$> alignHelper piIA n
    --   pure s { asPosition = p { piIA = newIA }}
    -- scan s@StateLabelScan {asPosition = p@PositionInfo {..}} (AAlignVA n) = do
    --   newVA  <- fst <$> alignHelper piVA n
    --   newRVA <- fst <$> alignHelper piRelativeVA n
    --   pure s { asPosition = p { piVA = newVA, piRelativeVA = newRVA }}

-- | Obvious
alignHelper :: Position -> Natural -> Either AssemblyError (Position, Position)
alignHelper p n
  = do
    delta <- align n p
    pure (p `add` delta, delta)

-- | Solve label references to dictionary addresses.
{-
solveReferences
  :: Address address
  => Config address
  -> Map.Map LabelText PositionInfo
  -> Seq Atom
  -> Either AssemblyError (Seq Atom)
solveReferences c labelDictionary atoms
    = asrsAtoms <$>
        foldM (solveAtomReferences c labelDictionary) initialState atoms
  where
    initialState = StateReferenceSolve empty
-}

-- | Solve references possibly present in an Atom
{-
solveAtomReferences
  :: forall address
  .  Address address
  => Config address
  -> Map.Map LabelText PositionInfo
  -> StateReferenceSolve address
  -> Atom (Reference LabelText)
  -> Either AssemblyError (StateReferenceSolve address)
solveAtomReferences _ labelDictionary s@StateReferenceSolve {..} = \case
    AAlignIA n ->
      pure s
        { asrsAtoms = asrsAtoms |> AAlignIA n
        }
    AAlignVA n ->
      pure s
        { asrsAtoms = asrsAtoms |> AAlignVA n
        }
    ABytes xs ->
      pure s
        { asrsAtoms = asrsAtoms |> ABytes xs
        }
    ALabel lab ->
      pure s
        { asrsAtoms = asrsAtoms |> ALabel lab }
    AAddr ref -> do
      solvedAddr <- AAddr <$> solveReference ref
      pure s
        { asrsAtoms = asrsAtoms |> solvedAddr
        }
  where
    query labelText = Either.maybeToEither (ReferenceMissing labelText)
                        (Map.lookup labelText labelDictionary)

    addressOf labelText f = query labelText >>= positionDowncast . f

    solveReference
      :: Reference LabelText -> Either AssemblyError ({- Reference -} address)
    solveReference (RefVA labelText) =
      {- RefVA         <$> -} addressOf labelText piVA
    solveReference (RefRelativeVA labelText) =
      {- RefRelativeVA <$> -} addressOf labelText piRelativeVA
    solveReference (RefIA labelText) =
      {- RefIA         <$> -} addressOf labelText piIA
-}

encodeW32 :: Word32 -> Either AssemblyError BS.ByteString
encodeW32 = pure . Bin.runPut . Bin.putWord32le . fromIntegral

encodeW8 :: Word8 -> Either AssemblyError BS.ByteString
encodeW8 = pure . Bin.runPut . Bin.putWord8 . fromIntegral

encodeI8W8 :: Int8 -> Either AssemblyError BS.ByteString
encodeI8W8 = pure . Bin.runPut . Bin.putWord8 . fromIntegral

-- | Encode solved references to ByteString. Keeps track of current positions
encode
  :: Config
  -> Map.Map LabelText PositionInfo
  -> Seq Atom
  -> Either AssemblyError BS.ByteString
encode c@Config{..} labelMap atoms
    = do
      basePosition <- integralToPosition acVirtualBaseAddress
      sesEncoded <$> foldM encodeAtom (initialState basePosition) atoms
  where
    initialState basePosition = StateEncodeSolved
      (PositionInfo zero zero basePosition) ""

    query labelText = Either.maybeToEither (ReferenceMissing labelText)
                        (Map.lookup labelText labelMap)

    addressOf labelText f = query labelText >>= positionDowncast . f

    solveReference :: Address a => Reference -> Either AssemblyError a
    solveReference ref = addressOf labelText f
      where (labelText, f) = getter ref
    -- solveReference (RefVA labelText) =
    --   {- RefVA         <$> -} addressOf labelText piVA
    -- solveReference (RefRelativeVA labelText) =
    --   {- RefRelativeVA <$> -} addressOf labelText piRelativeVA
    -- solveReference (RefIA labelText) =
    --   {- RefIA         <$> -} addressOf labelText piIA

    emitBytes s@StateEncodeSolved{..} bytes = do
      advancePosition <- addOffsets c sesPosition (fromIntegral $ BS.length bytes)
      pure s
        { sesPosition = advancePosition -- advance the position with length bytes
        , sesEncoded = sesEncoded <> bytes
        }

    encodeAtom s@StateEncodeSolved {sesPosition = pos@PositionInfo {..}, ..} =
      \case
        ALabel _ -> pure s
        AAddrW8 ref -> solveReference ref >>= encodeW8 >>= emitBytes s
        AAddrW32 ref -> solveReference ref >>= encodeW32 >>= emitBytes s
        AAddrOffsetI8 ref -> do
          let (referenceLabel, positionGetter) = getter ref
          targetPosition <- positionGetter <$> query referenceLabel
          offset <- sub targetPosition (positionGetter pos)
          encodeI8W8 offset >>= emitBytes s
        ABytes bytes -> do
          advancePosition <- addOffsets c pos (fromIntegral $ BS.length bytes)
          pure s
            { sesPosition = advancePosition
            , sesEncoded = sesEncoded <> bytes
            }
        AAlignIA n -> do
          (newIA, delta) <- alignHelper piIA n
          replicateCount <- positionDowncast delta
          pure s
            { sesPosition = pos { piIA = newIA }
            , sesEncoded  = sesEncoded <> BS.replicate replicateCount 0x00
            }
        (AAlignVA n) -> do
          newVA  <- fst <$> alignHelper piVA n
          newRVA <- fst <$> alignHelper piRelativeVA n
          pure s
            { sesPosition = pos { piVA = newVA, piRelativeVA = newRVA }
            }

getter :: Reference -> (LabelText, PositionInfo -> Position)
getter (RefIA         target) = (target, piIA)
getter (RefRelativeVA target) = (target, piRelativeVA)
getter (RefVA         target) = (target, piVA)

assemble :: Encodable op => Config -> op -> Either AssemblyError BS.ByteString
assemble cfg input
  = do
    atoms    <- atomize input
    labelMap <- scanLabels cfg atoms
    -- solveReferences cfg labelMap atoms >>= encodeSolved cfg
    encode cfg labelMap atoms
