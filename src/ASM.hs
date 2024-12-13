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

-- import Debug.Trace

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
  :: Config -> Positions -> Natural -> Either AssemblyError Positions
addOffsets Config{..} a@Positions{..} n
  = do
    basePosition <- integralToPosition acVirtualBaseAddress
    opSize       <- integralToPosition n
    pure $ a
      { piIA         = piIA `add` opSize
      , piRelativeVA = piRelativeVA `add` opSize
      , piVA         = piRelativeVA `add` opSize `add` basePosition
      }

-- | Extract all labels in the sequence in a Map. The key is the label and the
-- value is positional information (Positions)
scanLabels
  :: Config
  -> Seq Atom
  -> Either AssemblyError (Map.Map LabelText Positions)
scanLabels c@Config{..} atoms = do
  basePosition <- integralToPosition acVirtualBaseAddress
  aslsLabels <$> foldM scan (initialState basePosition) atoms
  where
    initialState basePosition = StateLabelScan
      (Positions zero zero basePosition) Map.empty

    scan s@StateLabelScan {asPosition = p@Positions{..}, ..} = \case
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
      AExprW8 _ -> do
        newPosition <- addOffsets c p 1
        pure s { asPosition = newPosition }
      AExprI8 _ -> do
        newPosition <- addOffsets c p 1
        pure s { asPosition = newPosition }
      AExprW32 _ -> do
        newPosition <- addOffsets c p 4
        pure s { asPosition = newPosition }
      ABytes bs -> do
        newPosition <- addOffsets c p (fromIntegral $ BS.length bs)
        pure s { asPosition = newPosition }

-- | Obvious
alignHelper :: Position -> Natural -> Either AssemblyError (Position, Position)
alignHelper p n
  = do
    delta <- align n p
    pure (p `add` delta, delta)

encodeW32 :: Word32 -> Either AssemblyError BS.ByteString
encodeW32 = pure . Bin.runPut . Bin.putWord32le . fromIntegral

encodeW8 :: Word8 -> Either AssemblyError BS.ByteString
encodeW8 = pure . Bin.runPut . Bin.putWord8 . fromIntegral

encodeI8W8 :: Int8 -> Either AssemblyError BS.ByteString
encodeI8W8 = pure . Bin.runPut . Bin.putWord8 . fromIntegral

-- | Encode solved references to ByteString. Keeps track of current positions
encode
  :: Config
  -> Map.Map LabelText Positions
  -> Seq Atom
  -> Either AssemblyError BS.ByteString
encode c@Config{..} labelMap atoms
    = do
      basePosition <- integralToPosition acVirtualBaseAddress
      sesEncoded <$> foldM encodeAtom (initialState basePosition) atoms
  where
    initialState basePosition = StateEncode
      (Positions zero zero basePosition) ""

    query labelText = Either.maybeToEither (ReferenceMissing labelText)
                        (Map.lookup labelText labelMap)

    addressOf labelText f = f <$> query labelText -- >>= {- positionDowncast . -} f

    queryReference :: Reference -> Either AssemblyError Position
    queryReference ref = addressOf labelText f
      where
        f         = getter ref
        labelText = getLabel ref

    emitBytes s@StateEncode{..} bytes = do
      advancePosition <- addOffsets c sesPosition (fromIntegral $ BS.length bytes)
      pure s
        { sesPosition = advancePosition -- advance the position with length bytes
        , sesEncoded = sesEncoded <> bytes
        }

    solveExpr :: Positions -> ExprReference -> Either AssemblyError Integer
    solveExpr _ (ExprRef ref) = positionToInteger <$> queryReference ref
    solveExpr p (ExprDiff er1 er2)
      = do
          v1 <- solveExpr p er1
          v2 <- solveExpr p er2
          pure $ v1 - v2
    solveExpr p (ExprSum er1 er2)
      = do
          v1 <- solveExpr p er1
          v2 <- solveExpr p er2
          pure $ v1 + v2
    solveExpr positions ExprCurrentIA         = pure $ positionToInteger $ piIA positions
    solveExpr positions ExprCurrentRelativeVA = pure $ positionToInteger $ piRelativeVA positions
    solveExpr positions ExprCurrentVA         = pure $ positionToInteger $ piVA positions

    encodeAtom s@StateEncode{sesPosition = pos@Positions{..}, ..} =
      \case
        ALabel   _    -> pure s
        AExprW8  expr -> solveExpr pos expr >>= downcast >>= encodeW8   >>= emitBytes s
        AExprW32 expr -> solveExpr pos expr >>= downcast >>= encodeW32  >>= emitBytes s
        AExprI8  expr -> solveExpr pos expr >>= downcast >>= encodeI8W8 >>= emitBytes s
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

getLabel :: Reference -> LabelText
getLabel (RefIA         label) = label
getLabel (RefRelativeVA label) = label
getLabel (RefVA         label) = label

getter :: Reference -> Positions -> Position
getter RefIA{}         = piIA
getter RefRelativeVA{} = piRelativeVA
getter RefVA{}         = piVA

-- solveReferenceToPosition
--   :: forall a . Address a
--   => Reference -> Positions -> Either AssemblyError a
-- solveReferenceToPosition (RefIA _) pos = positionDowncast $ piIA pos
-- solveReferenceToPosition (RefRelativeVA _) pos = positionDowncast $ piRelativeVA pos
-- solveReferenceToPosition (RefVA _) pos = positionDowncast $ piVA pos

-- getter :: Reference -> (LabelText, Positions -> Position)
-- getter (RefIA         target) = (target, piIA)
-- getter (RefRelativeVA target) = (target, piRelativeVA)
-- getter (RefVA         target) = (target, piVA)

assemble :: Encodable op => Config -> op -> Either AssemblyError BS.ByteString
assemble cfg input
  = do
    atoms    <- atomize input
    labelMap <- scanLabels cfg atoms
    -- solveReferences cfg labelMap atoms >>= encodeSolved cfg
    encode cfg labelMap atoms
