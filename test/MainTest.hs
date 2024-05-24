{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module MainTest where

import Test.Hspec

import Common

import ASM
import ASM.Types

import qualified Data.Sequence as Seq
import qualified Data.Binary.Put as Bin
import qualified Data.Word as Word
-- import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BS

-- | Opcode defined for testing purposes
data TestOpcode address
  = JumpTo address
  | Noop
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

data BSByteShow = BSByteShow BS.ByteString deriving (Eq)

instance Show BSByteShow where
  show (BSByteShow bs) = show (BS.unpack bs)

-- | Warning, this should match the Encodable lengths...
-- ByteSized and Encodable are separate to reflect that label references are of
-- known size, but their encoding is not obtainable until resolved
instance ByteSized (TestOpcode a) where
  sizeof (JumpTo _)       = 1 + 4
  sizeof Noop             = 2

instance Address Word32

instance Encodable Word32 where
  encode = pure . Seq.fromList . BS.unpack . Bin.runPut . Bin.putWord32le . fromIntegral

instance Encodable (TestOpcode (Reference Word32)) where
  encode (JumpTo ref)
    = do
      addr <- encode ref
      pure $ Seq.singleton 0x01 <> addr
  encode Noop
    = pure $ Seq.fromList [0x03, 0x03]
  -- encode x = Left
  --   $ ReferenceTypeNotSupportedInOpcode $
  --     "Invalid combination of opcodes and references: " <> Text.pack (show x)

emptyAtoms :: Seq.Seq (Atom (TestOpcode (Reference LabelText)))
emptyAtoms = Seq.fromList []

testSeq1 :: Seq.Seq (Atom (TestOpcode (Reference LabelText)))
testSeq1 = Seq.fromList [AOp (JumpTo (RefVA "test"))]

testSeq2 :: Seq.Seq (Atom (TestOpcode (Reference LabelText)))
testSeq2 = Seq.fromList [ALabel "TEST"]

defaultConfig :: Config Word.Word32
defaultConfig = Config {..}
  where
    acVirtualBaseAddress = 0x100

main :: IO ()
main = hspec $
  do
    it "Empty list assembly returns no bytes" $
      assembleAtoms []
        `shouldBe` Right ""
    it "Label should not generate any bytes" $
      assembleAtoms [ALabel "l"]
        `shouldBe` Right ""
    it "Undefined reference returns an error" $
      assembleAtoms [AOp (JumpTo (RefVA "missing"))]
        `shouldBe` Left (ReferenceMissing "missing")
    it "Address encoding is 32 bit Little Endian" $
      encode (0x100 :: Word32)
        `shouldBe` Right (Seq.fromList [0x00, 0x01, 0x00, 0x00])
    it "A RefVA reference should return the virtual address 0x100 for top" $
      assembleAtoms [ALabel "top", AOp (JumpTo (RefVA "top"))]
        `shouldBeBS` Right (BS.pack [0x01, 0x00, 0x01, 0x00, 0x00])
    it "A RefRelativeVA reference should return 0 for top" $
      assembleAtoms [ALabel "top", AOp (JumpTo (RefRelativeVA "top"))]
        `shouldBeBS` Right (BS.pack [0x01, 0x00, 0x00, 0x00, 0x00])
    it "A RefIA reference should return 0 for top" $
      assembleAtoms [ALabel "top", AOp (JumpTo (RefIA "top"))]
        `shouldBeBS` Right (BS.pack [0x01, 0x00, 0x00, 0x00, 0x00])

    it "A RefForwardOffsetVA should return 0 for top" $
      assembleAtoms [ALabel "top", AOp (JumpTo (RefForwardOffsetVA "top"))]
        `shouldBeBS` Right (BS.pack [0x01, 0x00, 0x00, 0x00, 0x00])

    it "A RefForwardOffsetVA should return 2 for top" $
      assembleAtoms
        [ ALabel "top"
        , AOp Noop
        , AOp (JumpTo (RefForwardOffsetVA "top"))
        ]
        `shouldBeBS` Right (BS.pack [0x03, 0x03, 0x01, 0x02, 0x00, 0x00, 0x00])

    it "A RefForwardOffsetVA should return 7 to bottom" $
      assembleAtoms
        [ AOp (JumpTo (RefForwardOffsetVA "bottom"))
        , AOp Noop
        , ALabel "bottom"
        ]
        `shouldBeBS` Right
          (BS.pack [0x01, 0x07, 0x00, 0x00, 0x00, 0x03, 0x03])
                -- ^ reference from                      -- ^ reference to

    it "A RefForwardOffsetVA should return 7 to bottom" $
      assembleAtoms
        [ AOp Noop
        , AOp (JumpTo (RefForwardOffsetVA "bottom"))
        , AOp Noop
        , ALabel "bottom"
        ]
        `shouldBeBS` Right
          (BS.pack [0x03, 0x03, 0x01, 0x07, 0x00, 0x00, 0x00, 0x03, 0x03])
                            -- ^ reference from                      -- ^ reference to

    -- RefIA address
  -- | -- | Offset to another label's Virtual Address from the Virtual Address of
    -- -- the referrer begin location. A signed value. Used for relative jumps. If positive
    -- -- then the target is below. If negative then the target is above. For x64 note that
    -- -- you are interested in the offset relative to the referrer END location, so you will
    -- -- have to add the width of the reference.
    -- RefForwardOffsetVA address
  -- | RefForwardOffsetVASolved
    --   { refCurrentVA :: address -- From just after the reference
    --   , refTargetVA  :: address
    --   }
  -- | -- | The unsigned offset from the first label Image Address to the
    -- -- second one: the first label must be <= than the second one and
    -- -- the delta must fit the given Size (this is to be error-checked
    -- -- at run time). Helps to define executable file values.
    -- -- TODO: Couldn't we convert this to the delta between the current pos and a label?
    -- RefLabelDifferenceIA
    --   { difiaFrom :: address
    --   , difiaTo   :: address
    --   }

  where
    -- Wraps the bytestring to produce different show output
    shouldBeBS
      :: HasCallStack
      => Either AssemblyError BS.ByteString
      -> Either AssemblyError BS.ByteString
      -> IO ()
    shouldBeBS (Right got) (Right expected) = BSByteShow got `shouldBe` BSByteShow expected
    shouldBeBS got expected = shouldBe got expected
    assembleAtoms
      :: [Atom (TestOpcode (Reference LabelText))]
      -> Either AssemblyError BS.ByteString
    assembleAtoms = assemble defaultConfig . Seq.fromList
