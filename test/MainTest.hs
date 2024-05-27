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
  encode _ = pure . Seq.fromList . BS.unpack . Bin.runPut . Bin.putWord32le . fromIntegral

instance Encodable (SolvedReference Word32) where
  encode addressInfo (SolvedIA addr)         = encode addressInfo addr
  encode addressInfo (SolvedRelativeVA addr) = encode addressInfo addr
  encode addressInfo (SolvedVA addr)         = encode addressInfo addr

instance Encodable (TestOpcode (SolvedReference Word32)) where
  encode addressInfo (JumpTo ref)
    = do
      addr <- encode addressInfo ref
      pure $ Seq.singleton 0x01 <> addr
  encode _addressInfo Noop
    = pure $ Seq.fromList [0x03, 0x03]

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
      encode (AddressInfo (0 :: Word32) (0 :: Word32)) (0x100 :: Word32)
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
      :: [Atom (TestOpcode Reference)]
      -> Either AssemblyError BS.ByteString
    assembleAtoms = assemble defaultConfig . Seq.fromList
