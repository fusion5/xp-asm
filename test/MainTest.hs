{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module MainTest where

import Test.Hspec

import Common

import ASM
import ASM.Types
import Data.Int

import qualified Data.Sequence as Seq
import qualified Data.Binary.Put as Bin
import qualified Data.ByteString.Lazy as BS

-- | Opcode defined for testing purposes
data TestOpcode address
  = JumpAbsolute address
  | JumpRelative address
  | Noop
  | IAOffset16
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

newtype BSByteShow = BSByteShow BS.ByteString deriving (Eq)

instance Show BSByteShow where
  show (BSByteShow bs) = show (BS.unpack bs)

-- | Warning, this should match the Encodable lengths...
-- ByteSized and Encodable are separate to reflect that label references are of
-- known size, but their encoding is not obtainable until resolved
instance ByteSized TestOpcode where
  sizeRVA (JumpAbsolute _) = 1 + 4
  sizeRVA (JumpRelative _) = 1 + 1
  sizeRVA Noop       = 2
  sizeRVA IAOffset16 = 0

  sizeIA (JumpAbsolute _)  = 1 + 4
  sizeIA (JumpRelative _)  = 1 + 1
  sizeIA Noop        = 2
  sizeIA IAOffset16  = 16

instance Address Word32

instance Encodable Word32 where
  encode _ = pure . Seq.fromList . BS.unpack . Bin.runPut . Bin.putWord32le . fromIntegral

instance Encodable Int8 where
  encode _ = pure . Seq.fromList . BS.unpack . Bin.runPut . Bin.putWord8 . fromIntegral

-- Example of encoding an absolute reference to an address
encodeAbsolute
  :: (Address address)
  => AddressInfo address
  -> SolvedReference Word32
  -> Either AssemblyError (Seq.Seq Word8)
encodeAbsolute addressInfo (SolvedIA addr)         = encode addressInfo addr
encodeAbsolute addressInfo (SolvedRelativeVA addr) = encode addressInfo addr
encodeAbsolute addressInfo (SolvedVA addr)         = encode addressInfo addr

-- Example of encoding a relative reference to an address.
-- If the offset exceeds 1 byte signed integer then error out with overflow.
encodeRelative
  :: (Address address)
  => AddressInfo address
  -> SolvedReference Word32
  -> Either AssemblyError (Seq.Seq Word8)
encodeRelative ai@AddressInfo {..} solvedReference
    = go solvedReference >>= encodeInt8
  where
    go (SolvedIA targetAddr)
      = safeDowncast (fromIntegral targetAddr - fromIntegral aiIA)
    go (SolvedRelativeVA targetAddr)
      = safeDowncast (fromIntegral targetAddr - fromIntegral aiRelativeVA)
    go (SolvedVA targetAddr)
      = safeDowncast (fromIntegral targetAddr - fromIntegral aiRelativeVA)
    encodeInt8 :: Int8 -> Either AssemblyError (Seq.Seq Word8)
    encodeInt8 = encode ai

instance Encodable (TestOpcode (SolvedReference Word32)) where
  encode addressInfo (JumpAbsolute ref)
    = do
      addr <- encodeAbsolute addressInfo ref
      pure $ Seq.singleton 0x01 <> addr
  encode addressInfo (JumpRelative ref)
    = do
      addr <- encodeRelative addressInfo ref
      pure $ Seq.singleton 0x02 <> addr
  encode _addressInfo Noop
    = pure $ Seq.fromList [0x03, 0x03]
  encode _addressInfo IAOffset16
    = pure Seq.empty

defaultConfig :: Config Word32
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
      assembleAtoms [AOp (JumpAbsolute (RefVA "missing"))]
        `shouldBe` Left (ReferenceMissing "missing")

    it "Address encoding is 32 bit Little Endian" $
      encode (AddressInfo (0 :: Word32) (0 :: Word32) (0 :: Word32)) (0x100 :: Word32)
        `shouldBe` Right (Seq.fromList [0x00, 0x01, 0x00, 0x00])

    it "A RefVA reference should return the virtual address 0x100 for top" $
      assembleAtoms
        [ ALabel "top"
        , AOp (JumpAbsolute (RefVA "top"))
        ]
        `shouldBeBS` Right (BS.pack [0x01, 0x00, 0x01, 0x00, 0x00])

    it "A RefVA reference should return the virtual address 0x102 in the middle" $
      assembleAtoms
        [ AOp Noop
        , ALabel "top"
        , AOp (JumpAbsolute (RefVA "top"))
        ]
        `shouldBeBS` Right (BS.pack [0x03, 0x03, 0x01, 0x02, 0x01, 0x00, 0x00])

    it "A RefVA reference should return the virtual address 0x107 at the end" $
      assembleAtoms
        [ AOp (JumpAbsolute (RefVA "bottom"))
        , AOp Noop
        , ALabel "bottom"
        ]
        `shouldBeBS` Right (BS.pack [0x01, 0x07, 0x01, 0x00, 0x00, 0x03, 0x03])

    it "A RefRelativeVA reference should return 0 for top" $
      assembleAtoms
        [ ALabel "top"
        , AOp (JumpAbsolute (RefRelativeVA "top"))
        ]
        `shouldBeBS` Right (BS.pack [0x01, 0x00, 0x00, 0x00, 0x00])

    it "A RefRelativeVA reference should return 5 for bottom" $
      assembleAtoms
        [ AOp (JumpAbsolute (RefRelativeVA "bottom"))
        , ALabel "bottom"
        ]
        `shouldBeBS` Right (BS.pack [0x01, 0x05, 0x00, 0x00, 0x00])

    it "A RefIA reference should return 0 for top" $
      assembleAtoms
        [ ALabel "top"
        , AOp (JumpAbsolute (RefIA "top"))
        ]
        `shouldBeBS` Right (BS.pack [0x01, 0x00, 0x00, 0x00, 0x00])

    it "A RefIA reference should return 5 for bottom" $
      assembleAtoms
        [ AOp (JumpAbsolute (RefIA "bottom"))
        , ALabel "bottom"
        ]
        `shouldBeBS` Right (BS.pack [0x01, 0x05, 0x00, 0x00, 0x00])

    it "A RefIA reference should be affected by an Image Address offset" $
      assembleAtoms
        [ AOp IAOffset16
        , ALabel "top"
        , AOp (JumpAbsolute (RefIA "top"))
        ]
        `shouldBeBS` Right (BS.pack [0x01, 0x10, 0x00, 0x00, 0x00])

    it "A RefRVA reference should not be affected by an Image Address offset" $
      assembleAtoms
        [ AOp IAOffset16
        , ALabel "top"
        , AOp (JumpAbsolute (RefRelativeVA "top"))
        ]
        `shouldBeBS` Right (BS.pack [0x01, 0x00, 0x00, 0x00, 0x00])

    it "A RefVA reference should not be affected by an Image Address offset" $
      assembleAtoms
        [ AOp IAOffset16
        , ALabel "top"
        , AOp (JumpAbsolute (RefVA "top"))
        ]
        `shouldBeBS` Right (BS.pack [0x01, 0x00, 0x01, 0x00, 0x00])

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
