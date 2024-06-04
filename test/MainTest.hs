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
  | IAOffset Natural
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
  sizeRVA Noop         = 2
  sizeRVA (IAOffset _) = 0

  sizeIA (JumpAbsolute _)  = 1 + 4
  sizeIA (JumpRelative _)  = 1 + 1
  sizeIA Noop         = 2
  sizeIA (IAOffset n) = fromIntegral n

instance Address Word32

-- Example of encoding of an address
-- Example of encoding a relative reference to an address.
-- If the offset exceeds 1 byte signed integer then error out with overflow.
encodeAbsoluteW32
  :: Address addr
  => SolvedReference addr
  -> Either AssemblyError BS.ByteString
encodeAbsoluteW32 (SolvedIA a)         = safeDowncast (fromIntegral a) >>= encodeW32
encodeAbsoluteW32 (SolvedRelativeVA a) = safeDowncast (fromIntegral a) >>= encodeW32
encodeAbsoluteW32 (SolvedVA a)         = safeDowncast (fromIntegral a) >>= encodeW32

encodeRelativeW8
  :: Address addr
  => PositionInfo addr
  -> SolvedReference addr
  -> Either AssemblyError BS.ByteString
encodeRelativeW8 PositionInfo {..} solvedReference
  = downcastEncode $ delta solvedReference
  where
    delta (SolvedIA targetAddr)
      = fromIntegral targetAddr - fromIntegral piIA
    delta (SolvedRelativeVA targetAddr)
      = fromIntegral targetAddr - fromIntegral piRelativeVA
    delta (SolvedVA targetAddr)
      = fromIntegral targetAddr - fromIntegral piVA
    downcastEncode i = safeDowncast i >>= encodeI8W8

encodeW32 :: Word32 -> Either AssemblyError BS.ByteString
encodeW32 = pure . Bin.runPut . Bin.putWord32le . fromIntegral

encodeI8W8 :: Int8 -> Either AssemblyError BS.ByteString
encodeI8W8 = pure . Bin.runPut . Bin.putWord8 . fromIntegral

instance Encodable TestOpcode where
  encode _ (JumpAbsolute ref)
    = do
      addr <- encodeAbsoluteW32 ref
      pure $ BS.pack [0x01] <> addr
  encode pos (JumpRelative ref)
    = do
      addr <- encodeRelativeW8 pos ref
      pure $ BS.pack [0x02] <> addr
  encode _ Noop
    = pure $ BS.pack [0x03, 0x03]
  encode _ (IAOffset _)
    = pure ""

defaultConfig :: Config Word32
defaultConfig = Config {..}
  where
    acVirtualBaseAddress = 0x100

main :: IO ()
main = hspec $
  do
    it "Empty list assembly returns no bytes" $
      assembleAtoms [] `shouldBeBytes` []

    it "Label should not generate any bytes" $
      assembleAtoms [ALabel "l"] `shouldBeBytes` []

    it "Undefined reference returns an error" $
      shouldBeError $
        assembleAtoms [AOp (JumpAbsolute (RefVA "missing"))]

    it "Address encoding is 32 bit Little Endian" $
      encodeW32 0x100
        `shouldBe` Right (BS.pack [0x00, 0x01, 0x00, 0x00])

    it "A RefVA reference should return the virtual address 0x100 for top" $
      assembleAtoms
        [ ALabel "top"
        , AOp (JumpAbsolute (RefVA "top"))
        ]
        `shouldBeBytes` [0x01, 0x00, 0x01, 0x00, 0x00]

    it "A RefVA reference should return the virtual address 0x102 in the middle" $
      assembleAtoms
        [ AOp Noop
        , ALabel "top"
        , AOp (JumpAbsolute (RefVA "top"))
        ]
        `shouldBeBytes` [0x03, 0x03, 0x01, 0x02, 0x01, 0x00, 0x00]

    it "A RefVA reference should return the virtual address 0x107 at the end" $
      assembleAtoms
        [ AOp (JumpAbsolute (RefVA "bottom"))
        , AOp Noop
        , ALabel "bottom"
        ]
        `shouldBeBytes` [0x01, 0x07, 0x01, 0x00, 0x00, 0x03, 0x03]

    it "A RefRelativeVA reference should return 0 for top" $
      assembleAtoms
        [ ALabel "top"
        , AOp (JumpAbsolute (RefRelativeVA "top"))
        ]
        `shouldBeBytes` [0x01, 0x00, 0x00, 0x00, 0x00]

    it "A RefRelativeVA reference should return 5 for bottom" $
      assembleAtoms
        [ AOp (JumpAbsolute (RefRelativeVA "bottom"))
        , ALabel "bottom"
        ]
        `shouldBeBytes` [0x01, 0x05, 0x00, 0x00, 0x00]

    it "A RefIA reference should return 0 for top" $
      assembleAtoms
        [ ALabel "top"
        , AOp (JumpAbsolute (RefIA "top"))
        ]
        `shouldBeBytes` [0x01, 0x00, 0x00, 0x00, 0x00]

    it "A RefIA reference should return 5 for bottom" $
      assembleAtoms
        [ AOp (JumpAbsolute (RefIA "bottom"))
        , ALabel "bottom"
        ]
        `shouldBeBytes` [0x01, 0x05, 0x00, 0x00, 0x00]

    it "A RefIA reference should be affected by an Image Address offset" $
      assembleAtoms
        [ AOp (IAOffset 16)
        , ALabel "top"
        , AOp (JumpAbsolute (RefIA "top"))
        ]
        `shouldBeBytes` [0x01, 0x10, 0x00, 0x00, 0x00]

    it "A RefRVA reference should not be affected by an Image Address offset" $
      assembleAtoms
        [ AOp (IAOffset 16)
        , ALabel "top"
        , AOp (JumpAbsolute (RefRelativeVA "top"))
        ]
        `shouldBeBytes` [0x01, 0x00, 0x00, 0x00, 0x00]

    it "A RefVA reference should not be affected by an Image Address offset" $
      assembleAtoms
        [ AOp (IAOffset 16)
        , ALabel "top"
        , AOp (JumpAbsolute (RefVA "top"))
        ]
        `shouldBeBytes` [0x01, 0x00, 0x01, 0x00, 0x00]


    it "Relative backwards image reference minimum (-128)" $
      assembleAtoms
        [ ALabel "top"
        , AOp (IAOffset 128)
        , AOp (JumpRelative (RefIA "top"))
        ]
        `shouldBeBytes` [0x02, 0x80]

    it "Relative backwards image reference underflow (-129)" $
      shouldBeError $
        assembleAtoms
          [ ALabel "top"
          , AOp (IAOffset 129)
          , AOp (JumpRelative (RefIA "top"))
          ]

    it "Relative backwards image reference -1" $
      assembleAtoms
        [ ALabel "top"
        , AOp (IAOffset 1)
        , AOp (JumpRelative (RefIA "top"))
        ]
        `shouldBeBytes` [0x02, 0xFF]

    it "Relative image reference offset 0" $
      assembleAtoms
        [ ALabel "top"
        , AOp (JumpRelative (RefIA "top"))
        ]
        `shouldBeBytes` [0x02, 0x00]

    it "Relative image reference offset 2" $
      assembleAtoms
        [ AOp (JumpRelative (RefIA "bottom"))
        , ALabel "bottom"
        ]
        `shouldBeBytes` [0x02, 0x02]

    it "Relative image reference maximum (127)" $
      assembleAtoms
        [ AOp (JumpRelative (RefIA "bottom"))
        , AOp (IAOffset 125)
        , ALabel "bottom"
        ]
        `shouldBeBytes` [0x02, 127]

    it "Relative image reference overflow (128)" $
      shouldBeError $
        assembleAtoms
          [ AOp (JumpRelative (RefIA "bottom"))
          , AOp (IAOffset 126)
          , ALabel "bottom"
          ]

  where
    -- Wraps the bytestring to produce different show output
    shouldBeBytes
      :: HasCallStack
      => Either AssemblyError BS.ByteString
      -> [Word8]
      -> IO ()
    shouldBeBytes (Right got) expected
      = BSByteShow got `shouldBe` BSByteShow (BS.pack expected)
    shouldBeBytes got expected
      = shouldBe got (Right (BS.pack expected))

    shouldBeError
      :: HasCallStack
      => Either AssemblyError BS.ByteString
      -> IO ()
    shouldBeError (Right got) =
      expectationFailure $
        "Expecting error, got " <> show (BSByteShow got)
    shouldBeError _ = pure ()

    assembleAtoms
      :: [Atom (TestOpcode Reference)]
      -> Either AssemblyError BS.ByteString
    assembleAtoms = assemble defaultConfig . Seq.fromList
