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
  = JumpAbsoluteW32 address
  | JumpRelativeW8 address
  | Noop
  | IAOffset Natural
  | VAOffset Natural
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

newtype BSByteShow = BSByteShow BS.ByteString deriving (Eq)

instance Show BSByteShow where
  show (BSByteShow bs) = show (BS.unpack bs)

-- | Some tests use a Word8 address space, others a Word32 address space.
instance Address Word8

instance Address Word32

-- Example of encoding of an address
encodeAbsoluteW32
  :: Address addr
  => Reference addr
  -> Either AssemblyError BS.ByteString
encodeAbsoluteW32 (RefIA a)         = safeDowncast (fromIntegral a) >>= encodeW32
encodeAbsoluteW32 (RefRelativeVA a) = safeDowncast (fromIntegral a) >>= encodeW32
encodeAbsoluteW32 (RefVA a)         = safeDowncast (fromIntegral a) >>= encodeW32

-- Example of encoding a relative reference to an address.
-- If the offset exceeds 1 byte signed integer then error out with overflow.
encodeRelativeW8
  :: Address addr
  => PositionInfo
  -> Reference addr
  -> Either AssemblyError BS.ByteString
encodeRelativeW8 PositionInfo {..} solvedReference
  = downcastEncode $ delta solvedReference
  where
    delta (RefIA targetAddr)
      = fromIntegral targetAddr - fromIntegral piIA
    delta (RefRelativeVA targetAddr)
      = fromIntegral targetAddr - fromIntegral piRelativeVA
    delta (RefVA targetAddr)
      = fromIntegral targetAddr - fromIntegral piVA
    downcastEncode i = safeDowncast i >>= encodeI8W8

encodeW32 :: Word32 -> Either AssemblyError BS.ByteString
encodeW32 = pure . Bin.runPut . Bin.putWord32le . fromIntegral

encodeI8W8 :: Int8 -> Either AssemblyError BS.ByteString
encodeI8W8 = pure . Bin.runPut . Bin.putWord8 . fromIntegral

instance Encodable TestOpcode where
  encode _ (JumpAbsoluteW32 ref)
    = do
      addr <- encodeAbsoluteW32 ref
      pure $ BS.pack [0x01] <> addr
  encode pos (JumpRelativeW8 ref)
    = do
      addr <- encodeRelativeW8 pos ref
      pure $ BS.pack [0x02] <> addr
  encode _ Noop
    = pure $ BS.pack [0x03, 0x03]
  encode _ (IAOffset _)
    = pure ""
  encode _ (VAOffset _)
    = pure ""

  -- | Warning, this should match the Encodable lengths (to be tested)
  sizeRVA (JumpAbsoluteW32 _) = 1 + 4
  sizeRVA (JumpRelativeW8 _)  = 1 + 1
  sizeRVA Noop                = 2
  sizeRVA (IAOffset _)        = 0
  sizeRVA (VAOffset n)        = fromIntegral n

  sizeIA (JumpAbsoluteW32 _)  = 1 + 4
  sizeIA (JumpRelativeW8 _)   = 1 + 1
  sizeIA Noop                 = 2
  sizeIA (IAOffset n)         = fromIntegral n
  sizeIA (VAOffset _)         = 0

configW8 :: Config Word8
configW8 = Config {..} where acVirtualBaseAddress = 0x80

configW32 :: Config Word32
configW32 = Config {..} where acVirtualBaseAddress = 0x100

main :: IO ()
main = hspec $
  do
    it "Empty list assembly returns no bytes" $
      assembleAtomsW8 [] `shouldBeBytes` []

    it "Label should not generate any bytes" $
      assembleAtomsW8 [ALabel "l"] `shouldBeBytes` []

    it "Undefined reference returns an error" $
      shouldBeError $
        assembleAtomsW8 [AOp (JumpAbsoluteW32 (RefVA "missing"))]

    it "Address encoding is 32 bit Little Endian" $
      encodeW32 0x100
        `shouldBe` Right (BS.pack [0x00, 0x01, 0x00, 0x00])

    it "A RefVA reference should return the virtual address 0x80 for top" $
      assembleAtomsW8
        [ ALabel "top"
        , AOp (JumpAbsoluteW32 (RefVA "top"))
        ]
        `shouldBeBytes` [0x01, 0x80, 0x00, 0x00, 0x00]

    it "A RefVA reference should return the virtual address 0x82 in the middle" $
      assembleAtomsW8
        [ AOp Noop
        , ALabel "top"
        , AOp (JumpAbsoluteW32 (RefVA "top"))
        ]
        `shouldBeBytes` [0x03, 0x03, 0x01, 0x82, 0x00, 0x00, 0x00]

    it "A RefVA reference should return the virtual address 0x87 at the end" $
      assembleAtomsW8
        [ AOp (JumpAbsoluteW32 (RefVA "bottom"))
        , AOp Noop
        , ALabel "bottom"
        ]
        `shouldBeBytes` [0x01, 0x87, 0x00, 0x00, 0x00, 0x03, 0x03]

    it "A RefRelativeVA reference should return 0 for top" $
      assembleAtomsW8
        [ ALabel "top"
        , AOp (JumpAbsoluteW32 (RefRelativeVA "top"))
        ]
        `shouldBeBytes` [0x01, 0x00, 0x00, 0x00, 0x00]

    it "A RefRelativeVA reference should return 5 for bottom" $
      assembleAtomsW8
        [ AOp (JumpAbsoluteW32 (RefRelativeVA "bottom"))
        , ALabel "bottom"
        ]
        `shouldBeBytes` [0x01, 0x05, 0x00, 0x00, 0x00]

    it "A RefIA reference should return 0 for top" $
      assembleAtomsW8
        [ ALabel "top"
        , AOp (JumpAbsoluteW32 (RefIA "top"))
        ]
        `shouldBeBytes` [0x01, 0x00, 0x00, 0x00, 0x00]

    it "A RefIA reference should return 5 for bottom" $
      assembleAtomsW8
        [ AOp (JumpAbsoluteW32 (RefIA "bottom"))
        , ALabel "bottom"
        ]
        `shouldBeBytes` [0x01, 0x05, 0x00, 0x00, 0x00]

    it "A RefIA reference should be affected by an Image Address offset" $
      assembleAtomsW8
        [ AOp (IAOffset 16)
        , ALabel "top"
        , AOp (JumpAbsoluteW32 (RefIA "top"))
        ]
        `shouldBeBytes` [0x01, 0x10, 0x00, 0x00, 0x00]

    it "A RefRVA reference should not be affected by an Image Address offset" $
      assembleAtomsW8
        [ AOp (IAOffset 16)
        , ALabel "top"
        , AOp (JumpAbsoluteW32 (RefRelativeVA "top"))
        ]
        `shouldBeBytes` [0x01, 0x00, 0x00, 0x00, 0x00]

    it "A Virtual Address Reference should error out at overflow" $
      shouldBeError $ assembleAtomsW8
        [ AOp (VAOffset 0x80)
        , ALabel "top"
        , AOp (JumpAbsoluteW32 (RefVA "top"))
        ]

    it "A Relative Virtual Address reference works before overflow" $
      assembleAtomsW8
        [ AOp (VAOffset 0x80)
        , AOp (VAOffset 0x7F)
        , ALabel "top"
        , AOp (JumpAbsoluteW32 (RefRelativeVA "top"))
        ]
        `shouldBeBytes` [0x01, 0xFF, 0x00, 0x00, 0x00]

    it "A Relative Virtual Address reference errors at overflow" $
      -- Overflow not because of the size allowed by the instruction (Word32),
      -- but because of the type of the relative virtual address (Word8).
      shouldBeError $ assembleAtomsW8
        [ AOp (VAOffset 0x80)
        , AOp (VAOffset 0x80)
        , ALabel "top"
        , AOp (JumpAbsoluteW32 (RefRelativeVA "top"))
        ]

    it "A Virtual Address reference works at maximum" $
      -- Almost overflows because of the computed virtual address
      -- exceeds the word8 address space
      assembleAtomsW8
        [ AOp (VAOffset 0x7F)
        , ALabel "top"
        , AOp (JumpAbsoluteW32 (RefVA "top"))
        ] `shouldBeBytes` [0x01, 0xFF, 0x00, 0x00, 0x00]

    it "A Virtual Address reference errors at overflow" $
      -- Overflow not because of the size allowed by the instruction (Word32),
      -- but because of the type of the virtual address (Word8) computed as the
      -- sum of the offset 0x80 plus the configured offset 0x80 which exceeds
      -- the address space
      shouldBeError $ assembleAtomsW8
        [ AOp (VAOffset 0x80)
        , ALabel "top"
        , AOp (JumpAbsoluteW32 (RefVA "top"))
        ]

    it "A RefVA reference should not be affected by an Image Address offset" $
      assembleAtomsW8
        [ AOp (IAOffset 16)
        , ALabel "top"
        , AOp (JumpAbsoluteW32 (RefVA "top"))
        ]
        `shouldBeBytes` [0x01, 0x80, 0x00, 0x00, 0x00]

    it "Relative backwards image reference minimum (-128)" $
      assembleAtomsW32
        [ ALabel "top"
        , AOp (IAOffset 128)
        , AOp (JumpRelativeW8 (RefIA "top"))
        ]
        `shouldBeBytes` [0x02, 0x80]

    it "Relative backwards image reference underflow (-129) due to instruction" $
      shouldBeError $
        assembleAtomsW32
          [ ALabel "top"
          , AOp (IAOffset 129)
          , AOp (JumpRelativeW8 (RefIA "top"))
          ]

    it "Relative backwards image reference -1" $
      assembleAtomsW8
        [ ALabel "top"
        , AOp (IAOffset 1)
        , AOp (JumpRelativeW8 (RefIA "top"))
        ]
        `shouldBeBytes` [0x02, 0xFF]

    it "Relative image reference offset 0" $
      assembleAtomsW8
        [ ALabel "top"
        , AOp (JumpRelativeW8 (RefIA "top"))
        ]
        `shouldBeBytes` [0x02, 0x00]

    it "Relative image reference offset 2" $
      assembleAtomsW8
        [ AOp (JumpRelativeW8 (RefIA "bottom"))
        , ALabel "bottom"
        ]
        `shouldBeBytes` [0x02, 0x02]

    it "Relative image reference maximum (0x7F)" $
      assembleAtomsW32
        [ AOp (JumpRelativeW8 (RefIA "bottom"))
        , AOp (IAOffset 125)
        , ALabel "bottom"
        ]
        `shouldBeBytes` [0x02, 0x7F]

    it "Relative image reference overflow (0x80)" $
      -- Overflow not because of the address (Word32) but because of
      -- instruction bounds (int8)
      shouldBeError $
        assembleAtomsW32
          [ AOp (JumpRelativeW8 (RefIA "bottom"))
          , AOp (IAOffset 0x80)
          , ALabel "bottom"
          ]

    it "Aligment tests" $ do
      safeAlignW8 0 0        `shouldBe` Left AlignTo0
      safeAlignW8 1 0        `shouldBe` Left AlignTo0
      safeAlignW8 maxBound 0 `shouldBe` Left AlignTo0
      safeAlignW8 0 1        `shouldBe` Right 0
      safeAlignW8 maxBound 1 `shouldBe` Right 0
      safeAlignW8 1 2        `shouldBe` Right 1
      safeAlignW8 2 2        `shouldBe` Right 0
      safeAlignW8 3 2        `shouldBe` Right 1
      safeAlignW8 maxBound (maxWord8 - 1) `shouldBe` Right (maxWord8 - 2)
      safeAlignW8 maxBound (maxWord8 + 1) `shouldBe` Right 1
      safeAlignW8 maxBound (2 * maxWord8) `shouldBe` Right maxWord8

  where
    maxWord8 :: Natural
    maxWord8 = fromIntegral (maxBound :: Word8)

    safeAlignW8 :: Word8 -> Natural -> Either AssemblyError Natural
    safeAlignW8 w8 n = fromIntegral w8 `safeAlign` n

    -- Wraps the bytestring to produce different show output
    shouldBeBytes
      :: HasCallStack => Either AssemblyError BS.ByteString -> [Word8] -> IO ()
    shouldBeBytes (Right got) expected
      = BSByteShow got `shouldBe` BSByteShow (BS.pack expected)
    shouldBeBytes got expected
      = got `shouldBe` Right (BS.pack expected)

    shouldBeError
      :: HasCallStack
      => Either AssemblyError BS.ByteString
      -> IO ()
    shouldBeError (Right got) = expectationFailure $
      "Expecting error, got " <> show (BSByteShow got)
    shouldBeError _ = pure ()

    -- | Run the assembler over an address of type Word8
    assembleAtomsW8
      :: [Atom (TestOpcode (Reference LabelText))]
      -> Either AssemblyError BS.ByteString
    assembleAtomsW8 = assemble configW8 . Seq.fromList

    assembleAtomsW32
      :: [Atom (TestOpcode (Reference LabelText))]
      -> Either AssemblyError BS.ByteString
    assembleAtomsW32 = assemble configW32 . Seq.fromList
