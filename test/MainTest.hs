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
  | Zeroes Natural
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

-- Example of encoding a relative reference to an address. Here relative means
-- relative to the image base memory address
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
    = pure $ BS.singleton 0x03
  encode _ (Zeroes n)
    = pure $ BS.replicate (fromIntegral n) 0x00

  -- | Warning, this should match the Encodable lengths (to be tested)
  sizeRVA (JumpAbsoluteW32 _) = 1 + 4
  sizeRVA (JumpRelativeW8 _)  = 1 + 1
  sizeRVA (Zeroes n)          = n
  sizeRVA Noop                = 1

  sizeIA (JumpAbsoluteW32 _)  = 1 + 4
  sizeIA (JumpRelativeW8 _)   = 1 + 1
  sizeIA (Zeroes n)           = n
  sizeIA Noop                 = 1

configW8 :: Config Word8
configW8 = Config {..} where acVirtualBaseAddress = 0x80

configW32 :: Config Word32
configW32 = Config {..} where acVirtualBaseAddress = 0x100

topReference
  :: (Reference LabelText -> TestOpcode (Reference LabelText))
  -> (LabelText -> Reference LabelText)
  -> Natural
  -> [Atom (TestOpcode (Reference LabelText))]
topReference opcode reference zeroesAfterTop
  = [ ALabel "top"
    , AOp (Zeroes zeroesAfterTop)
    , AOp (opcode (reference "top"))
    ]

midReference
  :: (Reference LabelText -> TestOpcode (Reference LabelText))
  -> (LabelText -> Reference LabelText)
  -> Natural
  -> Natural
  -> [Atom (TestOpcode (Reference LabelText))]
midReference opcode reference zeroesBeforeLabel zeroesAfterLabel
 = [ AOp (Zeroes zeroesBeforeLabel)
   , ALabel "mid"
   , AOp (Zeroes zeroesAfterLabel)
   , AOp (opcode (reference "mid"))
   ]

endReference
  :: (Reference LabelText -> TestOpcode (Reference LabelText))
  -> (LabelText -> Reference LabelText)
  -> Natural
  -> Natural
  -> [Atom (TestOpcode (Reference LabelText))]
endReference opcode reference zeroesBeforeReference zeroesBeforeLabel
 = [ AOp (Zeroes zeroesBeforeReference)
   , AOp (opcode (reference "end"))
   , AOp (Zeroes zeroesBeforeLabel)
   , ALabel "end"
   ]

w8TopLabelAbsoluteSpec
  :: HasCallStack
  => (LabelText -> Reference LabelText) -> String -> Word8 -> Spec
w8TopLabelAbsoluteSpec reference referenceName expectedByte
  = describe [qq|When assembling a reference of type $referenceName at the top|] $
      it [qq|Then byte $expectedByte is obtained|] $
        assembleW8 (topReference JumpAbsoluteW32 reference 1)
            `shouldBeBytes` bytes (0x00:0x01:expectedByte:0x00:0x00:0x00:[])

w8MidLabelAbsoluteSpec
  :: HasCallStack
  => (LabelText -> Reference LabelText) -> String -> Word8 -> Spec
w8MidLabelAbsoluteSpec reference referenceName expectedByte
  = describe [qq|When assembling a reference of type $referenceName in the middle|] $
      it [qq|Then byte $expectedByte is obtained|] $
        assembleW8
          (midReference JumpAbsoluteW32 reference 1 1)
            `shouldBeBytes` bytes (0x00:0x00:0x01:expectedByte:0x00:0x00:0x00:[])

w8EndLabelAbsoluteSpec
  :: HasCallStack
  => (LabelText -> Reference LabelText) -> String -> Word8 -> Spec
w8EndLabelAbsoluteSpec reference referenceName expectedByte
  = describe [qq|When assembling a reference of type $referenceName in the end|] $
      it [qq|Then byte $expectedByte is obtained|] $
        assembleW8
          (endReference JumpAbsoluteW32 reference 1 1)
            `shouldBeBytes` bytes (0x00:0x01:expectedByte:0x00:0x00:0x00:0x00:[])

-- | Note: numZeroes should be the boundary value that causes overflow. Here
-- the overflow doesn't happen because of the encoding of the instruction but
-- because of the assembler.
w8OverflowAbsoluteSpec
  :: HasCallStack
  => (LabelText -> Reference LabelText) -> String -> Natural -> Spec
w8OverflowAbsoluteSpec reference referenceName numZeroes
  = do
    describe [qq|When referencing $referenceName after {numZeroes - 1} zeroes|] $
      it [qq|Then the assembly succeeds|] $
        assembleW8
          (midReference JumpAbsoluteW32 reference (numZeroes - 1) 0)
            `shouldBeBytes` zeroesAndBytes (fromIntegral $ numZeroes - 1)
              [0x01, 0xFF, 0x00, 0x00, 0x00]
    describe [qq|When referencing a $referenceName after $numZeroes zeroes|] $
      it [qq|Then the address space overflows|] $
        shouldBeError $ assembleW8 $
          -- W32 instruction chosen to avoid hitting any instruction
          -- overflow path
          midReference JumpAbsoluteW32 reference numZeroes 0


main :: IO ()
main = hspec $
  do
    it "Empty list assembly returns no bytes" $
      assembleW8 [] `shouldBeBytes` BS.empty

    it "Label should not generate any bytes" $
      assembleW8 [ALabel "l"] `shouldBeBytes` BS.empty

    it "Undefined reference returns an error" $
      shouldBeError $
        assembleW8 [AOp (JumpAbsoluteW32 (RefVA "missing"))]

    it "Address encoding is 32 bit Little Endian" $
      encodeW32 0x100
        `shouldBe` Right (BS.pack [0x00, 0x01, 0x00, 0x00])

    describe "Given a 1 byte address space" $ do
      -- Test of happy-path, basic functionality of absolute references
      w8TopLabelAbsoluteSpec RefVA         "VA"         0x80 -- base image offset
      w8TopLabelAbsoluteSpec RefRelativeVA "RelativeVA" 0x00
      w8TopLabelAbsoluteSpec RefIA         "IA"         0x00
      w8MidLabelAbsoluteSpec RefVA         "VA"         0x81 -- base image offset + 1
      w8MidLabelAbsoluteSpec RefRelativeVA "RelativeVA" 0x01
      w8MidLabelAbsoluteSpec RefIA         "IA"         0x01
      w8EndLabelAbsoluteSpec RefVA         "VA"         0x87 -- base image offset + 7
      w8EndLabelAbsoluteSpec RefRelativeVA "RelativeVA" 0x07
      w8EndLabelAbsoluteSpec RefIA         "IA"         0x07

      -- Overflows of the address space caused solely by reference resolution
      w8OverflowAbsoluteSpec RefVA         "VA"         0x80
      w8OverflowAbsoluteSpec RefRelativeVA "RelativeVA" 0x100
      w8OverflowAbsoluteSpec RefIA         "IA"         0x100

      -- Relative references are more complicated than absolute ones because
      -- they involve subtraction which can both underflow and overflow.
      -- In this case the test covers rather the encoding than the assembler
      -- itself...
      it "Relative backwards image reference -1" $
        assembleW8
          [ ALabel "top"
          , AOp (Zeroes 1)
          , AOp (JumpRelativeW8 (RefIA "top"))
          ]
          `shouldBeBytes` bytes [0x00, 0x02, 0xFF]

      it "Relative image reference offset 0" $
        assembleW8
          [ ALabel "top"
          , AOp (JumpRelativeW8 (RefIA "top"))
          ]
          `shouldBeBytes` bytes [0x02, 0x00]

      it "Relative image reference offset 2" $
        assembleW8
          [ AOp (JumpRelativeW8 (RefIA "end"))
          , ALabel "end"
          ]
          `shouldBeBytes` bytes [0x02, 0x02]

    -- In general here we try to test instruction size constraints
    -- rather than with address space constraints
    describe "Given a 32bit address space" $ do
      it "Relative image reference maximum (0x7F)" $
        assembleW32
          [ AOp (JumpRelativeW8 (RefIA "bottom"))
          , AOp (Zeroes 125)
          , ALabel "bottom"
          ]
          `shouldBeBytes` bytesAndZeroes [0x02, 0x7F] 125

      it "Relative image reference overflow (0x80)" $
        -- Overflow not because of the address (Word32) but because of
        -- instruction bounds (int8)
        shouldBeError $
          assembleW32
            [ AOp (JumpRelativeW8 (RefIA "bottom"))
            , AOp (Zeroes 0x80)
            , ALabel "bottom"
            ]

      it "Relative backwards image reference minimum (-128)" $
        assembleW32
          [ ALabel "top"
          , AOp (Zeroes 128)
          , AOp (JumpRelativeW8 (RefIA "top"))
          ]
          `shouldBeBytes` zeroesAndBytes 128 [0x02, 0x80]

      it "Relative backwards image reference underflow (-129) due to instruction" $
        shouldBeError $
          assembleW32
            [ ALabel "top"
            , AOp (Zeroes 129)
            , AOp (JumpRelativeW8 (RefIA "top"))
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

maxWord8 :: Natural
maxWord8 = fromIntegral (maxBound :: Word8)

safeAlignW8 :: Word8 -> Natural -> Either AssemblyError Natural
safeAlignW8 w8 n = fromIntegral w8 `safeAlign` n

  -- Wraps the bytestring to produce different show output
shouldBeBytes
  :: HasCallStack => Either AssemblyError BS.ByteString -> BS.ByteString -> IO ()
shouldBeBytes (Right got) expected
  = BSByteShow got `shouldBe` BSByteShow expected
shouldBeBytes got expected
  = got `shouldBe` Right expected

shouldBeError
  :: HasCallStack
  => Either AssemblyError BS.ByteString
  -> IO ()
shouldBeError (Right got) = expectationFailure $
  "Expecting error, got " <> show (BSByteShow got)
shouldBeError _ = pure ()

  -- | Run the assembler over an address of type Word8
assembleW8
  :: [Atom (TestOpcode (Reference LabelText))]
  -> Either AssemblyError BS.ByteString
assembleW8 = assemble configW8 . Seq.fromList

assembleW32
  :: [Atom (TestOpcode (Reference LabelText))]
  -> Either AssemblyError BS.ByteString
assembleW32 = assemble configW32 . Seq.fromList

bytes :: [Word8] -> BS.ByteString
bytes = BS.pack

bytesAndZeroes :: [Word8] -> Int64 -> BS.ByteString
bytesAndZeroes xs n = BS.pack xs <> BS.replicate n 0

zeroesAndBytes :: Int64 -> [Word8] -> BS.ByteString
zeroesAndBytes n xs = BS.replicate n 0 <> BS.pack xs
