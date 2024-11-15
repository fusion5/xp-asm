{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module MainTest where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances.Natural ()

import Common

import ASM
import ASM.Types
import ASM.Types.Position
import Data.Int
import Test.Utils

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Binary.Put as Bin
import qualified Data.ByteString.Lazy as BS

-- | Some tests use a Word8 address space, others a Word32 address space.
instance Address Word8
instance Address Word32

main :: IO ()
main = hspec $
  do
    miscSpec
    address8Spec
    address32Spec
    positionSpec
    alignAtomSpec
    multiLabelSpec

-- | Things possibly used in a linkable object such as ELF for testing purposes.
-- Supports recursion.
data TestLinkableObject address
  = Section
    { beginLabel     :: LabelText
    , numberOfZeroes :: Natural
    , subsection     :: TestLinkableObject address
    , endLabel       :: LabelText
    }
  | SectionReferences
    { beginAddress :: address
    , endAddress   :: address
    }

instance Encodable TestLinkableObject where
  encode pI Section{..} = do
    subBS <- encode pI subsection
    pure $ BS.replicate (fromIntegral numberOfZeroes) 0x00 <> subBS
  encode _ SectionReferences{..} = do
    begin <- encodeAbsoluteW32 beginAddress
    end   <- encodeAbsoluteW32 endAddress
    pure $ begin <> end

  labels s0 Section{..} = do
    s1 <- updateLabels (insertLabel beginLabel (asPosition s0)) s0
    s2 <- labels s1 subsection
    updateLabels (insertLabel endLabel (asPosition s2)) s2
  labels s SectionReferences{} = pure s

  size Section{..} = numberOfZeroes + size subsection
  size SectionReferences{} = 8

-- | Opcode (e.g. x86-64) defined for testing purposes
data TestOpcode address
  = JumpAbsoluteW32 address
  | JumpRelativeW8 address
  | Noop
  | Zeroes Natural
  | Label LabelText
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

-- Example of encoding of an address
encodeAbsoluteW32
  :: Address addr
  => Reference addr
  -> Either AssemblyError BS.ByteString
encodeAbsoluteW32 = go
  where
    go (RefIA a)         = enc a
    go (RefRelativeVA a) = enc a
    go (RefVA a)         = enc a
    enc a = integralToPosition a >>= positionDowncast >>= encodeW32

-- | Example of encoding a relative reference to an address:
--
-- 0x00 00
-- 0x01 00 .test
-- 0x02 00
-- 0x03 <reference> (1 byte size reference to .test)
-- 0x04
-- ...
--
-- Decodes to
--
-- 0x00 00
-- 0x01 00
-- 0x02 00
-- 0x03 FE (-2 integer encoding)
-- 0x04
-- ...
--
-- If the offset exceeds 1 byte signed integer then error out with overflow.
encodeRelativeW8
  :: Address addr
  => PositionInfo
  -> Reference addr
  -> Either AssemblyError BS.ByteString
encodeRelativeW8 PositionInfo {..} solvedReference
  = do
    targetPosition <- integralToPosition target
    sub targetPosition currentPosition >>= encodeI8W8
  where
    (target, currentPosition)        = terms solvedReference
    terms (RefIA         targetAddr) = (targetAddr, piIA)
    terms (RefRelativeVA targetAddr) = (targetAddr, piRelativeVA)
    terms (RefVA         targetAddr) = (targetAddr, piVA)

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
  encode _ (Label _)
    = pure BS.empty

  labels s@StateLabelScan{..} (Label labelText)
    = pure s { aslsLabels = Map.insert labelText asPosition aslsLabels }
  labels s _ = pure s

  -- | Warning, this should match the Encodable lengths (to be tested)
  size (JumpAbsoluteW32 _) = 1 + 4
  size (JumpRelativeW8 _)  = 1 + 1
  size (Zeroes n)          = n
  size Noop                = 1
  size (Label _)           = 0

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
  = [ AOp (Label "top")
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
   , AOp (Label "mid")
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
   , AOp (Label "end")
   ]

w8AbsoluteSpec
  :: HasCallStack
  => (LabelText -> Reference LabelText) -> String -> Word8 -> Spec
w8AbsoluteSpec reference referenceName baseImageOffset
  = do
    it [qq|Absolute reference of type $referenceName to the top|] $
      assembleW8 (topReference JumpAbsoluteW32 reference 1)
        `shouldBeBytes`
          bytes (0x00:0x01:baseImageOffset:0x00:0x00:0x00:[])
    it [qq|Absolute reference of type $referenceName to the middle|] $
      assembleW8 (midReference JumpAbsoluteW32 reference 1 1)
        `shouldBeBytes`
          bytes (0x00:0x00:0x01:(baseImageOffset + 1):0x00:0x00:0x00:[])
    it [qq|Absolute reference of type $referenceName to the end|] $
      assembleW8 (endReference JumpAbsoluteW32 reference 1 1)
        `shouldBeBytes`
          bytes (0x00:0x01:(baseImageOffset + 7):0x00:0x00:0x00:0x00:[])

w32RelativeSpec
  :: HasCallStack
  => (LabelText -> Reference LabelText) -> String -> Spec
w32RelativeSpec reference referenceName
  = do
    it [qq|Relative -1 backwards reference of type $referenceName|] $
      assembleW32 (topReference JumpRelativeW8 reference 1)
        `shouldBeBytes` bytes [0x00, 0x02, 0xFF]
    it [qq|Relative 0-offset reference of type $referenceName|] $
      assembleW32 (topReference JumpRelativeW8 reference 0)
        `shouldBeBytes` bytes [0x02, 0x00]
    it [qq|Relative 2 forwards reference of type $referenceName|] $
      assembleW32 (endReference JumpRelativeW8 reference 0 0)
        `shouldBeBytes` bytes [0x02, 0x02]

miscSpec :: Spec
miscSpec
  = do
    it "Empty list assembly returns no bytes" $
      assembleW8 [] `shouldBeBytes` BS.empty

    it "Label should not generate any bytes" $
      assembleW8 [AOp (Label "l")] `shouldBeBytes` BS.empty

    it "Undefined reference returns an error" $
      shouldBeError $
        assembleW8 [AOp (JumpAbsoluteW32 (RefVA "missing"))]

    it "Address encoding is 32 bit Little Endian" $
      encodeW32 0x100
        `shouldBe` Right (BS.pack [0x00, 0x01, 0x00, 0x00])

address8Spec :: Spec
address8Spec
  = describe "Given an 8 bit address space" $ do
      -- Note: unhappy paths are not tested, because they are covered by
      -- position tests, since Position is mandatory
      let Config {..} = configW8
      w8AbsoluteSpec RefIA         "IA"         0x00
      w8AbsoluteSpec RefRelativeVA "RelativeVA" 0x00
      w8AbsoluteSpec RefVA         "VA"         acVirtualBaseAddress

address32Spec :: Spec
address32Spec
  = -- Test instruction size constraints rather than address space constraints
    describe "Given a 32bit address space" $ do
      -- Note: unhappy paths are not tested, because they are covered by
      -- position tests, since Position is mandatory
      -- Relative references are more complicated than absolute ones because
      -- they involve subtraction which can both underflow and overflow.
      -- In this case the test covers rather the encoding than the assembler
      -- itself...
      w32RelativeSpec RefIA         "IA"
      w32RelativeSpec RefRelativeVA "RelativeVA"
      w32RelativeSpec RefVA         "VA"

positionSpec :: Spec
positionSpec =
  describe "Position tests" $ do
    it "Alignment tests" $ do
      align 0 zero      `shouldBe` Left AlignTo0
      align 0 (mkPos 1) `shouldBe` Left AlignTo0
      align 1 zero      `shouldBe` Right zero
      align 1 (mkPos 1) `shouldBe` Right zero
      align 2 zero      `shouldBe` Right zero
      align 2 (mkPos 1) `shouldBe` Right (mkPos 1)
      align 2 (mkPos 2) `shouldBe` Right zero
    prop "Align property " $ \n -> do
      align (n+1) (mkPos $ n+2) `shouldBe` Right (mkPos n)
    it "Conversion from address to position" $ do
      integralToPosition (1  :: Integer) `shouldBe` Right (mkPos 1)
      integralToPosition (0  :: Integer) `shouldBe` Right zero
      integralToPosition (-1 :: Integer) `shouldBe` Left NegativeToNatural
    it "Downcast tests" $ do
      positionDowncast zero          `shouldBe`      mkRightW8 0
      positionDowncast (mkPos 0xFF)  `shouldBe`      mkRightW8 0xFF
      positionDowncast (mkPos 0x100) `shouldSatisfy` isLeftW8
      positionDowncast zero          `shouldBe`      mkRightI8 0
      positionDowncast (mkPos 0x7F)  `shouldBe`      mkRightI8 0x7F
      positionDowncast (mkPos 0x80)  `shouldSatisfy` isLeftI8
    it "Subtraction tests with under/overflows" $ do
      sub zero      zero         `shouldBe`      mkRightI8 0
      sub (mkPos 1) zero         `shouldBe`      mkRightI8 1
      sub zero      (mkPos 1)    `shouldBe`      mkRightI8 (-1)
      sub zero      (mkPos 0x80) `shouldBe`      mkRightI8 (-128)
      sub zero      (mkPos 0x81) `shouldSatisfy` isLeftI8
      sub (mkPos 0x7F) zero      `shouldBe`      mkRightI8 127
      sub (mkPos 0x80) zero      `shouldSatisfy` isLeftI8
  where
    mkRightW8 :: Word8 -> Either AssemblyError Word8
    mkRightW8 = Right
    isLeftW8 :: Either AssemblyError Word8 -> Bool
    isLeftW8 = either (const True) (const False)
    mkRightI8 :: Int8 -> Either AssemblyError Int8
    mkRightI8 = Right
    isLeftI8 :: Either AssemblyError Int8 -> Bool
    isLeftI8 = either (const True) (const False)

alignAtomSpec :: Spec
alignAtomSpec = do
  describe "Align image atom tests" $ do
    it "Align image to zero should result in error" $
      shouldBeError $ assembleW8 [AAlignIA 0]
    it "Align image to one should produce no bytes" $
      assembleW8 [AAlignIA 1] `shouldBeBytes` BS.empty
    it "Align image should work" $
      assembleW8 [AOp (Zeroes 1), AAlignIA 10]
        `shouldBeBytes` BS.replicate 10 0
  describe "Align memory atom tests" $ do
    it "Align memory to zero should result in error" $
      shouldBeError $ assembleW8 [AAlignVA 0]
    it "Align memory should not emit bytes" $
      assembleW8 [AOp (Zeroes 1), AAlignVA 10]
        `shouldBeBytes` BS.singleton 0
    it "Align memory should work" $ do
      -- This assumes that the base address is already aligned to 0x10
      (do
        baseAddress <- integralToPosition acVirtualBaseAddress
        align 0x10 baseAddress
        ) `shouldBe` Right (mkPos 0)

      assembleW8
        [ AOp (Zeroes 1)
        , AAlignVA 0x10
        , AOp (Label "here")
        , AOp (JumpAbsoluteW32 (RefVA         "here"))
        , AOp (JumpAbsoluteW32 (RefRelativeVA "here"))
        , AOp (JumpAbsoluteW32 (RefIA         "here"))
        ]
          `shouldBeBytes` bytes
            [ 0x00  -- Zeroes 1
                    -- VA and RVA set to 10
                    -- "here"
                    -- absolute VA of "here":
            , 0x01, acVirtualBaseAddress + 0x10, 0x00, 0x00, 0x00
                    -- absolute RVA of "here":
            , 0x01, 0x10, 0x00, 0x00, 0x00
                    -- absolute IA of "here":
            , 0x01, 1, 0x00, 0x00, 0x00
            ]
  where Config {..} = configW8

multiLabelSpec :: Spec
multiLabelSpec =
  describe "Objects with multiple labels" $
    it "Can encode mutliple labels" pending

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
