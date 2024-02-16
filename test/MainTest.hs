{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module MainTest where

import Test.Hspec

import Common

import qualified ASM
import qualified ASM.Types as ASM

import qualified Data.Sequence as Seq
import qualified Data.Binary.Put as Bin
import qualified Data.Word as Word
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BS

-- Example opcodes we pass to the assembler:
-- input:
--  TestOpcode LabelText -- label references
-- output:
--  TetOpcode Int64 -- resolved label references

data TestOpcode address
  = JumpTo address
  | JumpRelative address
  | Literal Word8
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

-- | Warning, it should be tested that this matches the asmToBin lengths...
instance ASM.ByteSized (TestOpcode a) where
  sizeof (JumpTo _)       = 1 + 4
  sizeof (JumpRelative _) = 1 + 1
  sizeof (Literal _)      = 2

instance ASM.Address Word32

instance ASM.ToWord8s (TestOpcode (ASM.Reference Word.Word32)) where
  toWord8s (JumpTo (ASM.RefVA i32))
    = do
      pure $ Seq.fromList $ 0x01 : BS.unpack (Bin.runPut (Bin.putWord32le (fromIntegral i32)))
  toWord8s (JumpRelative (ASM.RefForwardOffsetVASolved {..}))
    = do
      delta   <- refCurrentVA `ASM.safeMinus` refTargetVA
      deltaW8 <- ASM.safeDowncast (fromIntegral delta)
      pure $ Seq.fromList [0x02, deltaW8]
  toWord8s (Literal word8) =
      pure $ Seq.fromList [0x03, word8]
  toWord8s x = Left
    $ ASM.ReferenceTypeNotSupportedInOpcode $
      "Invalid combination of opcodes and references: " <> Text.pack (show x)

testSeq0 :: Seq.Seq (ASM.Atom (TestOpcode (ASM.Reference ASM.LabelText)))
testSeq0 = Seq.fromList []

testSeq1 :: Seq.Seq (ASM.Atom (TestOpcode (ASM.Reference ASM.LabelText)))
testSeq1 = Seq.fromList [ASM.Atom (JumpTo (ASM.RefVA "test"))]

testSeq2 :: Seq.Seq (ASM.Atom (TestOpcode (ASM.Reference ASM.LabelText)))
testSeq2 = Seq.fromList [ASM.Label "TEST"]

testSeq3 :: Seq.Seq (ASM.Atom (TestOpcode (ASM.Reference ASM.LabelText)))
testSeq3 = Seq.fromList [ASM.Atom (Literal 0x10)]

defaultConfig :: ASM.Config Word.Word32
defaultConfig = ASM.Config {..}
  where
    acVirtualBaseAddress = 0

main :: IO ()
main = hspec $ do
  it "Empty list assembly returns an empty BS" $
    ASM.assemble defaultConfig testSeq0 `shouldBe` Right ""
  it "Blabla" $
    ASM.assemble defaultConfig testSeq3 `shouldBe` Right "\x03\x10"
