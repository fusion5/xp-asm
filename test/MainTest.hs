{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module MainTest where

import Test.Hspec

import qualified ASM
import qualified ASM.Types as ASM

-- import qualified Data.Int as Int
import qualified Data.Sequence as Seq
import qualified Data.Binary as Bin
import qualified Data.Binary.Put as Bin
import qualified Data.Word as Word
import qualified Data.Text as Text

import GHC.Generics

-- Example opcodes we pass to the assembler:
-- input:
--  TestOpcode LabelText -- label references
-- output:
--  TetOpcode Int64 -- resolved label references

data TestOpcode address
  = JumpTo address
  | JumpRelative address
  | A1MinusA2 address address
  | FileOffset address
  | Other
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

-- | Warning, it should be tested that this matches the asmToBin lengths...
instance ASM.Sized (TestOpcode a) where
  sizeof (JumpTo _)       = 1 + 4
  sizeof (JumpRelative _) = 1 + 1
  sizeof (A1MinusA2 _ _)  = 1 + 4
  sizeof (FileOffset _)   = 1 + 4
  sizeof Other            = 1

instance ASM.ToBS (TestOpcode (ASM.SolvedReference Word.Word32)) where
  asmToBin (JumpTo (ASM.SolvedRefVA i32))
    = pure $ Bin.runPut $ do
        Bin.putWord8 0x01
        Bin.putWord32le (fromIntegral i32)
  asmToBin (JumpRelative (ASM.SolvedRefForwardOffsetVA {..}))
    = do
      delta   <- sfoCurrentVA `ASM.safeMinus` sfoTargetVA
      deltaW8 <- ASM.safeDowncast (fromIntegral delta)
      pure $ Bin.runPut $ do
        Bin.putWord8 0x02
        Bin.putWord8 deltaW8
  asmToBin x = Left
    $ ASM.ReferenceTypeNotSupportedInOpcode $
      "Invalid combination of opcodes and references: " <> Text.pack (show x)

-- emptySeqIn :: Seq.Seq (Atom (TestOpcode LabelText))
-- emptySeqIn = Seq.fromList []

-- emptySeqOut :: Seq.Seq (Atom (TestOpcode Int.Int64))
-- emptySeqOut = Seq.fromList []

testSeq :: Seq.Seq (ASM.Atom (TestOpcode ASM.Reference))
testSeq = Seq.fromList [ASM.AOp (JumpTo (ASM.RefVA "test"))]

defaultConfig :: ASM.Config Word.Word32
defaultConfig = ASM.Config {..}
  where
    acVirtualBaseAddress = 0

emptyInput :: Seq.Seq (ASM.Atom (TestOpcode ASM.Reference))
emptyInput = Seq.fromList []

main :: IO ()
main = hspec $ do
  it "Empty list assembly returns in empty BS" $ do
    ASM.assemble defaultConfig emptyInput `shouldBe` Right ""
