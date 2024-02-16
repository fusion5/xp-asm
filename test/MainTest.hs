{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Data.Proxy as P
import qualified Data.Vector.Sized as Vec

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
  safe = undefined

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

data Test2 (address :: Type) (n :: Nat) where
  JumpTo2 :: address -> Test2 address 5
  JumpRelative2 :: address -> Test2 address 2
  Literal2 :: Word8 -> Test2 address 2

instance Show (Test2 a n) where
  show _ = "Test2 {contents not shown}"

instance (KnownNat n) => ASM.ByteSized (Test2 a n) where
  sizeof _ = natVal (P.Proxy @n)

instance ASM.ToWord8s (Test2 (ASM.Reference Word.Word32) n) where
  toWord8s (JumpTo2 (ASM.RefVA i32)) = do
    pure $ Seq.fromList $ 0x01 : BS.unpack (Bin.runPut (Bin.putWord32le (fromIntegral i32)))
  toWord8s (JumpRelative2 (ASM.RefForwardOffsetVASolved {..})) = do
    delta   <- refCurrentVA `ASM.safeMinus` refTargetVA
    deltaW8 <- ASM.safeDowncast (fromIntegral delta)
    pure $ Seq.fromList [0x02, deltaW8]
  toWord8s (Literal2 w8) = pure $ Seq.fromList [0x03, w8]
  toWord8s x = Left
    $ ASM.ReferenceTypeNotSupportedInOpcode $
      "Invalid combination of opcodes and references: " <> Text.pack (show x)
  safe = undefined

convert :: Test2 (ASM.Reference Word.Word32) n -> Vec.Vector n Word8
convert (JumpTo2 (ASM.RefVA _i32)) =
  Vec.fromTuple (0x01, 0x02, 0x03, 0x04, 0x05)
convert (Literal2 w8) = Vec.fromTuple (0x03, w8)
convert _ = error "AA"


main :: IO ()
main = hspec $ do
  it "Empty list assembly returns an empty BS" $
    ASM.assemble defaultConfig testSeq0 `shouldBe` Right ""
  it "Blabla" $
    ASM.assemble defaultConfig testSeq3 `shouldBe` Right "\x03\x10"
