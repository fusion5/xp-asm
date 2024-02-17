{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module MainTest where

import Test.Hspec

import Common

import qualified ASM
import qualified ASM.Types as ASM

import qualified Data.Binary.Put as Bin
import qualified Data.Word as Word
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BS
import qualified Data.Proxy as P
import qualified Data.Vector.Sized as Vec

testSeq0 :: ASM.Container (ASM.Atom (Test2 (ASM.Reference ASM.LabelText))) 0
testSeq0 = ASM.Nil

testSeq1 :: ASM.Container (ASM.Atom (Test2 (ASM.Reference ASM.LabelText))) 7
testSeq1 = ASM.Atom (JumpTo2 (ASM.RefVA "test"))
  `ASM.Cons` (ASM.Atom (Literal2 0x10) `ASM.Cons` ASM.Nil)

testSeq2 :: ASM.Container (ASM.Atom (Test2 (ASM.Reference ASM.LabelText))) 0
testSeq2 = ASM.Label "TEST" `ASM.Cons` ASM.Nil

testSeq3 :: ASM.Container (ASM.Atom (Test2 (ASM.Reference ASM.LabelText))) 2
testSeq3 = ASM.Atom (Literal2 0x10) `ASM.Cons` ASM.Nil

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

errorText :: Text.Text -> a
errorText = error . Text.unpack

-- Could be in another module, is it possible to do this more efficient?

class (Integral a, ASM.Address a) => Reference32 a where
  word32le :: a -> (Word8, Word8, Word8, Word8)

instance Reference32 Word.Word32 where
  word32le w32
    = case BS.unpack (Bin.runPut (Bin.putWord32le w32)) of
      [w0, w1, w2, w3] -> (w0, w1, w2, w3)
      _                -> error "internal error"

-- We make labels to be fake references for completeness sake
instance Reference32 ASM.LabelText where
  word32le _ = word32le (0 :: Word32)

instance Integral ASM.LabelText where
  quotRem = undefined
  toInteger = undefined

instance (Reference32 ref) => ASM.ToWord8s (Test2 (ASM.Reference ref)) where
  safe (JumpTo2 (ASM.RefVA i32)) = do
    pure $ 0x01 `Vec.cons` Vec.fromTuple (word32le i32)
  safe (JumpRelative2 (ASM.RefForwardOffsetVASolved {..})) = do
    delta   <- refCurrentVA `ASM.safeMinus` refTargetVA
    deltaW8 <- ASM.safeDowncast (fromIntegral delta)
    pure $ Vec.fromTuple (0x02, deltaW8)
  safe (Literal2 w8) = pure $ Vec.fromTuple (0x03, w8)
  safe x = Left $
        ASM.ReferenceTypeNotSupportedInOpcode $
          "Invalid combination of opcodes and references: " <> Text.pack (show x)

convert :: Test2 (ASM.Reference Word.Word32) n -> Vec.Vector n Word8
convert (JumpTo2 (ASM.RefVA _i32)) =
  Vec.fromTuple (0x01, 0x02, 0x03, 0x04, 0x05)
convert (Literal2 w8) = Vec.fromTuple (0x03, w8)
convert _ = error "AA"

-- Provide code for mapping addresses/references?
instance ASM.FunctorSized2 Test2 where
  fmapSized2 f = go
    where
      go (JumpTo2 ref) = JumpTo2 (f ref)
      go (JumpRelative2 ref) = JumpRelative2 (f ref)
      go (Literal2 w8) = Literal2 w8

instance (ASM.Address Word32)

main :: IO ()
main = hspec $ do
  it "Empty list assembly returns an empty BS" $
    ASM.assemble defaultConfig testSeq0 `shouldBe` Right ""
  it "Blabla" $
    ASM.assemble defaultConfig testSeq3 `shouldBe` Right "\x03\x10"
