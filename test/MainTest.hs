{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module MainTest where

import Test.Hspec

import Common

import qualified ASM
import qualified ASM.Types as ASM

import qualified Data.Binary.Put as Bin
import qualified Data.Word as Word
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Sized as Vec

testSeq0 :: ASM.Container (ASM.Atom (Opcode (ASM.Reference ASM.LabelText))) 0
testSeq0 = ASM.Nil

testSeq1 :: ASM.Container (ASM.Atom (Opcode (ASM.Reference ASM.LabelText))) 7
testSeq1 =
  ASM.Tree
    (ASM.Leaf $ ASM.Atom (JumpTo (ASM.RefVA "test")))
    (ASM.Tree (ASM.Leaf $ ASM.Atom (Literal 0x10)) ASM.Nil)

testSeq2 :: ASM.Container (ASM.Atom (Opcode (ASM.Reference ASM.LabelText))) 0
testSeq2 =
  ASM.Tree (ASM.Leaf (ASM.Label "TEST")) ASM.Nil

testSeq3 :: ASM.Container (ASM.Atom (Opcode (ASM.Reference ASM.LabelText))) 2
testSeq3 =
  ASM.Tree
    (ASM.Tree (ASM.Leaf $ ASM.Atom (Literal 0x10)) ASM.Nil)
    ASM.Nil

testSeq4 :: ASM.Container (ASM.Atom (Opcode (ASM.Reference ASM.LabelText))) 4
testSeq4 =
  ASM.Tree
    (ASM.Leaf $ ASM.Atom (Literal 0x10))
    (ASM.Tree (ASM.Leaf $ ASM.Atom $ Literal 0x20) ASM.Nil)


defaultConfig :: ASM.Config Word.Word32
defaultConfig = ASM.Config {..}
  where
    acVirtualBaseAddress = 0

data Opcode (address :: Type) (n :: Nat) where
  JumpTo       :: address -> Opcode address 5
  JumpRelative :: address -> Opcode address 2
  Literal      :: Word8   -> Opcode address 2

instance Show (Opcode a n) where
  show _ = "Opcode {contents not shown}"

errorText :: Text.Text -> a
errorText = error . Text.unpack

-- Could be in another module, is it possible to do this more efficient?

word32le :: Word32 -> (Word8, Word8, Word8, Word8)
word32le w32
  = case BS.unpack (Bin.runPut (Bin.putWord32le w32)) of
    [w0, w1, w2, w3] -> (w0, w1, w2, w3)
    _                -> error "internal error"

instance ASM.Binary (Opcode (ASM.Reference Word.Word32)) where
  encode (JumpTo (ASM.RefVA i32)) = do
    pure $ 0x01 `Vec.cons` Vec.fromTuple (word32le i32)
  encode (JumpRelative (ASM.RefForwardOffsetVASolved {..})) = do
    delta   <- refCurrentVA `ASM.safeMinus` refTargetVA
    deltaW8 <- ASM.safeDowncast (fromIntegral delta)
    pure $ Vec.fromTuple (0x02, deltaW8)
  encode (Literal w8) = pure $ Vec.fromTuple (0x03, w8)
  encode x = Left $
        ASM.ReferenceTypeNotSupportedInOpcode $
          "Invalid combination of opcodes and references: " <> Text.pack (show x)

-- Provide code for updating addresses/references in an Applicative context
-- (in our case Either Exception). This boilerplate is required because
-- it's not possible to derive instances for Opcode.
instance ASM.FunctorMSized Opcode where
  mapMSized f = go
    where
      go (JumpTo ref) = JumpTo <$> f ref
      go (JumpRelative ref) = JumpRelative <$> f ref
      go (Literal w8) = pure $ Literal w8

instance (ASM.Address Word32)

main :: IO ()
main = hspec $ do
  it "Empty list assembly returns an empty BS" $
    ASM.assemble defaultConfig testSeq0 `shouldBe` Right ""
  it "Undefined reference should result in an error" $
    ASM.assemble defaultConfig testSeq1 `shouldBe` Left (ASM.ReferenceMissing "test")
  it "A sequence of 1 opcode results in a sequence of bytes" $
    ASM.assemble defaultConfig testSeq3 `shouldBe` Right "\x03\x10"
  it "A sequence of 2 opcodes results in a sequence of bytes" $
    ASM.assemble defaultConfig testSeq4 `shouldBe` Right "\x03\x10\x03\x20"

