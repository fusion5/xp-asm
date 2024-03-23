{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QualifiedDo #-}

module MainTest where

import Test.Hspec

import Common
import Prelude
import ASM.Types.WriterSized as WriterSized

import qualified ASM
import qualified ASM.Types as ASM
import qualified Container

import qualified Data.Binary.Put as Bin
import qualified Data.Word as Word
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Sized as Vec

type OpcodeLabels = Opcode (ASM.Reference ASM.LabelText)

writeAtom :: Opcode ref n -> WriterSized (Container.Container (ASM.Atom (Opcode ref))) () n
writeAtom = write . ASM.Atom

writeLabel :: ASM.LabelText -> WriterSized (Container.Container (ASM.Atom (Opcode ref))) () 0
writeLabel = write . ASM.Label

testSeq0 :: ASM.AtomContainer OpcodeLabels 0
testSeq0 = WriterSized.execWriter $ WriterSized.do
  WriterSized.pure ()

testSeq1 :: ASM.AtomContainer OpcodeLabels 7
testSeq1 = WriterSized.execWriter $ WriterSized.do
  writeAtom $ JumpTo (ASM.RefVA "test")
  writeAtom $ Literal 0x10

testSeq2 :: ASM.AtomContainer OpcodeLabels 0
testSeq2 = WriterSized.execWriter $ writeLabel "TEST"

testSeq3 :: ASM.AtomContainer OpcodeLabels 2
testSeq3 = WriterSized.execWriter $ writeAtom $ Literal 0x10

testSeq4 :: ASM.AtomContainer OpcodeLabels 4
testSeq4
  = execWriter $ WriterSized.do
      writeAtom $ Literal 0x10
      writeAtom $ Literal 0x20

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

instance ASM.Encode (Opcode (ASM.Reference Word.Word32)) where
  encode (JumpTo (ASM.RefVA i32)) = do
    Prelude.pure $ 0x01 `Vec.cons` Vec.fromTuple (word32le i32)
  encode (JumpRelative (ASM.RefForwardOffsetVASolved {..})) = do
    delta   <- refCurrentVA `ASM.safeMinus` refTargetVA
    deltaW8 <- ASM.safeDowncast (fromIntegral delta)
    Prelude.pure $ Vec.fromTuple (0x02, deltaW8)
  encode (Literal w8) = Prelude.pure $ Vec.fromTuple (0x03, w8)
  encode x = Left $
        ASM.ReferenceTypeNotSupportedInOpcode $
          "Invalid combination of opcodes and references: " <> Text.pack (show x)

-- Provide code for updating addresses/references in an Applicative context
-- (in our case Either Exception). This boilerplate is required because
-- it's not possible to derive instances for Opcode.
instance ASM.TraversableSized Opcode where
  mapMSized f = go
    where
      go (JumpTo ref) = JumpTo <$> f ref
      go (JumpRelative ref) = JumpRelative <$> f ref
      go (Literal w8) = Prelude.pure $ Literal w8

instance (ASM.Address Word32)

shouldSatisfyError
  :: (Show a, HasCallStack)
  => Either ASM.AssemblyError a -> (ASM.AssemblyError -> Bool) -> Expectation
shouldSatisfyError (Left e) p = e `shouldSatisfy` p
shouldSatisfyError (Right x) _ = expectationFailure $ "Expecting an error, got: " ++ show x

shouldBeSuccess
  :: (Show a, Eq a, HasCallStack)
  => Either ASM.AssemblyError a -> a -> Expectation
shouldBeSuccess (Left e) _ = expectationFailure $ "Expecting success, got error: " ++ show e
shouldBeSuccess (Right x) y = x `shouldBe` y

isReferenceMissing :: Text.Text -> ASM.AssemblyError -> Bool
isReferenceMissing expected (ASM.ReferenceMissing got) = expected == got
isReferenceMissing _ _ = False

main :: IO ()
main = hspec $ do
  it "Empty list assembly returns an empty BS" $
    ASM.assemble defaultConfig testSeq0 `shouldBeSuccess` ""
  it "Undefined reference should result in an error" $
    ASM.assemble defaultConfig testSeq1 `shouldSatisfyError` isReferenceMissing "test"
  it "A sequence of 1 opcode results in a sequence of bytes" $
    ASM.assemble defaultConfig testSeq3 `shouldBeSuccess` "\x03\x10"
  it "A sequence of 2 opcodes results in a sequence of bytes" $
    ASM.assemble defaultConfig testSeq4 `shouldBeSuccess` "\x03\x10\x03\x20"
