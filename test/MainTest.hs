{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module MainTest where

import Test.Hspec

import Common

import ASM
import ASM.Types

import qualified Data.Sequence as Seq
import qualified Data.Binary.Put as Bin
import qualified Data.Word as Word
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BS

-- | Opcode defined for testing purposes
data TestOpcode address
  = JumpTo address
  | JumpRelative address
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

-- | Warning, this should match the Encodable lengths...
-- ByteSized and Encodable are separate to reflect that label references are of
-- known size, but their encoding is not obtainable until resolved
instance ByteSized (TestOpcode a) where
  sizeof (JumpTo _)       = 1 + 4
  sizeof (JumpRelative _) = 1 + 1

instance Address Word32

instance Encodable (TestOpcode (Reference Word.Word32)) where
  encode (JumpTo (RefVA i32))
    = pure $ Seq.fromList $ 0x01 : BS.unpack (Bin.runPut (Bin.putWord32le (fromIntegral i32)))
  encode (JumpRelative (RefForwardOffsetVASolved {..}))
    = do
      delta   <- refCurrentVA `safeMinus` refTargetVA
      deltaW8 <- safeDowncast (fromIntegral delta)
      pure $ Seq.fromList [0x02, deltaW8]
  encode x = Left
    $ ReferenceTypeNotSupportedInOpcode $
      "Invalid combination of opcodes and references: " <> Text.pack (show x)

emptyAtoms :: Seq.Seq (Atom (TestOpcode (Reference LabelText)))
emptyAtoms = Seq.fromList []

testSeq1 :: Seq.Seq (Atom (TestOpcode (Reference LabelText)))
testSeq1 = Seq.fromList [AOp (JumpTo (RefVA "test"))]

testSeq2 :: Seq.Seq (Atom (TestOpcode (Reference LabelText)))
testSeq2 = Seq.fromList [ALabel "TEST"]

defaultConfig :: Config Word.Word32
defaultConfig = Config {..}
  where
    acVirtualBaseAddress = 0

main :: IO ()
main = hspec $ do
  it "Empty list assembly returns an empty BS" $
    assembleAtoms []
      `shouldBe` Right ""
  it "Undefined reference returns an error" $
    assembleAtoms [AOp (JumpTo (RefVA "missing"))]
      `shouldBe` Left (ReferenceMissing "missing")
  where
    assembleAtoms
      :: [Atom (TestOpcode (Reference LabelText))]
      -> Either AssemblyError BS.ByteString
    assembleAtoms = assemble defaultConfig . Seq.fromList
