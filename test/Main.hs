module Main where

import ASM.Types

import Test.Hspec
-- import qualified Data.Text as Text
import qualified Data.Int as Int
import qualified ASM
import qualified Data.Sequence as Seq

-- Example opcodes we pass to the assembler:
-- input:
--  TestOpcode LabelText -- label references
-- output:
--  TetOpcode Int64 -- resolved label references

data TestOpcode address
  = JumpTo address
  | Other
  deriving (Show, Eq)

emptySeqIn :: Seq.Seq (Atom (TestOpcode LabelText))
emptySeqIn = Seq.fromList []

emptySeqOut :: Seq.Seq (Atom (TestOpcode Int.Int64))
emptySeqOut = Seq.fromList []

main :: IO ()
main = hspec $ do
  describe "Given a test input..." $ do
    it "Results in the test output" $ do
      ASM.assemble emptySeqIn `shouldBe` emptySeqOut
      -- ASM.assemble [AOpcode Other, JumpTo "ABC"] `shouldBe` [Other]
