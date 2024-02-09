module ASM (
  assemble
) where

import ASM.Types

import qualified Data.Sequence as Seq
-- import qualified Data.ByteString as BS

assemble :: Seq.Seq (Atom (opcode LabelText)) -> Seq.Seq (Atom (opcode address))
assemble _ = undefined

_consumeOpcode :: ASMConfig a -> ASMInput a -> o -> ASMOutput a o
_consumeOpcode = undefined
