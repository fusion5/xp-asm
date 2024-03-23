module ASM.Types.Encode (Encode (..)) where

import Common
import Prelude
import ASM.Types.Exception

import qualified Data.Vector.Sized as Vec

class Encode (opcode :: Nat -> Type) where
  encode :: opcode n -> Either AssemblyError (Vec.Vector n Word8)
