module ASM.Types.Exception (
  AssemblyError (..)
) where

import Prelude
import qualified Data.Text as Text
import qualified Control.Exception as Exception

data AssemblyError
  = Arithmetic Exception.SomeException
  | ReferenceMissing Text.Text
  | FromLabelAfterTo
  | ReferenceTypeNotSupportedInOpcode Text.Text
  | OpcodeToByteString AssemblyError
  | InternalError Text.Text
  deriving (Show)

instance Exception.Exception AssemblyError
