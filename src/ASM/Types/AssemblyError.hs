module ASM.Types.AssemblyError
where

import qualified Data.Text as Text
import qualified Control.Exception as Exception

newtype SomeExceptionWrap
  = ExceptionWrap Exception.SomeException deriving (Show)

-- | Defining a manual instance because SomeException doesn't make it possible
-- to derive Eq. This is a solution to the problem of checking for Exceptions
-- in unit tests.
instance Eq SomeExceptionWrap where
  (ExceptionWrap se1) == (ExceptionWrap se2) = show se1 == show se2

data AssemblyError
  = Arithmetic SomeExceptionWrap
  | ReferenceMissing Text.Text
  | -- | In a calculated label difference value that gives the number of bytes
    -- between the "From" label and the "To" label, the result must be
    -- positive, therefore the "From" label must be defined before the "To"
    -- label.
    FromLabelAfterTo
  | ReferenceTypeNotSupportedInOpcode Text.Text
  | OpcodeToByteString AssemblyError
  | InternalError Text.Text
  | AlignTo0
  | NegativeToNatural
  | ReferenceExists Text.Text
  deriving (Show, Eq)

instance Exception.Exception AssemblyError

