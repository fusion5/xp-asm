{-# LANGUAGE DeriveGeneric #-}
module ASM.Types where

import GHC.Generics

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as BS
import qualified Data.Int as Int
import qualified Control.Exception as Exception

data AssemblyError
  = Arithmetic SomeExceptionWrap
  | ReferenceMissing LabelText
  | FromLabelAfterTo
  | ReferenceTypeNotSupportedInOpcode Text.Text
  | OpcodeToByteString AssemblyError
  deriving (Show, Eq)

instance Exception.Exception AssemblyError

newtype SomeExceptionWrap = SEW Exception.SomeException deriving (Show)

instance Eq SomeExceptionWrap where
  (SEW se1) == (SEW se2) = show se1 == show se2

-- Defining a manual instance because SomeException doesn't make it possible to derive

type LabelText = Text.Text

class Sized a where
  sizeof :: a -> Int.Int64

-- | The Binary class doesn't allow for nice error handling, therefore we do it using a
-- different one.
class ToBS a where
  asmToBin :: a -> Either AssemblyError BS.ByteString

data AddressInfo address
  = AddressInfo
  { -- TODO: document
    aiIA  :: address
  , -- TODO: document
    aiRelativeVA :: address
  }

-- | Referrer --- reference --> Referree
-- The type of references defined here must cover the needs of all assemblers
-- defined using ASM.
data Reference
  = -- | Virtual Address (in-memory address) of label
    RefVA LabelText
  | -- | Relative Virtual Address (in-memory address minus image base
    -- address) of label
    RefRelativeVA LabelText
  | -- | Image Address (in-file address, offset from the beginning of)
    -- the file) of label
    RefIA LabelText
  | -- | Offset to another label's Virtual Address from the Virtual Address of
    -- the referrer begin location. A signed value. Used for relative jumps. If positive
    -- then the target is below. If negative then the target is above. For x64 note that
    -- you are interested in the offset relative to the referrer END location, so you will
    -- have to add the width of the reference.
    RefForwardOffsetVA LabelText
  | -- | The unsigned offset from the first label Image Address to the
    -- second one: the first label must be <= than the second one and
    -- the delta must fit the given Size (this is to be error-checked
    -- at run time). Helps to define executable file values.
    -- TODO: Couldn't we convert this to the delta between the current pos and a label?
    RefLabelDifferenceIA
      { difiaFrom :: LabelText
      , difiaTo   :: LabelText
      }

-- | The solver provides the addresses of the labels involved. But how they should be
-- processed is up to the library user.
data SolvedReference address
  = SolvedRefVA address
  | SolvedRefRelativeVA address
  | SolvedRefIA address
  | SolvedRefForwardOffsetVA
    { sfoCurrentVA :: address
    , sfoTargetVA  :: address
    }
  | SolvedRefLabelDifferenceIA
    { srdiaFrom :: address
    , srdiaTo   :: address
    }
  deriving (Show)

-- | The operation type is a subset of the instruction set, e.g. for x86 jmp, mov, etc.
-- It is polymorphic in the representation of address references.
data Atom operation
  = AOp operation
  | ALabel LabelText
  | AData BS.ByteString
  deriving (Show, Eq, Generic)

instance ToBS operation => ToBS (Atom operation) where
  asmToBin (AOp op)   = asmToBin op
  asmToBin (ALabel _) = pure mempty
  asmToBin (AData bs) = pure bs

-- | Constant parameters for the assembler.
data Config address
  = Config
    { -- | The in-memory image base location
      acVirtualBaseAddress :: address
    }

-- | The state of the label scanning process
data StateLabelScan address
  = StateLabelScan
    { -- | Current offset in generated image file (from the beginning)
      aslsIAOffset :: address
      -- | Current in-memory offset relative to the image base address. This
      -- is needed because some output files (e.g. Portable Executable) must
      -- specify to the loader where values are to be stored in memory. This is
      -- initially 0 and it is often refered to as RVA (Relative Value Address)
      -- in Microsoft documentation.
    , aslsRelativeVAOffset :: address
      -- | Unique label name generator counter
      -- , asUID :: Integer
      -- | Encountered labels
    , aslsLabels :: Map.Map LabelText (AddressInfo address)
    }

-- | The state of the assembler, i.e. both inputs and outputs.
data StateReferenceSolve op address
  = StateReferenceSolve
    { asrsAtoms :: Seq.Seq (Atom (op (SolvedReference address)))
    , asrsRelativeVAOffset :: address
    }
