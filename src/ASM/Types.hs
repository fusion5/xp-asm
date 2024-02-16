{-# LANGUAGE DeriveGeneric #-}
module ASM.Types where

import Common

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Int as Int
import qualified Control.Exception as Exception

data AssemblyError
  = Arithmetic SomeExceptionWrap
  | ReferenceMissing LabelText
  | FromLabelAfterTo
  | ReferenceTypeNotSupportedInOpcode Text.Text
  | OpcodeToByteString AssemblyError
  | InternalError Text.Text
  deriving (Show, Eq)

instance Exception.Exception AssemblyError

newtype SomeExceptionWrap = SEW Exception.SomeException deriving (Show)

instance Eq SomeExceptionWrap where
  (SEW se1) == (SEW se2) = show se1 == show se2

-- Defining a manual instance because SomeException doesn't make it possible to derive

type LabelText = Text.Text

class ByteSized a where
  sizeof :: a -> Int.Int64

-- | The Binary class doesn't allow for nice error handling, therefore we use ExceptT
-- different one.
class ToWord8s a where
  toWord8s :: a -> Either AssemblyError (Seq.Seq Word8)

instance ToWord8s a => ToWord8s (Seq.Seq a) where
  toWord8s as = join <$> mapM toWord8s as

class (Num a, Ord a, Bounded a) => Address a where

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
data Reference address
  = -- | Virtual Address (in-memory address) of label
    RefVA address
  | -- | Relative Virtual Address (in-memory address minus image base
    -- address) of label
    RefRelativeVA address
  | -- | Image Address (in-file address, offset from the beginning of)
    -- the file) of label
    RefIA address
  | -- | Offset to another label's Virtual Address from the Virtual Address of
    -- the referrer begin location. A signed value. Used for relative jumps. If positive
    -- then the target is below. If negative then the target is above. For x64 note that
    -- you are interested in the offset relative to the referrer END location, so you will
    -- have to add the width of the reference.
    RefForwardOffsetVA address
  | RefForwardOffsetVASolved
      { refCurrentVA :: address -- From just after the reference
      , refTargetVA  :: address
      }
  | -- | The unsigned offset from the first label Image Address to the
    -- second one: the first label must be <= than the second one and
    -- the delta must fit the given Size (this is to be error-checked
    -- at run time). Helps to define executable file values.
    -- TODO: Couldn't we convert this to the delta between the current pos and a label?
    RefLabelDifferenceIA
      { difiaFrom :: address
      , difiaTo   :: address
      }
  deriving (Show)

-- | The operation type is a subset of the instruction set, e.g. for x86 jmp, mov, etc.
-- It is polymorphic in the representation of address references.
data Atom operation
  = AOp operation
  | ALabel LabelText
  deriving (Show, Eq, Generic)

instance ToWord8s operation => ToWord8s (Atom operation) where
  toWord8s (AOp op)   = toWord8s op
  toWord8s (ALabel _) = pure mempty

-- | Constant parameters for the assembler.
data Config address
  = Config
    { -- | The in-memory image base location
      acVirtualBaseAddress :: address
    }

-- | The state of the label scanner
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

-- | The state of the reference solver
data StateReferenceSolve op address
  = StateReferenceSolve
    { asrsAtoms :: Seq.Seq (Atom (op (Reference address)))
    , asrsRelativeVAOffset :: address
    }
