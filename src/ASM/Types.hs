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
  | -- | In a calculated label difference value that gives the number of bytes
    -- between the "From" label and the "To" label, the result must be
    -- positive, therefore the "From" label must be defined before the "To"
    -- label.
    FromLabelAfterTo
  | ReferenceTypeNotSupportedInOpcode Text.Text
  | OpcodeToByteString AssemblyError
  | InternalError Text.Text
  deriving (Show, Eq)

instance Exception.Exception AssemblyError

newtype SomeExceptionWrap = SEW Exception.SomeException deriving (Show)

-- | Defining a manual instance because SomeException doesn't make it possible
-- to derive Eq. This is a solution to the problem of checking for Exceptions
-- in unit tests.
instance Eq SomeExceptionWrap where
  (SEW se1) == (SEW se2) = show se1 == show se2

type LabelText = Text.Text

-- TODO: consider Foreign.Storable, add the 'alignment' method
class ByteSized a where
  sizeof :: a -> Int.Int64

-- | Because the Binary class doesn't easily allow nice error handling.
class Encodable a where
  encode :: a -> Either AssemblyError (Seq.Seq Word8)

instance Encodable a => Encodable (Seq.Seq a) where
  encode as = join <$> mapM encode as

-- | Memory / program addresses have certain constraints
class (Num a, Ord a, Bounded a) => Address a where

data AddressInfo address
  = AddressInfo
  { -- | Image Address e.g. of a label (in-file address, offset from the
    -- beginning of the file)
    aiIA  :: address
  , -- | Relative Virtual Address e.g. of a label (in-memory address minus
    -- image base address)
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
  | -- | Image Address (in-file address, offset from the beginning of
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

-- | An Atom is either an operation (typically called opcode) or a label.
data Atom operation
  = -- | The operation type is a subset of the instruction set, e.g. for x86
    -- jmp, mov, etc. which can be polymorphic in the representation of
    -- address references
    AOp operation
  | -- | A Label helps to refer to the program point where it is included
    -- by name
    ALabel LabelText
  deriving (Show, Eq, Generic)

instance Encodable operation => Encodable (Atom operation) where
  encode (AOp op)   = encode op
  encode (ALabel _) = pure mempty

-- | Constant parameters for the assembler.
data Config address
  = Config
    { -- | The in-memory image base location
      acVirtualBaseAddress :: address
    }

-- | The label scanner traverses the program and builds a Map of labels it
-- encountered and their address information. This is its state
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
      -- | Encountered labels so far
    , aslsLabels :: Map.Map LabelText (AddressInfo address)
    }

-- | The reference solver uses the Map of labels and their address information
-- to solve references to labels. this is its state
data StateReferenceSolve op address
  = StateReferenceSolve
    { asrsAtoms :: Seq.Seq (Atom (op (Reference address)))
    , asrsRelativeVAOffset :: address
    }
