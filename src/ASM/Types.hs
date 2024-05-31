{-# LANGUAGE DeriveGeneric #-}

module ASM.Types where

import Common

import qualified Control.Exception as Exception
import qualified Data.ByteString.Lazy as BS
import qualified Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

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

newtype SomeExceptionWrap
  = ExceptionWrap Exception.SomeException deriving (Show)

-- | Defining a manual instance because SomeException doesn't make it possible
-- to derive Eq. This is a solution to the problem of checking for Exceptions
-- in unit tests.
instance Eq SomeExceptionWrap where
  (ExceptionWrap se1) == (ExceptionWrap se2) = show se1 == show se2

type LabelText = Text.Text

-- TODO: consider Foreign.Storable, add the 'alignment' method
-- TODO: The size might not always be Int64...
class ByteSized f where
  sizeIA  :: f a -> Int.Int64
  sizeRVA :: f a -> Int.Int64

-- | Define the encoding of opcodes outside of the library.
-- | Why not use the Binary class? It doesn't easily allow nice error handling.
class Encodable a where
  encode
    :: Address address
    => PositionInfo address -- needed to compute certain offsets
    -> a -- what to encode
    -> Either AssemblyError BS.ByteString

-- | Memory / program addresses have certain constraints
class (Integral a, Ord a, Bounded a) => Address a where

data PositionInfo address
  = PositionInfo
  { -- | Image Address e.g. of a label (in-file address, offset from the
    -- beginning of the file)
    piIA  :: address
  , -- | Relative Virtual Address e.g. of a label (in-memory address minus
    -- image base address)
    piRelativeVA :: address
  , -- | Virtual address e.g. of a label
    piVA :: address
  }

-- The type of references and solved references defined here must cover the
-- needs of all assemblers defined using ASM.
data Reference
  = -- | Image Address (in-file address, offset from the beginning of
    -- the file) of label
    RefIA LabelText
  | -- | Relative Virtual Address (in-memory address minus image base
    -- address) of label. In other words relative to the image base address.
    RefRelativeVA LabelText
  | -- | Virtual Address (in-memory address) of label
    RefVA LabelText
  deriving (Show)

data SolvedReference address
  = SolvedIA address
  | SolvedRelativeVA address
  | SolvedVA address

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

instance Encodable operation => Encodable (Atom operation)
  where
    encode pos (AOp op)   = encode pos op
    encode _   (ALabel _) = pure mempty

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
    { -- | The position information contains:
      --   - Current offset in generated image file (from the beginning)
      --   - Current in-memory offset relative to the image base address. This
      --     is needed because some output files (e.g. Portable Executable)
      --     must specify to the loader where values are to be stored in
      --     memory. This is initially 0 and it is often refered to as RVA
      --     (Relative Value Address) in Microsoft documentation.
      --   - Current memory address
      asPosition :: PositionInfo address
      -- | Encountered labels so far
    , aslsLabels :: Map.Map LabelText (PositionInfo address)
    }

-- | The reference solver uses the Map of labels and their address information
-- to solve references to labels. this is its state
data StateReferenceSolve op address
  = StateReferenceSolve
    { asrsAtoms :: Seq.Seq (Atom (op (SolvedReference address)))
    }

data StateEncodeSolved address
  = StateEncodeSolved
    { sesPosition :: PositionInfo address
    , sesEncoded  :: BS.ByteString
    }
