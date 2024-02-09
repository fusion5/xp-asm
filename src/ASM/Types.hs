module ASM.Types where

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

type LabelText = Text.Text

data AddressInfo address
  = AddressInfo
  { -- TODO: document
    apFile :: address
  , -- TODO: document
    apRVA  :: address
  }

data Reference
  = -- | Virtual Address (in-memory address) of label
    RLabelVA LabelText
  | -- | Relative Virtual Address (in-memory address minus image base
    -- address) of label
    RLabelRVA LabelText
  | -- | Image Address (in-file address, offset from the beginning of)
    -- the file) of label
    RLabelIA LabelText
  | -- | Offset to another label's Virtual Address from just after the
    -- reference virtual address. A signed value. Used for relative jumps
    RLabelRelativeVA LabelText
  | -- | The unsigned offset from the first label Image Address to the
    -- second one: the first label must be <= than the second one and
    -- the delta must fit the given Size (this is to be error-checked
    -- at run time). Helps to define the executable file headers.
    RLabelDifferenceIA
      { difiaFrom :: LabelText
      , difiaTo   :: LabelText
      }

-- | The opcode type is a subset of the instruction set, e.g. for x86 jmp, mov, etc.
-- It can be polymorphic.
data Atom opcode
  = AOpcode opcode
  | ALabel LabelText
  | AComment Text.Text
  deriving (Show, Eq)

-- | Constant parameters for the assembler.
data ASMConfig address
  = ASMConfig
    { -- | The in-memory image base location
      acImageBase :: address
    }

-- Parameters passed to the assembler
data ASMInput address
  = ASMInput
    { -- | Current offset in generated image file (from the beginning)
      aiImageOffset :: address
    , -- | Current in-memory offset relative to the image base address. This
      -- is needed because some output files (e.g. Portable Executable) must
      -- specify to the loader where values are to be stored in memory. This is
      -- initially 0 and it is often refered to as RVA (Relative Value Address)
      -- in Microsoft documentation.
      aiRVAOffset :: address
    }

-- | The state of the assembler, i.e. both inputs and outputs.
data ASMState address
  = ASMState
    { -- | Unique label name generator counter
      asUID :: Integer
    }

data ASMOutput address opcode
  = ASMOutput
    { -- | Encountered labels
      aoLabels :: Map.Map LabelText (AddressInfo address)
    , -- | Sequence of atoms generated
      aoAtoms :: Seq.Seq (Atom opcode)
    }
