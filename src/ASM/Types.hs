{-# LANGUAGE DeriveGeneric #-}

module ASM.Types
  ( Address
  , Atom (..)
  , Config (..)
  , Encodable (..)
  , SomeExceptionWrap (..)
  , LabelText
  , PositionInfo (..)
  , Reference (..)
  , StateEncodeSolved (..)
  , StateLabelScan (..)
  , StateReferenceSolve (..)
  , module ASM.Types.Position
  , module ASM.Types.AssemblyError
  , insertLabel
  , updateLabels
  , updatePosition
  ) where

import Common

import ASM.Types.Position hiding (mkPos)
import ASM.Types.AssemblyError
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text


-- | A Label helps to refer by name to the program point where the label is
type LabelText = Text.Text

-- | Define the encoding of opcodes outside of the library.
-- | Why not use the Binary class? It doesn't easily allow nice error handling.
class Encodable op where
  labels
    :: StateLabelScan address
    -> op (Reference address) -- what to scan, without solved references
    -> Either AssemblyError (StateLabelScan address)
  encode
    :: Address address        -- concrete address, i.e. solved
    => PositionInfo           -- needed to compute e.g. relative jump offsets
    -> op (Reference address) -- what to encode, with solved references
    -> Either AssemblyError BS.ByteString
  -- TODO: Why can't we can get rid of this and use the size of encode, which
  -- would be lazy and wouldn't actually require computing the encoding?
  size :: op a -> Natural

-- | Memory / program addresses have certain constraints. Note that this
-- allows for negative numbers, maybe it's not ideal
class (Integral a, Ord a, Bounded a) => Address a where

data PositionInfo
  = PositionInfo
  { -- | Image Address e.g. of a label (in-file address, offset from the
    -- beginning of the file)
    piIA         :: Position
  , -- | Relative Virtual Address e.g. of a label (in-memory address minus
    -- image base address)
    piRelativeVA :: Position
  , -- | Virtual address e.g. of a label
    piVA         :: Position
  }

-- The type of references and solved references defined here must cover the
-- needs of all assemblers defined using ASM.
data Reference address
  = -- | Image Address (in-file address, offset from the beginning of
    -- the file) of label
    RefIA address
  | -- | Relative Virtual Address (in-memory address minus image base
    -- address) of label. In other words relative to the image base address.
    RefRelativeVA address
  | -- | Virtual Address (in-memory address) of label
    RefVA address
  deriving (Show)

-- | An Atom is either an operation (typically called opcode) or a label.
data Atom op
  = -- | The op type is a subset of the instruction set, e.g. for x86
    -- jmp, mov, etc. which can be polymorphic in the representation of
    -- address references
    AOp op
  |  -- | Emit as many zeroes as needed to reach a multiple given as parameter.
    -- Note that it doesn't alter the virtual addresses; for that, use AlignVA
    AAlignIA Natural
  -- | Aligns both VA and RVA (memory) to a specified alignment. Does not emit
  -- zeroes in the image; for that, use AAlignIA
  | AAlignVA Natural
  deriving (Show, Eq, Generic)

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
      asPosition :: PositionInfo
      -- | Encountered labels so far
    , aslsLabels :: Map.Map LabelText PositionInfo
    }

insertLabel
  :: LabelText
  -> PositionInfo
  -> Map.Map LabelText PositionInfo
  -> Either AssemblyError (Map.Map LabelText PositionInfo)
insertLabel label positionInfo m
  = case Map.lookup label m of
      Just _existingPosition -> Left $ ReferenceExists label
      Nothing -> pure $ Map.insert label positionInfo m

updateLabels
  :: (Map.Map LabelText PositionInfo
      -> Either AssemblyError (Map.Map LabelText PositionInfo))
  -> StateLabelScan address
  -> Either AssemblyError (StateLabelScan address)
updateLabels f s = do
  s' <- f (aslsLabels s)
  pure $ s { aslsLabels = s' }

updatePosition
  :: (PositionInfo -> PositionInfo)
  -> StateLabelScan address -> StateLabelScan address
updatePosition f s = s { asPosition = f (asPosition s) }

-- | The reference solver uses the Map of labels and their address information
-- to solve references to labels. this is its state
newtype StateReferenceSolve op address
  = StateReferenceSolve
    { asrsAtoms :: Seq.Seq (Atom (op (Reference address)))
    }

data StateEncodeSolved address
  = StateEncodeSolved
    { sesPosition :: PositionInfo
    , sesEncoded  :: BS.ByteString
    }
