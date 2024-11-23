{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ASM.Types
  ( Address
  , Atom (..)
  , Config (..)
  , Encodable (..)
  , SomeExceptionWrap (..)
  , LabelText
  , Positions (..)
  , Reference (..)
  , StateEncodeSolved (..)
  , StateLabelScan (..)
  , StateAtomize (..)
  , module ASM.Types.Position
  , module ASM.Types.AssemblyError
  , insertLabel
  , updateLabels
  , updatePosition
  ) where

import Common
import Data.Sequence as Seq

import ASM.Types.Position hiding (mkPos)
import ASM.Types.AssemblyError
import Data.ByteString.Lazy as BS

import qualified Data.Map as Map
import qualified Data.Text as Text

-- | A Label helps to refer by name to the program point where the label is
type LabelText = Text.Text

-- | Define the encoding of opcodes outside of the library.
-- | Why not use the Binary class? It doesn't easily allow nice error handling.
class Encodable op where
  -- what to generate atoms for, without solved references
  atomize :: op -> Either AssemblyError (Seq Atom)

instance Encodable op => Encodable [op] where
  atomize [] = Right Seq.empty
  atomize (x:xs) = (<>) <$> atomize x <*> atomize xs

-- | Memory / program addresses have certain constraints. Note that this
-- allows for negative numbers, maybe it's not ideal
class (Integral a, Ord a, Bounded a) => Address a where
  -- toBS :: a -> ByteString
  -- addressSize :: a -> Natural

data Positions
  = Positions
  { -- | Image Address e.g. of a label (in-file address, offset from the
    -- beginning of the file)
    piIA         :: Position
  , -- | Relative Virtual Address e.g. of a label (in-memory address minus
    -- image base address)
    piRelativeVA :: Position
  , -- | Virtual address e.g. of a label
    piVA         :: Position
  } deriving (Show, Eq)

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
  deriving (Show, Eq, Generic)

data Atom
  = ALabel        LabelText
  | AAddrW8       Reference
  | AAddrW32      Reference
  | AAddrOffsetI8
    { -- Where does the offset start from relative to the current position:
      offsetFromDelta :: Integer
    , offsetTo        :: Reference
    }
  | ABytes        ByteString
  | AAlignIA      Natural
  | AAlignVA      Natural
  deriving (Show, Eq, Generic)

-- | Constant parameters for the assembler.
data Config
  = Config
    { -- | The in-memory image base location
      acVirtualBaseAddress :: Natural
    }

data StateAtomize address
  = StateAtomize
    { atPosition :: Positions
      -- | Atoms built so far
    , atAtoms:: Seq Atom
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
      asPosition :: Positions
      -- | Encountered labels so far
    , aslsLabels :: Map.Map LabelText Positions
    }

insertLabel
  :: LabelText
  -> Positions
  -> Map.Map LabelText Positions
  -> Either AssemblyError (Map.Map LabelText Positions)
insertLabel label positionInfo m
  = case Map.lookup label m of
      Just _existingPosition -> Left $ ReferenceExists label
      Nothing -> pure $ Map.insert label positionInfo m

updateLabels
  :: (Map.Map LabelText Positions
      -> Either AssemblyError (Map.Map LabelText Positions))
  -> StateLabelScan address
  -> Either AssemblyError (StateLabelScan address)
updateLabels f s = do
  s' <- f (aslsLabels s)
  pure $ s { aslsLabels = s' }

updatePosition
  :: (Positions -> Positions)
  -> StateLabelScan address -> StateLabelScan address
updatePosition f s = s { asPosition = f (asPosition s) }

data StateEncodeSolved address
  = StateEncodeSolved
    { sesPosition :: Positions
    , sesEncoded  :: BS.ByteString
    }
