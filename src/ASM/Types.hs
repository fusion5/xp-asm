{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module ASM.Types (
    Address
  , AddressInfo (..)
  , AssemblyError (..)
  , Atom (..)
  , Config (..)
  , TraversableSized (..)
  , LabelText
  , Reference (..)
  , StateLabelScan (..)
  , StateReferenceSolve (..)
  , Encode (..)

) where

import Prelude hiding ((>>=), (>>), return, (++), pure, (<*>))
import Common hiding ((>>=), (>>), return)

import Container
import ASM.Types.Exception
import ASM.Types.Encode

import qualified Prelude

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Vector.Sized as Vec

type LabelText = Text.Text

-- Because we cannot derive the usual Haskell classes for multi-type-parameter GADTs, this helps to
-- map over sized containers that are polymorphic in an unsized type. More specifically, in this
-- use case:
--  - the container is the opcode set
--  - the unsized types are the address type, which is either textual label references, and
--    resolved literal addresses.
-- TraversableSized is used used to replace label references with resolved literal addresses.
-- The opcode datatype should implement this class outside of this library.
-- TODO: Would it suffice to define a functor-like instance in the application?
class TraversableSized (c :: Type -> Nat -> Type) where
  mapMSized :: forall (a :: Type) (b :: Type) (n :: Nat) m
            . Applicative m => (a -> m b) -> c a n -> m (c b n)

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
-- It can be, but not necessarily, polymorphic in the representation of address references.
data Atom (operation :: Nat -> Type) (n :: Nat) where
  Atom :: operation n -> Atom operation n
  Label :: LabelText  -> Atom operation 0

instance Encode operation => Encode (Atom operation) where
  encode (Atom op) = encode op
  encode (Label _) = Prelude.pure Vec.empty

-- | Constant parameters for the assembler.
data Config address
  = Config
    { -- | The in-memory image base location
      acVirtualBaseAddress :: address
    }

-- | The state of the label scanner
data StateLabelScan address (n :: Nat)
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
data StateReferenceSolve (op :: Type {- address -} -> Nat {- n -} -> Type) address n
  = StateReferenceSolve
    { asrsAtoms :: Container (Atom (op (Reference address))) n
    , asrsRelativeVAOffset :: address
    }
