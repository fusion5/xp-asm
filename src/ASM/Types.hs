{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ASM.Types (
    Address
  , AddressInfo (..)
  , AssemblyError (..)
  , Atom (..)
  , ByteSized (..)
  , Config (..)
  , Container (..)
  , FoldCallback (..)
  , FunctorMSized (..)
  , LabelText
  , Reference (..)
  , SomeExceptionWrap (..)
  , StateLabelScan (..)
  , StateReferenceSolve (..)
  , Binary (..)
  , foldMContainer
) where

import Common

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Control.Exception as Exception
import qualified Data.Vector.Sized as Vec

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

-- Defining a manual instance because SomeException doesn't make it possible to derive
instance Eq SomeExceptionWrap where
  (SEW se1) == (SEW se2) = show se1 == show se2

type LabelText = Text.Text

-- TODO: consider Foreign.Storable, add the 'alignment' method
class ByteSized (a :: Nat -> Type) where
  sizeof :: (KnownNat n) => a n -> Natural

class Binary (opcode :: Nat -> Type) where
  encode :: opcode n -> Either AssemblyError (Vec.Vector n Word8)

-- | A list of sized elements. Its size is the sum of its elements.
data Container (operation :: Nat -> Type) (n :: Nat) where
    Nil  :: Container a 0
    Cons :: (KnownNat n1, KnownNat n2, n ~ n1 + n2)
         => operation n1
         -> Container operation n2
         -> Container operation (n1 + n2)

-- Needed because we need the forall qualifiers on the counts. Is there a simpler way?
newtype FoldCallback m state operation =
  FoldCallback
    (  forall n1 n2
    .  (KnownNat n1, KnownNat n2)
    => (state n1 -> operation n2 -> m (state (n2 + n1)))
    )

foldMContainer
  :: (Monad m)
  => FoldCallback m state operation
  -> state 0
  -> Container operation n
  -> m (state n)
foldMContainer _ e Nil = pure e
foldMContainer (FoldCallback f) e (Cons op container) = do
  x <- foldMContainer (FoldCallback f) e container
  f x op

-- Because we cannot derive Functor for multi-type-parameter GADT:
-- A functor for sized containers over an unsized component. In our
-- case the container is the opcode set and the unsized component is
-- the address type. This is used to replace label references with
-- actual addresses.
class FunctorMSized (c :: Type -> Nat -> Type) where
  mapMSized :: forall (a :: Type) (b :: Type) (n :: Nat) m
            . Applicative m => (a -> m b) -> c a n -> m (c b n)

instance Binary opcode => Binary (Container opcode) where
  encode Nil = pure Vec.empty
  encode (Cons el container) = (Vec.++) <$> encode el <*> encode container

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

instance Binary operation => Binary (Atom operation) where
  encode (Atom op) = encode op
  encode (Label _) = pure Vec.empty

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
