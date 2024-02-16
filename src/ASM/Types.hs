{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

{-- LANGUAGE TypeSynonymInstances #-}
{-- LANGUAGE FlexibleInstances #-}
{-- LANGUAGE UndecidableInstances #-}

module ASM.Types where

import Common

import qualified Data.Text as Text
import qualified Data.Map as Map
-- import qualified Data.Sequence as Seq
-- import qualified Data.Int as Int
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
class ByteSized a where
  sizeof :: a -> Natural

class ToWord8s (opcode :: Nat -> Type) where
  -- TODO: Add Either
  safe :: forall n . opcode n -> Vec.Vector n Word8

data Container (operation :: Nat -> Type) (n :: Nat) where
    -- One :: a n -> Container a n
    -- Two :: a n1 -> a n2 -> Container a (n1 + n2)
    Nil  :: Container a 0
    Cons :: (KnownNat n1, KnownNat n2, n ~ n1 + n2)
         => operation n1
         -> Container operation n2
         -> Container operation (n1 + n2)

-- Needed because we need the forall qualifiers on the counts. Is there a simpler way?
newtype FoldCallback m state operation =
  FoldCallback
    (  forall n1 n2 . (KnownNat n1, KnownNat n2)
    => (state n1 -> operation n2 -> m (state (n2 + n1)))
    )

foldMNats
  :: (Monad m)
  => FoldCallback m state operation
  -> state 0
  -> Container operation n
  -> m (state n)
foldMNats _            e Nil = pure e
foldMNats (FoldCallback f) e (Cons op container) = do
  x <- foldMNats (FoldCallback f) e container
  f x op

-- Because we cannot derive Functor for our opcode GADT
class FunctorSized (c :: (Nat -> Type) -> Nat -> Type) where
  fmapSized :: forall (a :: Nat -> Type) (b :: Nat -> Type) (n :: Nat)
            . (a n -> b n) -> c a n -> c b n

-- Corresponds to a -> m b
newtype MapMFunction a b =
  MapMFunction (forall (n :: Nat) . KnownNat n => a n -> Either AssemblyError (b n))

-- The problem is we cannot sequenceM the eithers without losing the size information.
-- This should fix that.
newtype EitherSized (c :: Nat -> Type) (n :: Nat) =
  EitherSized (Either AssemblyError (c n))

-- sequenceA :: Applicative f => t (f a) -> f (t a)
sequenceANats
  :: forall
     (n :: Nat)
     (container :: (Nat -> Type) -> Nat -> Type)
     (a :: Nat -> Type)
  .  (FunctorSized container, KnownNat n)
  => container (EitherSized a) n
  -> EitherSized (container a) n
sequenceANats = mapMNats (MapMFunction eithersizedToEither) -- . eitherToEithersized

mapMNats
  :: forall
     (n :: Nat)
     (container :: (Nat -> Type) -> Nat -> Type)
     (a :: Nat -> Type)
     (b :: Nat -> Type)
  .  (FunctorSized container, KnownNat n)
  => MapMFunction a b -> container a n -> EitherSized (container b) n
mapMNats (MapMFunction f) = sequenceANats . fmapSized (eitherToEithersized . f)

eitherToEithersized
  :: forall (a :: Nat -> Type) (n :: Nat)
  .  Either AssemblyError (a n)
  -> EitherSized a n
eitherToEithersized = EitherSized

eithersizedToEither
  :: forall (a :: Nat -> Type) (n :: Nat)
  .  EitherSized a n
  -> Either AssemblyError (a n)
eithersizedToEither (EitherSized e) = e

{- do
  x :: b n <- f object
  pure undefined
  -}

instance ToWord8s opcode => ToWord8s (Container opcode) where
  safe Nil = Vec.empty
  safe (Cons el container) = safe el Vec.++ safe container

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

{-
data Atom (operation :: Nat -> Type) (n :: Nat)
  = Atom (operation n)
  | Label LabelText
  deriving (Show, Eq, Generic)

instance ToWord8s operation => ToWord8s (Atom operation) where
  safe (Atom op) = safe op
  safe (Label _) = Vec.empty --
  {- Produces:
      Expected: Vec.Vector n Word8
        Actual: Vec.Vector 0 Word8
      ‘n’ is a rigid type variable bound by
        the type signature for:
          safe :: forall (n :: Nat). Atom operation -> Vec.Vector n Word8
          -}
-}

-- | The operation type is a subset of the instruction set, e.g. for x86 jmp, mov, etc.
-- It can be, but not necessarily, polymorphic in the representation of address references.
data Atom (operation :: Nat -> Type) (n :: Nat) where
  Atom :: operation n -> Atom operation n
  Label :: LabelText   -> Atom operation 0

instance ToWord8s operation => ToWord8s (Atom operation) where
  safe (Atom op) = safe op
  safe (Label _) = Vec.empty

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
