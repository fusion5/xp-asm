{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Container
  ( Container
  , (++)
  , foldMContainer
  , FoldCallback (..)
  , empty
  , singleton
  )
where

import Common

import qualified Prelude
import qualified Data.Vector.Sized as Vec
import ASM.Types.Encode

-- | A list of sized elements. Its size is the sum of its elements.
data Container (operation :: Nat -> Type) (n :: Nat) where
  Nil  :: Container operation 0
  Leaf :: operation n -> Container operation n
  Tree :: (KnownNat n1, KnownNat n2, n ~ n1 + n2)
       => Container operation n1
       -> Container operation n2
       -> Container operation (n1 + n2)

(++)
  :: (KnownNat n1, KnownNat n2)
  => Container atom n1
  -> Container atom n2
  -> Container atom (n1 + n2)
Nil ++ c = c
c ++ Nil = c
l ++ r = Tree l r


-- Needed because we need the forall qualifiers on the counts. Is there a simpler way?
newtype FoldCallback m state operation =
  FoldCallback
    (  forall n1 n2
    .  (KnownNat n1, KnownNat n2)
    => (state n1 -> operation n2 -> m (state (n2 + n1)))
    )

foldMContainer
  :: (Monad m , KnownNat k, KnownNat n)
  => FoldCallback m state operation
  -> state k
  -> Container operation n
  -> m (state (k + n))
foldMContainer _ s Nil  = Prelude.pure s
foldMContainer (FoldCallback f) s  (Leaf op)    = f s op
foldMContainer (FoldCallback f) s0 (Tree c2 c1) = do
  s1 <- foldMContainer (FoldCallback f) s0 c1
  foldMContainer (FoldCallback f) s1 c2

instance Encode opcode => Encode (Container opcode) where
  encode Nil          = Prelude.pure Vec.empty
  encode (Leaf c)     = encode c
  encode (Tree c1 c2) = (Vec.++) Prelude.<$> encode c1 Prelude.<*> encode c2

empty :: Container op 0
empty = Nil

singleton :: op n -> Container op n
singleton = Leaf
