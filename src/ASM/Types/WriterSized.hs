{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module ASM.Types.WriterSized
  ( (>>=)
  , (>>)
  , WriterSized
  , execWriter
  , pure
  , runWriter
  , write
  ) where

-- Counterpart of Writer monad, but the produced output has a size.

import qualified Prelude

import Common hiding ((>>=), (>>))
import Container

data WriterSized (w :: Nat -> Type) (a :: Type) (n :: Nat) where
  WriterSized :: w n -> a -> WriterSized w a n

pure :: a -> WriterSized (Container operation) a 0
pure = WriterSized empty

(>>=)
  :: (KnownNat n1, KnownNat n2)
  => WriterSized (Container op) a n1
  -> (a -> WriterSized (Container op) b n2)
  -> WriterSized (Container op) b (n1 + n2)
(>>=) (WriterSized c1 a) f
  = case f a of
      WriterSized c2 b -> WriterSized (c1 ++ c2) b

(>>)
  :: (KnownNat n1, KnownNat n2)
  => WriterSized (Container op) a n1
  -> WriterSized (Container op) b n2
  -> WriterSized (Container op) b (n1 + n2)
(>>) (WriterSized c1 _) (WriterSized c2 b)
  = WriterSized (c1 ++ c2) b

write :: a n -> WriterSized (Container a) () n
write x = WriterSized (singleton x) ()

runWriter :: WriterSized (Container op) a n -> (a, Container op n)
runWriter (WriterSized container x) = (x, container)

execWriter :: WriterSized (Container op) a n -> Container op n
execWriter = Prelude.snd Prelude.. runWriter
