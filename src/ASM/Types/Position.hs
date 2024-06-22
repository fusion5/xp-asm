module ASM.Types.Position
  ( Position
  , add
  , align
  , integralToPosition
  , mkPos -- for tests only, not really unsafe but it's an escape hatch
  , positionDowncast
  , sub
  , zero
  ) where

-- Do not import this module directly, except in unit tests.
-- Import ASM.Types instead.

import Common
import ASM.Types.AssemblyError
import qualified Numeric.Decimal.BoundedArithmetic as B
import qualified Data.Either.Extra as Either

-- | Position represents the offset in bytes at which the assembler is
-- currently located. Position is used instead of the polymorphic address
-- type because sometimes the address is a reference to a label, but Position
-- should always be known. Position has no upper bound, but addresses have an
-- upper bound. Addresses can be converted to positions using
-- integralToPosition.
-- Sometimes Position differences are also encoded as Position.
newtype Position = Position Natural
  deriving (Show, Eq)

newtype PositionDifference = PositionDifference Integer
  deriving (Show, Eq)

-- | Convert an address or some encoding size to Position.
-- Unfortunately i don't know how to represent non-negative bounded numbers at
-- the type level so this function can fail.
integralToPosition :: Integral a => a -> Either AssemblyError Position
integralToPosition n | n >= 0 = Right $ Position $ fromIntegral n
integralToPosition _          = Left NegativeToNatural

mkPos :: Natural -> Position
mkPos = Position

add :: Position -> Position -> Position
add (Position n1) (Position n2) = Position (n1 + n2)

-- Used mainly to compute relative jump offsets.
sub :: (Integral a, Bounded a) => Position -> Position -> Either AssemblyError a
sub (Position n1) (Position n2)
  = downcast (fromIntegral n1 - fromIntegral n2 :: Integer)

zero :: Position
zero = Position 0

-- Convert a position to an address, if possible. This is needed when
-- converting from a label position to an address. If the label position
-- exceeds the address space, it can error out.
positionDowncast
  :: (Integral a, Bounded a) => Position -> Either AssemblyError a
positionDowncast (Position n) = downcast n

downcast
  :: (Integral a, Integral b, Bounded b)
  => a -> Either AssemblyError b
downcast = Either.mapLeft (Arithmetic . ExceptionWrap) . B.fromIntegerBounded
         . fromIntegral

-- | How many bytes are needed to reach the next bigger multiple of n
align :: Natural -> Position -> Either AssemblyError Position
align 0 _            = Left AlignTo0
align n (Position p) = assert (n >= exceed) $
    Right $ Position $ if exceed == 0 then 0 else n - exceed
  where
    exceed :: Natural
    exceed = fromIntegral p `mod` n
