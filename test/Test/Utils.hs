module Test.Utils (module Test.Utils) where

import qualified Data.ByteString.Lazy as BS

newtype BSByteShow = BSByteShow BS.ByteString deriving (Eq)

instance Show BSByteShow where
  show (BSByteShow bs) = show (BS.unpack bs)

