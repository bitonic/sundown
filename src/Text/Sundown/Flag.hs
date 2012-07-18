module Text.Sundown.Flag
    ( Flag (..)
    , toCUInt
    ) where

import Data.Bits
import Foreign.C.Types

class Flag a where
   flagIndexes :: a -> [(CUInt, Bool)]

toCUInt :: Flag a => a -> CUInt
toCUInt = foldr (\(f, b) -> ((if b then f else 0) .|.)) 0 . flagIndexes
