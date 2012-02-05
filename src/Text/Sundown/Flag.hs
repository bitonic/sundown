module Text.Sundown.Flag
       ( Flag (..)
       , toCUInt
       ) where

import Foreign.C.Types
import Data.Bits

class Flag a where
  flagIndexes :: a -> [(CUInt, Bool)]

toCUInt :: Flag a => a -> CUInt
toCUInt flag = foldl (\uint (f, b) -> (if b then f else 0) .|. uint)
                     0 (flagIndexes flag)
