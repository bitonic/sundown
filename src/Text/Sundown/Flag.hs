module Text.Sundown.Flag
       ( Flag (..)
       , toCUInt
       ) where


import Foreign.C.Types
import Data.Bits

class Flag a where
  flagIndexes :: a -> [(Int, Bool)]

toCUInt :: Flag a => a -> CUInt
toCUInt flag = foldl (\uint (ix, b) -> shiftL (fromIntegral . fromEnum $ b) ix .|. uint)
                     0 (flagIndexes flag)



