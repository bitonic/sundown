{-# Language ForeignFunctionInterface #-}

module Text.Sundown.Buffer.Foreign
    ( Buffer (..)
    , getBufferData
    , bufnew
    , bufputs
    , bufgrow
    , bufrelease
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Foreign
import Foreign.C.String
import Foreign.C.Types

#include "buffer.h"

data Buffer = Buffer
    { bufData  :: CString
    , bufSize  :: CSize
    , bufASize :: CSize
    , bufUnit  :: CSize
    }

instance Storable Buffer where
    sizeOf _    = (#size struct buf)
    alignment _ = alignment (undefined :: CInt)
    peek ptr    = do d  <- (#peek struct buf, data) ptr
                     s  <- (#peek struct buf, size) ptr
                     as <- (#peek struct buf, asize) ptr
                     u  <- (#peek struct buf, unit) ptr
                     return Buffer { bufData  = d 
                                   , bufSize  = s 
                                   , bufASize = as 
                                   , bufUnit  = u
                                   }
    poke _ _    = error "Buffer.poke not implemented."

getBufferData :: Buffer -> IO ByteString
getBufferData Buffer {bufData = d, bufSize = s}
    | d == nullPtr = return $ BS.pack [0]
    | otherwise    = BS.packCStringLen (d, fromIntegral s)

foreign import ccall "buffer.h bufnew"
    bufnew :: CSize -> IO (Ptr Buffer)

bufputs :: Ptr Buffer -> ByteString -> IO ()
bufputs buf bs = BS.useAsCString bs $ bufputs' buf
foreign import ccall "buffer.h bufputs"
    bufputs' :: Ptr Buffer -> CString -> IO ()

foreign import ccall "buffer.h bufgrow"
    bufgrow :: Ptr Buffer -> CSize -> IO CInt

foreign import ccall "buffer.h bufrelease"
    bufrelease :: Ptr Buffer -> IO ()