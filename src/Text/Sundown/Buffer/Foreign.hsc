{-# Language ForeignFunctionInterface #-}

module Text.Sundown.Buffer.Foreign
    ( Buffer (..)
    , getBufferData
    , c_bufnew
    , c_bufputs
    , c_bufgrow
    , c_bufrelease
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
    c_bufnew :: CSize -> IO (Ptr Buffer)

c_bufputs :: Ptr Buffer -> ByteString -> IO ()
c_bufputs buf bs = BS.useAsCString bs $ c_bufputs' buf
foreign import ccall "buffer.h bufputs"
    c_bufputs' :: Ptr Buffer -> CString -> IO ()

foreign import ccall "buffer.h bufgrow"
    c_bufgrow :: Ptr Buffer -> CSize -> IO CInt

foreign import ccall "buffer.h bufrelease"
    c_bufrelease :: Ptr Buffer -> IO ()