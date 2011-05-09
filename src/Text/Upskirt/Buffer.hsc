{-# Language ForeignFunctionInterface #-}

module Text.Upskirt.Buffer
       ( Buffer (..)
       , c_bufnew
       , c_bufputs
       , c_bufgrow
       , c_bufrelease
       ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString         (ByteString)
import qualified Data.ByteString as BS

#include "buffer.h"

data Buffer = Buffer { bufData  :: ByteString
                     , bufSize  :: Int
                     , bufASize :: Int
                     , bufUnit  :: Int
                     , bufRef   :: Int
                     }

instance Storable Buffer where
  sizeOf _ = (#size struct buf)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    d  <- (#peek struct buf, data) ptr
    dbs <- if d == nullPtr
           then return $ BS.pack [0]
           else BS.packCString d
    s  <- (#peek struct buf, size) ptr
    as <- (#peek struct buf, asize) ptr
    u  <- (#peek struct buf, unit) ptr
    r  <- (#peek struct buf, ref) ptr
    return $ Buffer dbs s as u r
  poke ptr (Buffer dbs s as u r) = BS.useAsCString dbs $ \d -> do
    (#poke struct buf, data) ptr d
    (#poke struct buf, size) ptr s
    (#poke struct buf, asize) ptr as
    (#poke struct buf, unit) ptr u
    (#poke struct buf, ref) ptr r

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
