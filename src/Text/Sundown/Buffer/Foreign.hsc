{-# Language ForeignFunctionInterface #-}
module Text.Sundown.Buffer.Foreign
    ( Buffer (..)
    , getBufferData
    , bufnew
    , bufputs
    , bufgrow
    , bufrelease
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Foreign
import Foreign.C.String
import Foreign.C.Types

#include "buffer.h"

data Buffer = Buffer
    { buf_data  :: CString
    , buf_size  :: CSize
    , buf_asize :: CSize
    , buf_unit  :: CSize
    }

instance Storable Buffer where
    sizeOf _    = #{size struct buf}
    alignment _ = alignment (undefined :: CInt)
    peek ptr    = Buffer <$> #{peek struct buf, data} ptr
                         <*> #{peek struct buf, size} ptr
                         <*> #{peek struct buf, asize} ptr
                         <*> #{peek struct buf, unit} ptr
    poke _ _    = error "Buffer.poke not implemented."

getBufferData :: Buffer -> IO ByteString
getBufferData Buffer {buf_data = d, buf_size = s}
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