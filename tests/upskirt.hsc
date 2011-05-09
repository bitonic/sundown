{-# Language ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types
import Foreign.C.String
import System (getArgs)
import Control.Monad (liftM)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS
  

#include "buffer.h"
#include "markdown.h"
#include "array.h"


data Buffer = Buffer { bufData  :: CString
                     , bufSize  :: CSize
                     , bufASize :: CSize
                     , bufUnit  :: CSize
                     , bufRef   :: CInt
                     }
data Renderer


instance Storable Buffer where
  sizeOf _ = (#size struct buf)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
    d  <- (#peek struct buf, data) ptr
    s  <- (#peek struct buf, size) ptr
    as <- (#peek struct buf, asize) ptr
    u  <- (#peek struct buf, unit) ptr
    r  <- (#peek struct buf, ref) ptr
    return $ Buffer d s as u r
  poke ptr (Buffer d s as u r) = do
    (#poke struct buf, data) ptr d
    (#poke struct buf, size) ptr s
    (#poke struct buf, asize) ptr as
    (#poke struct buf, unit) ptr u
    (#poke struct buf, ref) ptr r
  
instance Storable Renderer where
    sizeOf _ = (#size struct mkd_renderer)
    alignment _ = alignment (undefined :: Ptr ())
    peek _ = error "Renderer.peek is not implemented"
    poke _ _ = error "Renderer.poke is not implemented"


foreign import ccall "markdown.h ups_markdown"
  c_ups_markdown :: Ptr Buffer -> Ptr Buffer -> Ptr Renderer -> CUInt -> IO ()

foreign import ccall "buffer.h bufnew"
  c_bufnew :: CSize -> IO (Ptr Buffer)

foreign import ccall "buffer.h bufputs"
  c_bufputs :: Ptr Buffer -> CString -> IO ()

foreign import ccall "buffer.h bufgrow"
  c_bufgrow :: Ptr Buffer -> CSize -> IO CInt

foreign import ccall "buffer.h bufrelease"
  c_bufrelease :: Ptr Buffer -> IO ()

foreign import ccall "xhtml.h ups_xhtml_renderer"
  c_ups_xhtml_renderer :: Ptr Renderer -> CUInt -> IO ()

foreign import ccall "xhtml.h ups_free_renderer"
  c_ups_free_renderer :: Ptr Renderer -> IO ()


main :: IO ()
main =
  alloca $ \renderer ->
  BS.useAsCString (UTF8.fromString "(~R∊R∘.×R)/R←1↓⍳R") $ \input -> do
    -- Allocate buffers
    ob <- c_bufnew 64
    ib <- c_bufnew 1024
  
    -- Put the input content into the buffer
    c_bufputs ib input
  
    -- Do the markdown
    c_ups_xhtml_renderer renderer 0
    c_ups_markdown ob ib renderer 0xFF
    c_ups_free_renderer renderer
  
    -- Print the output
    Buffer {bufData = output} <- peek ob
    liftM UTF8.toString (BS.packCString output) >>= putStrLn
  
    -- Release the buffers
    c_bufrelease ib
    c_bufrelease ob
