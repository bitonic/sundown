{-# Language ForeignFunctionInterface #-}

module Text.Upskirt.Renderers.Xhtml
       ( renderHtml
       , XhtmlRenderMode (..)
       ) where

import Foreign
import Foreign.C.Types

import Data.ByteString (ByteString)

import System.IO.Unsafe

import Text.Upskirt.Buffer
import Text.Upskirt.Markdown
import Text.Upskirt.Flag


data XhtmlRenderMode = XhtmlRenderMode { xhtmlSkipHtml :: Bool
                                       , xhtmlSkipStyle :: Bool
                                       , xhtmlSkipImages :: Bool
                                       , xhtmlSkipLinks :: Bool
                                       , xhtmlExpandTabs :: Bool
                                       , xhtmlSafelink :: Bool
                                       , xhtmlToc :: Bool
                                       , xhtmlHardWrap :: Bool
                                       , xhtmlGithubBlockcode :: Bool
                                       }

instance Flag XhtmlRenderMode where
  flagIndexes mode = [ (0,  xhtmlSkipHtml mode)
                     , (1,  xhtmlSkipStyle mode)
                     , (2,  xhtmlSkipImages mode)
                     , (3,  xhtmlSkipLinks mode)
                     , (5,  xhtmlExpandTabs mode)
                     , (7,  xhtmlSafelink mode)
                     , (8,  xhtmlToc mode)
                     , (9,  xhtmlHardWrap mode)
                     , (10, xhtmlGithubBlockcode mode)
                     ]


c_ups_xhtml_renderer :: Ptr Renderer -> XhtmlRenderMode -> IO ()
c_ups_xhtml_renderer rndr mode = c_ups_xhtml_renderer' rndr (toCUInt mode)
foreign import ccall "xhtml.h ups_xhtml_renderer"
  c_ups_xhtml_renderer' :: Ptr Renderer -> CUInt -> IO ()

foreign import ccall "xhtml.h ups_toc_renderer"
  c_ups_toc_renderer :: Ptr Renderer -> IO ()

foreign import ccall "xhtml.h ups_free_renderer"
  c_ups_free_renderer :: Ptr Renderer -> IO ()

foreign import ccall "xhtml.h ups_xhtml_smartypants"
  c_ups_xhtml_smartypants :: Ptr Buffer -> Ptr Buffer -> IO ()

renderHtml :: ByteString -> Extensions -> XhtmlRenderMode -> ByteString
renderHtml input exts mode =
  unsafePerformIO $
  alloca $ \renderer -> do
    -- Allocate buffers
    ob <- c_bufnew 64
    ib <- c_bufnew 1024
    
    -- Put the input content into the buffer
    c_bufputs ib input
    
    -- Do the markdown
    c_ups_xhtml_renderer renderer mode
    c_ups_markdown ob ib renderer exts
    c_ups_free_renderer renderer
    
    -- Get the result
    Buffer {bufData = output} <- peek ob
    
    c_bufrelease ib
    c_bufrelease ob
    
    return output