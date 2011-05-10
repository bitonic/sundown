{-# Language ForeignFunctionInterface #-}

module Text.Upskirt.Renderers.Xhtml.Foreign
       ( XhtmlRenderMode (..)
       , c_ups_xhtml_renderer
       , c_ups_toc_renderer
       , c_ups_free_renderer
       , c_ups_xhtml_smartypants
       ) where


import Foreign
import Foreign.C.Types

import Text.Upskirt.Buffer.Foreign
import Text.Upskirt.Markdown.Foreign
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
