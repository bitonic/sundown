{-# Language ForeignFunctionInterface #-}

module Text.Sundown.Renderers.Html.Foreign
       ( HtmlRenderMode (..)
       , c_sdhtml_renderer
       , c_sdhtml_toc_renderer
       , c_sdhtml_smartypants
       ) where


import Data.ByteString.Unsafe (unsafeUseAsCString)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Text.Sundown.Buffer.Foreign
import Text.Sundown.Flag
import Text.Sundown.Markdown.Foreign

#include "html.h"

data HtmlRenderMode = HtmlRenderMode { htmlSkipHtml :: Bool -- ^ Drop in-line HTML tags from the
                                                           -- output
                                     , htmlSkipStyle :: Bool -- ^ Don't add any style tags to the
                                                            -- output
                                     , htmlSkipImages :: Bool -- ^ Don't include images in the output
                                     , htmlSkipLinks :: Bool -- ^ Don't include links in the output
                                     , htmlExpandTabs :: Bool
                                     , htmlSafelink :: Bool -- ^ Sanity check links for known URL
                                                           -- schemes
                                     , htmlToc :: Bool -- ^ Include a table of contents in the output
                                     , htmlHardWrap :: Bool
                                     , htmlUseXhtml :: Bool -- ^ Produce XHTML output instead of HTML
                                     }


instance Flag HtmlRenderMode where
  flagIndexes mode = [ (0, htmlSkipHtml mode)
                     , (1, htmlSkipStyle mode)
                     , (2, htmlSkipImages mode)
                     , (3, htmlSkipLinks mode)
                     , (4, htmlExpandTabs mode)
                     , (5, htmlSafelink mode)
                     , (6, htmlToc mode)
                     , (7, htmlHardWrap mode)
                     , (8, htmlUseXhtml mode)
                     ]

data HtmlRenderOptions

instance Storable HtmlRenderOptions where
  sizeOf _ = (#size struct html_renderopt)
  alignment _ = alignment (undefined :: Ptr ())
  peek _ = error "HtmlRenderopt.peek is not implemented"
  poke _ = error "HtmlRenderopt.poke is not implemented"

c_sdhtml_renderer :: Ptr Callbacks -> Ptr HtmlRenderOptions -> HtmlRenderMode -> IO ()
c_sdhtml_renderer rndr options mode = c_sdhtml_renderer' rndr options (toCUInt mode)
foreign import ccall "html.h sdhtml_renderer"
  c_sdhtml_renderer' :: Ptr Callbacks -> Ptr HtmlRenderOptions -> CUInt -> IO ()

foreign import ccall "html.h sdhtml_toc_renderer"
  c_sdhtml_toc_renderer :: Ptr Callbacks -> Ptr HtmlRenderOptions -> IO ()

foreign import ccall "html.h sdhtml_smartypants"
  c_sdhtml_smartypants :: Ptr Buffer -> CString -> CSize -> IO ()
