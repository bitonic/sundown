{-# Language ForeignFunctionInterface #-}

module Text.Upskirt.Renderers.Html.Foreign
       ( HtmlRenderMode (..)
       , c_sdhtml_renderer
       , c_sdhtml_toc_renderer
       , c_sdhtml_free_renderer
       , c_sdhtml_smartypants
       ) where


import Foreign
import Foreign.C.Types

import Text.Upskirt.Buffer.Foreign
import Text.Upskirt.Markdown.Foreign
import Text.Upskirt.Flag


data HtmlRenderMode = HtmlRenderMode { htmlSkipHtml :: Bool
                                     , htmlSkipStyle :: Bool
                                     , htmlSkipImages :: Bool
                                     , htmlSkipLinks :: Bool
                                     , htmlExpandTabs :: Bool
                                     , htmlSafelink :: Bool
                                     , htmlToc :: Bool
                                     , htmlHardWrap :: Bool
                                     , htmlGithubBlockcode :: Bool
                                     , htmlUseXhtml :: Bool
                                     }


instance Flag HtmlRenderMode where
  flagIndexes mode = [ (0,  htmlSkipHtml mode)
                     , (1,  htmlSkipStyle mode)
                     , (2,  htmlSkipImages mode)
                     , (3,  htmlSkipLinks mode)
                     , (5,  htmlExpandTabs mode)
                     , (7,  htmlSafelink mode)
                     , (8,  htmlToc mode)
                     , (9,  htmlHardWrap mode)
                     , (10, htmlGithubBlockcode mode)
                     , (11, htmlUseXhtml mode)
                     ]


c_sdhtml_renderer :: Ptr Renderer -> HtmlRenderMode -> IO ()
c_sdhtml_renderer rndr mode = c_sdhtml_renderer' rndr (toCUInt mode)
foreign import ccall "html.h sdhtml_renderer"
  c_sdhtml_renderer' :: Ptr Renderer -> CUInt -> IO ()

foreign import ccall "html.h sdhtml_toc_renderer"
  c_sdhtml_toc_renderer :: Ptr Renderer -> IO ()

foreign import ccall "html.h sdhtml_free_renderer"
  c_sdhtml_free_renderer :: Ptr Renderer -> IO ()

foreign import ccall "html.h sdhtml_smartypants"
  c_sdhtml_smartypants :: Ptr Buffer -> Ptr Buffer -> IO ()
