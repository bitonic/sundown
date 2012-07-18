{-# Language ForeignFunctionInterface #-}

module Text.Sundown.Renderers.Html.Foreign
    ( HtmlRenderMode (..)
    , c_sdhtml_renderer
    , c_sdhtml_toc_renderer
    , c_sdhtml_smartypants
    ) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Text.Sundown.Buffer.Foreign
import Text.Sundown.Flag
import Text.Sundown.Markdown.Foreign

#include "html.h"

data HtmlRenderMode = HtmlRenderMode
    { htmlSkipHtml   :: Bool -- ^ Drop in-line HTML tags from the output
    , htmlSkipStyle  :: Bool -- ^ Don't add any style tags to the output
    , htmlSkipImages :: Bool -- ^ Don't include images in the output
    , htmlSkipLinks  :: Bool -- ^ Don't include links in the output
    , htmlExpandTabs :: Bool
    , htmlSafelink   :: Bool -- ^ Sanity check links for known URL schemes
    , htmlToc        :: Bool -- ^ Include a table of contents in the output
    , htmlHardWrap   :: Bool
    , htmlUseXhtml   :: Bool -- ^ Produce XHTML output instead of HTML
    , htmlEscape     :: Bool
    }


instance Flag HtmlRenderMode where
    flagIndexes mode = [ (#{const HTML_SKIP_HTML},   htmlSkipHtml mode)
                       , (#{const HTML_SKIP_STYLE},  htmlSkipStyle mode)
                       , (#{const HTML_SKIP_IMAGES}, htmlSkipImages mode)
                       , (#{const HTML_SKIP_LINKS},  htmlSkipLinks mode)
                       , (#{const HTML_EXPAND_TABS}, htmlExpandTabs mode)
                       , (#{const HTML_SAFELINK},    htmlSafelink mode)
                       , (#{const HTML_TOC},         htmlToc mode)
                       , (#{const HTML_HARD_WRAP},   htmlHardWrap mode)
                       , (#{const HTML_USE_XHTML},   htmlUseXhtml mode)
                       , (#{const HTML_ESCAPE},      htmlEscape mode)
                       ]

data HtmlRenderOptions

instance Storable HtmlRenderOptions where
    sizeOf _    = (#size struct html_renderopt)
    alignment _ = alignment (undefined :: Ptr ())
    peek _      = error "HtmlRenderopt.peek is not implemented"
    poke _      = error "HtmlRenderopt.poke is not implemented"

c_sdhtml_renderer
    :: Ptr Callbacks -> Ptr HtmlRenderOptions -> HtmlRenderMode -> IO ()
c_sdhtml_renderer rndr options mode =
    c_sdhtml_renderer' rndr options (toCUInt mode)
foreign import ccall "html.h sdhtml_renderer"
    c_sdhtml_renderer'
        :: Ptr Callbacks -> Ptr HtmlRenderOptions -> CUInt -> IO ()

foreign import ccall "html.h sdhtml_toc_renderer"
    c_sdhtml_toc_renderer :: Ptr Callbacks -> Ptr HtmlRenderOptions -> IO ()

foreign import ccall "html.h sdhtml_smartypants"
    c_sdhtml_smartypants :: Ptr Buffer -> CString -> CSize -> IO ()
