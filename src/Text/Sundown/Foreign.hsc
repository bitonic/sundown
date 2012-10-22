{-# Language EmptyDataDecls #-}
{-# Language ForeignFunctionInterface #-}
module Text.Sundown.Foreign
    ( Extensions (..)
    , noExtensions
    , allExtensions
    , Callbacks
    , sd_markdown_new
    , sd_markdown_render
    , sd_markdown_free
    ) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Text.Sundown.Buffer.Foreign
import Text.Sundown.Flag

#include "markdown.h"

-- | A set of switches to enable or disable markdown features.
data Extensions = Extensions
    { extNoIntraEmphasis :: Bool
      -- ^ Turn off underscores insode a word does designating emphasis.
    , extTables          :: Bool
    , extFencedCode      :: Bool
    -- ^ Turns on a non-indentation form of code-blocks, by blocking off a
    --   regionwith ~ or \`.
    , extAutolink        :: Bool
    -- ^ Turn things that look like URLs and email addresses into links
    , extStrikethrough   :: Bool
    -- ^ Surround text with `~` to designate it as struck through
    , extSpaceHeaders    :: Bool
    , extSuperscript     :: Bool
    , extLaxSpacing      :: Bool
    -- ^ Allow blocks inside of paragraphs, instead requireing tags to be on
    --   separate lines
    }

-- | All 'Extensions' disabled
noExtensions :: Extensions
noExtensions = Extensions False False False False False False False False

-- | All 'Extensions' enabled
allExtensions :: Extensions
allExtensions = Extensions True True True True True True True True

instance Flag Extensions where
    flagIndexes exts =
        [ (#{const MKDEXT_NO_INTRA_EMPHASIS}, extNoIntraEmphasis exts)
        , (#{const MKDEXT_TABLES},            extTables exts)
        , (#{const MKDEXT_FENCED_CODE},       extFencedCode exts)
        , (#{const MKDEXT_AUTOLINK},          extAutolink exts)
        , (#{const MKDEXT_STRIKETHROUGH},     extStrikethrough exts)
        , (#{const MKDEXT_SPACE_HEADERS},     extSpaceHeaders exts)
        , (#{const MKDEXT_SUPERSCRIPT},       extSuperscript exts)
        , (#{const MKDEXT_LAX_SPACING},       extLaxSpacing exts)
        ]

data Callbacks

instance Storable Callbacks where
    sizeOf _    = #{size struct sd_callbacks}
    alignment _ = alignment (undefined :: Ptr ())
    peek _      = error "Callbacks.peek is not implemented"
    poke _ _    = error "Callbacks.poke is not implemented"

data Markdown

instance Storable Markdown where
    sizeOf _    = error "Markdown.sizeOf is not implemented"
    alignment _ = alignment (undefined :: Ptr ())
    peek _      = error "Markdown.peek is not implemented"
    poke _      = error "Markdown.poke is not implemented"

sd_markdown_new
    :: Extensions -> CSize -> Ptr Callbacks -> Ptr () -> IO (Ptr Markdown)
sd_markdown_new extensions max_nesting callbacks opaque =
    sd_markdown_new' (toCUInt extensions) max_nesting callbacks opaque
foreign import ccall "markdown.h sd_markdown_new"
    sd_markdown_new'
        :: CUInt -> CSize -> Ptr Callbacks -> Ptr () -> IO (Ptr Markdown)

foreign import ccall "markdown.h sd_markdown_render"
    sd_markdown_render
        :: Ptr Buffer -> CString -> CSize -> Ptr Markdown -> IO ()

foreign import ccall "markdown.h sd_markdown_free"
    sd_markdown_free :: Ptr Markdown -> IO ()
