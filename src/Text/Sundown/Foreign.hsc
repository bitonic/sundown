{-# Language ForeignFunctionInterface #-}
module Text.Sundown.Foreign
    ( Mkd_autolink
    , mkda_not_autolink
    , mkda_normal
    , mkda_email
    , Extensions (..)
    , noExtensions
    , allExtensions
    , Callbacks (..)      
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

type Mkd_autolink = CInt

mkda_not_autolink, mkda_normal, mkda_email :: Mkd_autolink
mkda_not_autolink = #{const MKDA_NOT_AUTOLINK}
mkda_normal       = #{const MKDA_NORMAL}
mkda_email        = #{const MKDA_EMAIL}

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

data Callbacks = Callbacks
    { cb_blockcode
      :: FunPtr (Ptr Buffer -> Ptr Buffer -> Ptr Buffer -> Ptr () -> IO ())
    , cb_blockquote :: FunPtr (Ptr Buffer -> Ptr Buffer -> Ptr () -> IO ())
    , cb_blockhtml :: FunPtr (Ptr Buffer -> Ptr Buffer -> Ptr () -> IO ())
    , cb_header :: FunPtr (Ptr Buffer -> Ptr Buffer -> Ptr () -> IO ())
    , cb_hrule :: FunPtr (Ptr Buffer -> Ptr () -> IO ())
    , cb_list :: FunPtr (Ptr Buffer -> Ptr Buffer -> CInt -> Ptr () -> IO ())
    , cb_listitem
      :: FunPtr (Ptr Buffer -> Ptr Buffer -> CInt -> Ptr () -> IO ())
    , cb_paragraph :: FunPtr (Ptr Buffer -> Ptr Buffer -> Ptr () -> IO ())
    , cb_table
      :: FunPtr (Ptr Buffer -> Ptr Buffer -> Ptr Buffer -> Ptr () -> IO ())
    , cb_table_row :: FunPtr (Ptr Buffer -> Ptr Buffer -> Ptr () -> IO ())
    , cb_table_cell
      :: FunPtr (Ptr Buffer -> Ptr Buffer -> CInt -> Ptr () -> IO ())

    , cb_autolink
      :: FunPtr (Ptr Buffer -> Ptr Buffer -> Mkd_autolink -> Ptr () -> IO ())
    , cb_codespan :: FunPtr (Ptr Buffer -> Ptr Buffer -> Ptr () -> IO ())
    , cb_double_emphasis :: FunPtr (Ptr Buffer -> Ptr Buffer -> Ptr () -> IO ())
    , cb_emphasis :: FunPtr (Ptr Buffer -> Ptr Buffer -> Ptr () -> IO ())
    , cb_image
      :: FunPtr (Ptr Buffer -> Ptr Buffer -> Ptr Buffer -> Ptr Buffer -> Ptr () -> IO ())
    , cb_linebreak :: FunPtr (Ptr Buffer -> Ptr () -> IO ())
    , cb_link
      :: FunPtr (Ptr Buffer -> Ptr Buffer -> Ptr Buffer -> Ptr Buffer -> Ptr () -> IO ())
    , cb_raw_html_tag :: FunPtr (Ptr Buffer -> Ptr () -> IO ())
    , cb_triple_emphasis :: FunPtr (Ptr Buffer -> Ptr () -> IO ())
    , cb_strikethrough :: FunPtr (Ptr Buffer -> Ptr () -> IO ())
    , cb_superscript :: FunPtr (Ptr Buffer -> Ptr () -> IO ())

    , cb_entity :: FunPtr (Ptr Buffer -> Ptr () -> IO ())
    , cb_normal_text :: FunPtr (Ptr Buffer -> Ptr () -> IO ())
    , cb_doc_header :: FunPtr (Ptr Buffer -> Ptr () -> IO ())
    , cb_doc_footer :: FunPtr (Ptr Buffer -> Ptr () -> IO ())
    }

instance Storable Callbacks where
    sizeOf _    = #{size struct sd_callbacks}
    alignment _ = alignment (undefined :: Ptr ())
    peek _      = error "Callbacks.peek is not implemented"
    poke ptr b  =
        do #{poke struct sd_callbacks, blockcode} ptr (cb_blockcode b)
           #{poke struct sd_callbacks, blockquote} ptr (cb_blockquote b)
           #{poke struct sd_callbacks, blockhtml} ptr (cb_blockhtml b)
           #{poke struct sd_callbacks, header} ptr (cb_header b)
           #{poke struct sd_callbacks, hrule} ptr (cb_hrule b)
           #{poke struct sd_callbacks, list} ptr (cb_list b)
           #{poke struct sd_callbacks, listitem} ptr (cb_listitem b)
           #{poke struct sd_callbacks, paragraph} ptr (cb_paragraph b)
           #{poke struct sd_callbacks, table} ptr (cb_table b)
           #{poke struct sd_callbacks, table_row} ptr (cb_table_row b)
           #{poke struct sd_callbacks, table_cell} ptr (cb_table_cell b)
           #{poke struct sd_callbacks, autolink} ptr (cb_autolink b)
           #{poke struct sd_callbacks, codespan} ptr (cb_codespan b)
           #{poke struct sd_callbacks, double_emphasis} ptr (cb_double_emphasis b)
           #{poke struct sd_callbacks, emphasis} ptr (cb_emphasis b)
           #{poke struct sd_callbacks, image} ptr (cb_image b)
           #{poke struct sd_callbacks, linebreak} ptr (cb_linebreak b)
           #{poke struct sd_callbacks, link} ptr (cb_link b)
           #{poke struct sd_callbacks, raw_html_tag} ptr (cb_raw_html_tag b)
           #{poke struct sd_callbacks, triple_emphasis} ptr (cb_triple_emphasis b)
           #{poke struct sd_callbacks, strikethrough} ptr (cb_strikethrough b)
           #{poke struct sd_callbacks, superscript} ptr (cb_superscript b)
           #{poke struct sd_callbacks, entity} ptr (cb_entity b)
           #{poke struct sd_callbacks, normal_text} ptr (cb_normal_text b)
           #{poke struct sd_callbacks, doc_header} ptr (cb_doc_header b)
           #{poke struct sd_callbacks, doc_footer} ptr (cb_doc_footer b)

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
