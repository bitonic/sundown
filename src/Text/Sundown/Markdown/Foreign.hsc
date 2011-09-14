{-# Language ForeignFunctionInterface #-}

module Text.Sundown.Markdown.Foreign
       ( Callbacks
       , Extensions (..)
       , c_sd_markdown_new
       , c_sd_markdown_render
       , c_sd_markdown_free
       ) where

import Data.ByteString.Unsafe (unsafeUseAsCString)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Text.Sundown.Buffer.Foreign
import Text.Sundown.Flag

#include "markdown.h"

-- | A set of switches to enable or disable markdown features.
data Extensions = Extensions { extNoIntraEmphasis :: Bool -- ^ Turn off underscores insode a word
                                                         --   does designating emphasis.
                             , extTables :: Bool
                             , extFencedCode :: Bool -- ^ Turns on a non-indentation form of
                                                    -- code-blocks, by blocking off a regionwith ~
                                                    -- or \`.
                             , extAutolink :: Bool -- ^ Turn things that look like URLs and email
                                                  -- addresses into links
                             , extStrikethrough :: Bool -- ^ Surround text with `~` to designate it
                                                       -- as struck through
                             , extLaxHtmlBlocks :: Bool -- ^ Allow HTML markup inside of paragraphs,
                                                       -- instead requireing tags to be on separate
                                                       -- lines
                             , extSpaceHeaders :: Bool
                             , extSuperscript :: Bool
                             }

instance Flag Extensions where
  flagIndexes exts = [ (0, extNoIntraEmphasis exts)
                     , (1, extTables exts)
                     , (2, extFencedCode exts)
                     , (3, extAutolink exts)
                     , (4, extStrikethrough exts)
                     , (5, extLaxHtmlBlocks exts)
                     , (6, extSpaceHeaders exts)
                     , (7, extSuperScript exts)
                     ]


data Callbacks

instance Storable Callbacks where
  sizeOf _ = (#size struct sd_callbacks)
  alignment _ = alignment (undefined :: Ptr ())
  peek _ = error "Callbacks.peek is not implemented"
  poke _ _ = error "Callbacks.poke is not implemented"

data Markdown

instance Storable Markdown where
  sizeOf _ = error "Markdown.sizeOf is not implemented"
  alignment _ = alignment (undefined :: Ptr ())
  peek _ = error "Markdown.peek is not implemented"
  poke _ = error "Markdown.poke is not implemented"

c_sd_markdown_new :: Extensions -> CSize -> Ptr Callbacks -> Ptr () -> IO (Ptr Markdown)
c_sd_markdown_new extensions max_nesting callbacks opaque =
  c_sd_markdown_new' (toCUInt extensions) max_nesting callbacks opaque
foreign import ccall "markdown.h sd_markdown_new"
  c_sd_markdown_new' :: CUInt -> CSize -> Ptr Callbacks -> Ptr () -> IO (Ptr Markdown)

foreign import ccall "markdown.h sd_markdown_render"
  c_sd_markdown_render :: Ptr Buffer -> CString -> CSize -> Ptr Markdown -> IO ()

foreign import ccall "markdown.h sd_markdown_free"
  c_sd_markdown_free :: Ptr Markdown -> IO ()
