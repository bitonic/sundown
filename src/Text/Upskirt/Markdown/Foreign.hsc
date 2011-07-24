{-# Language ForeignFunctionInterface #-}

module Text.Upskirt.Markdown.Foreign
       ( Renderer
       , Extensions (..)
       , c_sd_markdown
       ) where

import Foreign
import Foreign.C.Types

import Text.Upskirt.Buffer.Foreign
import Text.Upskirt.Flag

#include "markdown.h"

data Renderer

-- | A set of switches to enable or disable markdown features.
data Extensions = Extensions { extNoIntraEmphasis :: Bool
                             , extTables          :: Bool
                             , extFencedCode      :: Bool
                             , extAutolink        :: Bool
                             , extStrikethrough   :: Bool
                             , extLaxHtmlBlocks   :: Bool
                             }

instance Flag Extensions where
  flagIndexes exts = [ (0, extNoIntraEmphasis exts)
                     , (1, extTables exts)
                     , (2, extFencedCode exts)
                     , (3, extAutolink exts)
                     , (4, extStrikethrough exts)
                     , (5, extLaxHtmlBlocks exts)
                     ]



instance Storable Renderer where
    sizeOf _ = (#size struct mkd_renderer)
    alignment _ = alignment (undefined :: Ptr ())
    peek _ = error "Renderer.peek is not implemented"
    poke _ _ = error "Renderer.poke is not implemented"

c_sd_markdown :: Ptr Buffer -> Ptr Buffer -> Ptr Renderer -> Extensions -> IO ()
c_sd_markdown ob ib rndr exts = c_sd_markdown' ob ib rndr (toCUInt exts)
foreign import ccall "markdown.h sd_markdown"
  c_sd_markdown' :: Ptr Buffer -> Ptr Buffer -> Ptr Renderer -> CUInt -> IO ()