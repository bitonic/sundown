{-# Language ForeignFunctionInterface #-}

module Text.Upskirt.Renderers.Xhtml
       ( renderHtml
       , noXhtmlModes
       , allXhtmlModes
       , smartypants
       , XhtmlRenderMode (..)
       ) where


import Foreign

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Text.Upskirt.Markdown.Foreign
import Text.Upskirt.Buffer.Foreign
import Text.Upskirt.Renderers.Xhtml.Foreign


-- | Parses a 'ByteString' containing the markdown, returns the Xhtml
-- code.
renderHtml :: ByteString -> Extensions -> XhtmlRenderMode -> ByteString
renderHtml input exts mode =
  unsafePerformIO $
  alloca $ \renderer -> do
    -- Allocate buffers
    ob <- c_bufnew 64
    ib <- c_bufnew $ fromInteger . toInteger $ BS.length input
    
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


-- | All the 'XhtmlRenderMode' disabled
noXhtmlModes :: XhtmlRenderMode
noXhtmlModes = XhtmlRenderMode False False False False False False False False False

-- | All the 'XhtmlRenderMode' enabled
allXhtmlModes :: XhtmlRenderMode
allXhtmlModes = XhtmlRenderMode True True True True True True True True True

-- | Converts punctuation in Html entities,
-- <http://daringfireball.net/projects/smartypants/>
smartypants :: ByteString -> ByteString
smartypants input =
  unsafePerformIO $ do
    ob <- c_bufnew 64
    ib <- c_bufnew $ fromInteger . toInteger $ BS.length input
    
    c_bufputs ib input
    
    c_ups_xhtml_smartypants ob ib
    
    Buffer {bufData = output} <- peek ob
    
    c_bufrelease ib
    c_bufrelease ob
    
    return output