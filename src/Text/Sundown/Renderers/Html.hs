{-# Language ForeignFunctionInterface #-}

module Text.Sundown.Renderers.Html
       ( renderHtml
       , noHtmlModes
       , allHtmlModes
       , smartypants
       , HtmlRenderMode (..)
       ) where


import Foreign

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Text.Sundown.Markdown.Foreign
import Text.Sundown.Buffer.Foreign
import Text.Sundown.Renderers.Html.Foreign

-- | Parses a 'ByteString' containing the markdown, returns the Html
-- code.
renderHtml :: ByteString -> Extensions -> HtmlRenderMode -> ByteString
renderHtml input exts mode =
  unsafePerformIO $
  alloca $ \renderer ->
  alloca $ \options -> do
    -- Allocate buffers
    ob <- c_bufnew 64
    ib <- c_bufnew . fromIntegral . BS.length $ input
    
    -- Put the input content into the buffer
    c_bufputs ib input
    
    -- Do the markdown
    c_sdhtml_renderer renderer options mode
    c_sd_markdown ob ib exts renderer (castPtr options)
    
    -- Get the result
    Buffer {bufData = output} <- peek ob
    
    c_bufrelease ib
    c_bufrelease ob    
    
    return output

-- | All the 'HtmlRenderMode' disabled
noHtmlModes :: HtmlRenderMode
noHtmlModes = HtmlRenderMode False False False False False False False False False False

-- | All the 'HtmlRenderMode' enabled
allHtmlModes :: HtmlRenderMode
allHtmlModes = HtmlRenderMode True True True True True True True True True True

-- | Converts punctuation in Html entities,
-- <http://daringfireball.net/projects/smartypants/>
smartypants :: ByteString -> ByteString
smartypants input =
  unsafePerformIO $ do
    ob <- c_bufnew 64
    ib <- c_bufnew $ fromInteger . toInteger $ BS.length input
    
    c_bufputs ib input
    
    c_sdhtml_smartypants ob ib
    
    Buffer {bufData = output} <- peek ob
    
    c_bufrelease ib
    c_bufrelease ob
    
    return output