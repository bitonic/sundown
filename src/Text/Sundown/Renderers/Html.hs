{-# Language ForeignFunctionInterface #-}

module Text.Sundown.Renderers.Html
       ( renderHtml
       , noHtmlModes
       , allHtmlModes
       , smartypants
       , HtmlRenderMode (..)
       ) where

import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)

import Text.Sundown.Buffer.Foreign
import Text.Sundown.Foreign
import Text.Sundown.Renderers.Html.Foreign

defaultMaxNesting :: Int
defaultMaxNesting = 16

-- | Parses a 'ByteString' containing the markdown, returns the Html code.
{-# NOINLINE renderHtml #-}
renderHtml :: ByteString
           -> Extensions
           -> HtmlRenderMode
           -> Maybe Int
           -- ^ The maximum nesting of the HTML. If Nothing, a default value
           -- (16) will be used.
           -> ByteString
renderHtml input exts mode maxNestingM =
    unsafePerformIO $
    alloca $ \callbacks ->
    alloca $ \options -> do
        -- Allocate buffers
        ob <- bufnew 64
        ib <- bufnew . fromIntegral . BS.length $ input

        -- Put the input content into the buffer
        bufputs ib input

        -- Do the markdown
        sdhtml_renderer callbacks options mode

        let maxNesting = fromIntegral $ fromMaybe defaultMaxNesting maxNestingM
        markdown <- sd_markdown_new exts maxNesting callbacks (castPtr options)

        Buffer {bufData = cs, bufSize = size} <- peek ib
        sd_markdown_render ob cs size markdown

        sd_markdown_free markdown

        -- Get the result
        output <- peek ob >>= getBufferData

        bufrelease ib
        bufrelease ob

        return output

-- | All the 'HtmlRenderMode' disabled
noHtmlModes :: HtmlRenderMode
noHtmlModes = HtmlRenderMode False False False False False False False False
                             False False

-- | All the 'HtmlRenderMode' enabled
allHtmlModes :: HtmlRenderMode
allHtmlModes = HtmlRenderMode True True True True True True True True True True

-- | Converts punctuation in Html entities,
-- <http://daringfireball.net/projects/smartypants/>
{-# NOINLINE smartypants #-}
smartypants :: ByteString -> ByteString
smartypants input =
    unsafePerformIO $ do
        ob <- bufnew 64
        ib <- bufnew $ fromInteger . toInteger $ BS.length input

        bufputs ib input

        Buffer {bufData = cs, bufSize = size} <- peek ib
        sdhtml_smartypants ob cs size

        output <- peek ob >>= getBufferData

        bufrelease ib
        bufrelease ob

        return output