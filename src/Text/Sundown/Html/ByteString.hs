{-# Language ForeignFunctionInterface #-}
{-|

Warning: you should not need to use this module, if you're storing your text
sanely. Use "Text.Sundown.Html.String" or "Text.Sundown.Html.Text" so that you
won't have to worry about text encoding.

If you really want to use 'ByteString's directly, make sure that they are UTF-8.

-}
module Text.Sundown.Html.ByteString
    ( renderHtml
    , smartypants
      -- * Markdown extensions         
    , Extensions (..)
    , allExtensions
    , noExtensions
      -- * Html render modes         
    , HtmlRenderMode (..)
    , noHtmlModes
    , allHtmlModes
    ) where

import Data.Maybe (fromMaybe)
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS

import Text.Sundown.Buffer.Foreign
import Text.Sundown.Foreign
import Text.Sundown.Html.Foreign

defaultMaxNesting :: Int
defaultMaxNesting = 16

-- | Parses a 'ByteString' containing the markdown, returns the Html code.
renderHtml :: ByteString
           -> Extensions
           -> HtmlRenderMode
           -> Bool              -- ^ If true, smartypant the output
           -> Maybe Int
           -- ^ The maximum nesting of the HTML. If Nothing, a default value
           -- (16) will be used.
           -> ByteString
{-# NOINLINE renderHtml #-}
renderHtml input exts mode sp maxNestingM =
    unsafePerformIO $
    alloca $ \callbacks ->
    alloca $ \options ->
    BS.unsafeUseAsCStringLen input $ \(ptr, len) -> do
        ob <- bufnew 64
        sdhtml_renderer callbacks options mode
        let maxNesting = fromIntegral $ fromMaybe defaultMaxNesting maxNestingM
        markdown <- sd_markdown_new exts maxNesting callbacks (castPtr options)
        sd_markdown_render ob ptr (fromIntegral len) markdown
        sd_markdown_free markdown
        res <- if sp then
                   do ob' <- bufnew 64
                      Buffer {buf_data = optr, buf_size = olen} <- peek ob
                      sdhtml_smartypants ob' optr olen
                      bufrelease ob
                      return ob'
               else return ob
        output <- peek res >>= getBufferData
        bufrelease res
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
smartypants :: ByteString -> ByteString
{-# NOINLINE smartypants #-}
smartypants input =
    unsafePerformIO $
    BS.unsafeUseAsCStringLen input $ \(ptr, len) -> do
        ob <- bufnew 64
        sdhtml_smartypants ob ptr (fromIntegral len)
        output <- peek ob >>= getBufferData
        bufrelease ob
        return output