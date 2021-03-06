{-# LANGUAGE FlexibleInstances #-}
{-|
Module exposing a generic class to convert to/from UTF8 'ByteString's, and the
corresponding generic markdown functions.
-}
module Text.Sundown.Html
    ( renderHtml
    , smartypants
      -- * Generic ByteString conversion
    , ToBS(..)
    , FromBS(..)
      -- * Markdown extensions
    , Extensions (..)
    , allExtensions
    , noExtensions
      -- * Html render modes
    , HtmlRenderMode(..)
    , noHtmlModes
    , allHtmlModes
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Text.Sundown.Foreign
import Text.Sundown.Html.ByteString (noHtmlModes, allHtmlModes)
import qualified Text.Sundown.Html.ByteString as BS
import Text.Sundown.Html.Foreign

-- | Converts something to an UTF8 'ByteString'.
class ToBS   a where toBS   :: a -> ByteString
-- | Converts an UTF8 'ByteString' to something.
class FromBS a where fromBS :: ByteString -> a

instance ToBS   ByteString where toBS   = id
instance FromBS ByteString where fromBS = id

instance ToBS   [Char]     where toBS   = T.encodeUtf8 . T.pack
instance FromBS [Char]     where fromBS = T.unpack . T.decodeUtf8

instance ToBS   Text       where toBS   = T.encodeUtf8
instance FromBS Text       where fromBS = T.decodeUtf8

-- | Parses markdown, returns the Html.
renderHtml :: (ToBS a, FromBS b)
           => Extensions
           -> HtmlRenderMode
           -> Bool              -- ^ If true, smartypant the output
           -> Maybe Int
           -- ^ The maximum nesting of the HTML. If Nothing, a default value
           -- (16) will be used.
           -> a
           -> b
renderHtml exts mode sp maxNestingM input =
    fromBS $ BS.renderHtml exts mode sp maxNestingM $ toBS input

smartypants :: (ToBS a, FromBS b) => a -> b
smartypants = fromBS . BS.smartypants . toBS

