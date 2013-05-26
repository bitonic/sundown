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
import qualified Data.ByteString.UTF8 as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as T

import Text.Sundown.Foreign
import Text.Sundown.Html.ByteString (noHtmlModes, allHtmlModes, HtmlRenderMode(..))
import qualified Text.Sundown.Html.ByteString as BS
import Text.Sundown.Html.Foreign

-- | Converts something to an UTF8 'ByteString'.
class ToBS   a where toBS   :: a -> ByteString
-- | Converts an UTF8 'ByteString' to something.
class FromBS a where fromBS :: ByteString -> a

instance ToBS   ByteString where toBS   = id
instance FromBS ByteString where fromBS = id

instance ToBS   [Char]     where toBS   = BS.fromString
instance FromBS [Char]     where fromBS = BS.toString

instance ToBS   Text       where toBS   = T.encodeUtf8
instance FromBS Text       where fromBS = T.decodeUtf8

-- | Parses markdown, returns the Html.
renderHtml :: (ToBS a, FromBS b)
           => a
           -> Extensions
           -> HtmlRenderMode
           -> Bool              -- ^ If true, smartypant the output
           -> Maybe Int
           -- ^ The maximum nesting of the HTML. If Nothing, a default value
           -- (16) will be used.
           -> b
renderHtml input exts mode sp maxNestingM =
    fromBS $ BS.renderHtml (toBS input) exts mode sp maxNestingM

smartypants :: (ToBS a, FromBS b) => a -> b
smartypants = fromBS . BS.smartypants . toBS

