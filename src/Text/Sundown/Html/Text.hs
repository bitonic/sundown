module Text.Sundown.Html.Text
       ( renderHtml
       , noHtmlModes
       , allHtmlModes
       , smartypants
       , HtmlRenderMode(..)
         -- * Convenient re-exports
       , Extensions (..)
       , allExtensions
       , noExtensions
       ) where

import Data.Text (Text)
import Data.Text.Encoding

import Text.Sundown
import Text.Sundown.Html.ByteString
    (noHtmlModes, allHtmlModes, HtmlRenderMode(..))
import qualified Text.Sundown.Html.ByteString as SundownBS

-- | Parses a 'ByteString' containing the markdown, returns the Html code.
renderHtml :: Text
           -> Extensions
           -> HtmlRenderMode
           -> Bool              -- ^ If true, smartypants the output           
           -> Maybe Int
           -- ^ The maximum nesting of the HTML. If Nothing, a default value
           -- (16) will be used.
           -> Text
renderHtml input exts mode sp maxNestingM = 
    decodeUtf8 $ SundownBS.renderHtml (encodeUtf8 input) exts mode sp
                                      maxNestingM

-- | Converts punctuation in Html entities,
-- <http://daringfireball.net/projects/smartypants/>
smartypants :: Text -> Text
smartypants = decodeUtf8 . SundownBS.smartypants . encodeUtf8
