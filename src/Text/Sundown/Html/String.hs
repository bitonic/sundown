module Text.Sundown.Html.String
    ( renderHtml
    , smartypants
      -- * Markdown extensions
    , Extensions (..)
    , allExtensions
    , noExtensions
      -- * Html render modes
    , HtmlRenderMode(..)
    , noHtmlModes
    , allHtmlModes
    ) where

import qualified Data.ByteString.UTF8 as BS

import Text.Sundown.Html.ByteString
    (noHtmlModes, allHtmlModes, HtmlRenderMode(..))
import qualified Text.Sundown.Html.ByteString as SundownBS
import Text.Sundown.Foreign

-- | Parses a 'ByteString' containing the markdown, returns the Html code.
renderHtml :: String
           -> Extensions
           -> HtmlRenderMode
           -> Bool              -- ^ If true, smartypant the output           
           -> Maybe Int
           -- ^ The maximum nesting of the HTML. If Nothing, a default value
           -- (16) will be used.
           -> String
renderHtml input exts mode sp maxNestingM = 
    BS.toString $ SundownBS.renderHtml (BS.fromString input) exts mode
                                       sp maxNestingM

-- | Converts punctuation in Html entities,
-- <http://daringfireball.net/projects/smartypants/>
smartypants :: String -> String
smartypants = BS.toString . SundownBS.smartypants . BS.fromString