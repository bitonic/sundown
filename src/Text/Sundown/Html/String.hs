module Text.Sundown.Html.String
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

import qualified Data.ByteString.UTF8 as BS

import Text.Sundown
import Text.Sundown.Html.ByteString
    (noHtmlModes, allHtmlModes, HtmlRenderMode(..))
import qualified Text.Sundown.Html.ByteString as SundownBS

-- | Parses a 'ByteString' containing the markdown, returns the Html code.
renderHtml :: String
           -> Extensions
           -> HtmlRenderMode
           -- ^ The maximum nesting of the HTML. If Nothing, a default value
           -- (16) will be used.
           -> Maybe Int -> String
renderHtml input exts mode maxNestingM = 
    BS.toString $ SundownBS.renderHtml (BS.fromString input) exts mode
                                       maxNestingM

-- | Converts punctuation in Html entities,
-- <http://daringfireball.net/projects/smartypants/>
smartypants :: String -> String
smartypants = BS.toString . SundownBS.smartypants . BS.fromString
