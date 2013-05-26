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

import Text.Sundown.Html hiding (renderHtml, smartypants)
import qualified Text.Sundown.Html as Sundown

-- | Parses a 'String' containing the markdown, returns the Html code.
renderHtml :: String
           -> Extensions
           -> HtmlRenderMode
           -> Bool              -- ^ If true, smartypant the output
           -> Maybe Int
           -- ^ The maximum nesting of the HTML. If Nothing, a default value
           -- (16) will be used.
           -> String
renderHtml = Sundown.renderHtml

-- | Converts punctuation in Html entities,
-- <http://daringfireball.net/projects/smartypants/>
smartypants :: String -> String
smartypants = Sundown.smartypants
