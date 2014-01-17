module Text.Sundown.Html.Text
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

import Data.Text (Text)

import Text.Sundown.Html hiding (renderHtml, smartypants)
import qualified Text.Sundown.Html as Sundown

-- | Parses a 'Text' containing the markdown, returns the Html code.
renderHtml :: Extensions
           -> HtmlRenderMode
           -> Bool              -- ^ If true, smartypant the output
           -> Maybe Int
           -- ^ The maximum nesting of the HTML. If Nothing, a default value
           -- (16) will be used.
           -> Text
           -> Text
renderHtml = Sundown.renderHtml

-- | Converts punctuation in Html entities,
-- <http://daringfireball.net/projects/smartypants/>
smartypants :: Text -> Text
smartypants = Sundown.smartypants
