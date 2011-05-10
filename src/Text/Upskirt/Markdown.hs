module Text.Upskirt.Markdown
       ( Extensions (..)
       , allExtensions
       , noExtensions
       ) where

import Text.Upskirt.Markdown.Foreign

-- | All 'Extensions' disabled
noExtensions :: Extensions
noExtensions = Extensions False False False False False False 

-- | All 'Extensions' enabled
allExtensions :: Extensions
allExtensions = Extensions True True True True True True
