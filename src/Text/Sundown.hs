{-|

Bindings to the github fork of the sundown library - previously known as
upskirt: <https://github.com/tanoku/sundown>

To actually get output, you have to use one of the @Text.Sundown.Html@ modules.

Example usage:

> import Text.Sundown.Renderers.Html
> import System (getArgs)
> import Control.Monad (liftM)
>
> main :: IO ()
> main = do
>   input <- liftM (!! 0) getArgs >>= readFile
>   putStrLn $ renderHtml input allExtensions noHtmlModes Nothing

-}
module Text.Sundown
    ( Extensions (..)
    , allExtensions
    , noExtensions
    ) where

import Text.Sundown.Foreign

-- | All 'Extensions' disabled
noExtensions :: Extensions
noExtensions = Extensions False False False False False False False False

-- | All 'Extensions' enabled
allExtensions :: Extensions
allExtensions = Extensions True True True True True True True True
