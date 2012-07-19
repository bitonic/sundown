{-|

Bindings to the github fork of the sundown library - previously known as
upskirt: <https://github.com/tanoku/sundown>

To actually get output, you have to use one of the @Text.Sundown.Html@ modules:

    * "Text.Sundown.Html.String"

    * "Text.Sundown.Html.Text"

    * "Text.Sundown.Html.ByteString"

Example usage:

> import Text.Sundown.Renderers.Html.String
> import System (getArgs)
> import Control.Monad (liftM)
>
> main :: IO ()
> main = do
>   input <- liftM (!! 0) getArgs >>= readFile
>   putStrLn $ renderHtml input allExtensions noHtmlModes Nothing

-}
module Text.Sundown
     ( -- * Markdown extensions
       Extensions (..)
     , noExtensions
     , allExtensions
     ) where

import Text.Sundown.Foreign