{-|

Bindings to the github fork of the upskirt library:
<https://github.com/tanoku/upskirt>

Example usage:

> import Text.Upskirt
> import Text.Upskirt.Renderers.Html
> import qualified Data.ByteString as BS
> import qualified Data.ByteString.UTF8 as UTF8
> import System (getArgs)
> import Control.Monad (liftM)
>
> main :: IO ()
> main = do
>   input <- liftM (!! 0) getArgs >>= BS.readFile
>   putStrLn $ UTF8.toString $ renderHtml input allExtensions noHtmlModes

-}

module Text.Upskirt
       ( module Text.Upskirt.Markdown
       ) where

import Text.Upskirt.Markdown



