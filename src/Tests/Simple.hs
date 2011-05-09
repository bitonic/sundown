
import Text.Upskirt.Markdown
import Text.Upskirt.Renderers.Xhtml
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Control.Monad (liftM)
import System (getArgs)

main :: IO ()
main = do
  -- Using strings since in the end that's what i want to do
  input <- liftM (!! 0) getArgs >>= readFile
  putStrLn $ UTF8.toString (renderHtml (UTF8.fromString input) allExtensions (XhtmlRenderMode False False False False False False False False False))