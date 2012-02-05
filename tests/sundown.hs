
import Text.Sundown
import Text.Sundown.Renderers.Html
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import System.Environment (getArgs)
import Control.Monad (liftM)

main :: IO ()
main = do
  input <- liftM (!! 0) getArgs >>= BS.readFile
  putStrLn $ UTF8.toString $ renderHtml input allExtensions noHtmlModes Nothing
