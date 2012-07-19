
import Text.Sundown.Html.String
import Control.Monad (liftM)
import System.Environment (getArgs)

main :: IO ()
main = do
  input <- liftM (!! 0) getArgs >>= readFile
  putStrLn $ renderHtml input allExtensions noHtmlModes Nothing
