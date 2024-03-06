import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (void)

type Parser = Parsec Void String

number :: Parser Int
number = read <$> some digitChar

numbers :: Parser [Int]
numbers = number sepBy char ','

main :: IO ()
main = do
  let result = parse numbers "" "123,456,789"
  case result of
    Left err -> putStr (errorBundlePretty err)
    Right nums -> print nums