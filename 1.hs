import Text.Parsec
import Text.Parsec.String (Parser)

number :: Parser Int
number = read <$> many1 digit

numbers :: Parser [Int]
numbers = number sepBy char ','

main :: IO ()
main = do
  let result = parse numbers "" "123,456,789"
  case result of
    Left err -> print err
    Right nums -> print nums