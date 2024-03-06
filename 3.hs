import Text.Regex.Applicative

data Phone = Phone Int Int Int deriving Show

phoneParser :: RE Char Phone
phoneParser = Phone <$> decimal <* hyphen <> decimal < hyphen <*> decimal
  where decimal = read <$> some (psym isDigit)
        hyphen = sym '-'

main :: IO ()
main = print $ "123-456-789" =~ phoneParser