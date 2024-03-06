import Text.Regex.Posix

main :: IO ()
main = do
  let result = "123-456-789" =~ "([0-9]+)-([0-9]+)-([0-9]+)" :: [[String]]
  print result