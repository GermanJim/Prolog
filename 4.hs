import Text.Regex.PCRE.Light

main :: IO ()
main = do
  let regex = compile "^(\\d+)-(\\d+)-(\\d+)$" []
  print $ match regex "123-456-789" []