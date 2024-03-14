import Data.List (intercalate)

-- Función para convertir un número a palabras en español
numToWords :: Int -> String
numToWords n
    | n == 0            = "cero"
    | n < 20            = unidades !! n
    | n == 20           = "veinte"
    | n < 30            = "veinti" ++ unidades !! (n `mod` 10)
    | n < 100           = if n `mod` 10 == 0
                            then decenas !! (n `div` 10)
                            else decenas !! (n `div` 10) ++ " y " ++ numToWords (n `mod` 10)
    | n < 200          = if n `mod` 100 == 0
                            then centenas !! (n `div` 100)
                            else centenas !! (n `div` 100) ++ "to " ++ numToWords (n `mod` 100)
    | n < 1000          = if n `mod` 100 == 0
                            then centenas !! (n `div` 100)
                            else centenas !! (n `div` 100) ++ " " ++ numToWords (n `mod` 100)
    | n < 2000          = if n `mod` 1000 == 0
                            then "mil"
                            else "mil " ++ numToWords (n `mod` 1000)
    | n < 1000000       = if n `mod` 1000 == 0
                            then numToWords (n `div` 1000) ++ " mil"
                            else numToWords (n `div` 1000) ++ " mil " ++ numToWords (n `mod` 1000)
    | otherwise         = "Fuera de rango"

unidades :: [String]
unidades = ["", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez",
            "once", "doce", "trece", "catorce", "quince", "dieciséis", "diecisiete", "dieciocho", "diecinueve"]

decenas :: [String]
decenas = ["", "diez", "veinte", "treinta", "cuarenta", "cincuenta", "sesenta", "setenta", "ochenta", "noventa"]

veintes :: [String]
veintes = ["", "veintiuno", "veintidos", "veintitres", "veinticuatro", "veinticinco", "veintiseis", "veintisiete", "veintiocho", "veintinueve"]

centenas :: [String]
centenas = ["", "cien", "doscientos", "trescientos", "cuatrocientos", "quinientos", "seiscientos", "setecientos", "ochocientos", "novecientos"]

-- Función para verificar si un número es primo
esPrimo :: Int -> Bool
esPrimo n
    | n <= 1    = False
    | otherwise = all (\x -> n `mod` x /= 0) [2..isqrt n]
    where isqrt = floor . sqrt . fromIntegral

-- Función principal que devuelve "FizzBuzz!" para números primos, y el número en palabras en español para otros casos
fizzBuzzOrNumToWords :: Int -> String
fizzBuzzOrNumToWords n
    | esPrimo n = "FizzBuzz!"
    | otherwise = numToWords n

-- Función para imprimir el resultado
imprimirResultado :: String -> IO ()
imprimirResultado resultado = putStrLn resultado

-- Pruebas automatizadas
grupo1 :: [(Int, String)]
grupo2 :: [(Int, String)]
grupo3 :: [(Int, String)]
grupo4 :: [(Int, String)]

grupo1 = [
   (0, "cero"), (1, "uno"), (2, "FizzBuzz!"), (3, "FizzBuzz!"), (4, "cuatro"), (5, "FizzBuzz!"), (6, "seis"), (7, "FizzBuzz!"),
   (8, "ocho"), (9, "nueve"), (10, "diez"), (11, "FizzBuzz!"), (12, "doce"), (13, "FizzBuzz!"), (14, "catorce"), 
   (15, "quince"), (20, "veinte"), (30, "treinta")]
grupo2 = [
   (16, "dieciséis"), (17, "FizzBuzz!"), (18, "dieciocho"), (20, "veinte"), (21, "veintiuno"), (24, "veinticuatro"), (26, "veintiseis"), (28, "veintiocho"),
   (29, "FizzBuzz!")]
grupo3 = [
   (30, "treinta"), (31, "FizzBuzz!"), (37, "FizzBuzz!"), (40, "cuarenta"), (50, "cincuenta"), (60, "sesenta"), (70, "setenta"), (80, "ochenta"),
   (90, "noventa"), (100, "cien")]
grupo4 = [
   (200, "doscientos"), (300, "trescientos"), (400, "cuatrocientos"), (500, "quinientos"), (600, "seiscientos"), (700, "setecientos"), (800, "ochocientos"),
   (900, "novecientos")]

-- Función para ejecutar las pruebas y verificar si son exitosas
ejecutarPruebas1 :: Bool
ejecutarPruebas2 :: Bool
ejecutarPruebas3 :: Bool
ejecutarPruebas4 :: Bool
ejecutarPruebas1 = and [fizzBuzzOrNumToWords n == expected | (n, expected) <- grupo1]
ejecutarPruebas2 = and [fizzBuzzOrNumToWords n == expected | (n, expected) <- grupo2]
ejecutarPruebas3 = and [fizzBuzzOrNumToWords n == expected | (n, expected) <- grupo3]
ejecutarPruebas4 = and [fizzBuzzOrNumToWords n == expected | (n, expected) <- grupo4]

-- Función para imprimir los resultados de las pruebas
imprimirResultadosPruebas :: IO ()
imprimirResultadosPruebas = do
    putStrLn "Pruebas automatizadas:"
    putStrLn $ "¿Pruebas del grupo 1 exitosas? " ++ if ejecutarPruebas1 then "Sí" else "No"
    putStrLn $ "¿Pruebas del grupo 2 exitosas? " ++ if ejecutarPruebas2 then "Sí" else "No"
    putStrLn $ "¿Pruebas del grupo 3 exitosas? " ++ if ejecutarPruebas3 then "Sí" else "No"
    putStrLn $ "¿Pruebas del grupo 4 exitosas? " ++ if ejecutarPruebas4 then "Sí" else "No"

-- Función principal para ejecutar el programa
main :: IO ()
main = do
    putStrLn "Ingrese un número entre 0 y 1000000:"
    numStr <- getLine
    let num = read numStr :: Int
    let resultado = fizzBuzzOrNumToWords num
    imprimirResultado resultado
    imprimirResultadosPruebas