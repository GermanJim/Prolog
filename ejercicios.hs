import Data.Char (toUpper)

-- 1) Descuento y IVA
aplicarDescuento :: Float -> Float -> Float
aplicarDescuento precio descuento = precio * (1 - descuento / 100)

aplicarIVA :: Float -> Float -> Float
aplicarIVA precio iva = precio * (1 + iva / 100)

aplicarDescuentoOIVA :: [(String, Float, Float)] -> (Float -> Float -> Float) -> Float
aplicarDescuentoOIVA cesta funcion = sum [funcion precio porcentaje | (producto, precio, porcentaje) <- cesta]

-- 2) Aplicar función a cada elemento de la lista
aplicarFuncionLista :: (a -> b) -> [a] -> [b]
aplicarFuncionLista _ [] = []
aplicarFuncionLista f (x:xs) = f x : aplicarFuncionLista f xs

-- 3) Contar palabras y longitud
contarPalabrasYLongitud :: String -> [(String, Int)]
contarPalabrasYLongitud frase = map (\x -> (x, length x)) (words frase)

-- 4) Asignaturas y calificaciones
asignaturasYCalificaciones :: [(String, Int)] -> [(String, String)]
asignaturasYCalificaciones notas = [(map toUpper asignatura, calificacion nota) | (asignatura, nota) <- notas]
  where
    calificacion n
      | n >= 95 = "Excelente"
      | n >= 85 = "Notable"
      | n >= 75 = "Bueno"
      | n >= 70 = "Suficiente"
      | otherwise = "Desempeño insuficiente"

-- 5) Módulo de un vector
moduloVector :: [Float] -> Float
moduloVector = sqrt . sum . map (^2)

-- 6) Valores atípicos
valoresAtipicos :: [Float] -> [Float]
valoresAtipicos xs = [x | x <- xs, abs((x - media) / desviacion) > 3]
  where
    n = fromIntegral $ length xs
    media = sum xs / n
    desviacion = sqrt (sum [(x - media) ^ 2 | x <- xs] / n)

main :: IO ()
main = do
    -- Ejemplo de uso de aplicarDescuentoOIVA
    let cestaCompra = [("Producto1", 100, 10), ("Producto2", 50, 5)]
    putStrLn "Precio final con descuento:"
    print $ aplicarDescuentoOIVA cestaCompra aplicarDescuento
    putStrLn "Precio final con IVA:"
    print $ aplicarDescuentoOIVA cestaCompra aplicarIVA

    -- Ejemplo de uso de aplicarFuncionLista
    putStrLn "Aplicar (*2) a cada elemento de la lista [1,2,3,4,5]:"
    print $ aplicarFuncionLista (*2) [1,2,3,4,5]

    -- Ejemplo de uso de contarPalabrasYLongitud
    putStrLn "Contar palabras y longitud de la frase 'Esta es una frase de ejemplo':"
    print $ contarPalabrasYLongitud "Esta es una frase de ejemplo"

    -- Ejemplo de uso de asignaturasYCalificaciones
    let notasAlumno = [("matemáticas", 85), ("física", 65)]
    putStrLn "Asignaturas y calificaciones:"
    print $ asignaturasYCalificaciones notasAlumno

    -- Ejemplo de uso de moduloVector
    putStrLn "Módulo del vector [3, 4]:"
    print $ moduloVector [3, 4]

    -- Ejemplo de uso de valoresAtipicos
    putStrLn "Valores atípicos de la lista [1, 2, 3, 4, 5, 100]:"
    print $ valoresAtipicos [1, 2, 3, 4, 5, 100]
