import Data.Char (toUpper)

-- Definición del tipo Inmueble
type Inmueble = (Int, Int, Int, Bool, Char)

-- 1) Calculadora científica
calcularFuncion :: (Float -> Float) -> Int -> IO ()
calcularFuncion funcion valor = do
    putStrLn "Valor | Resultado"
    putStrLn "------------------"
    mapM_ (\x -> putStrLn $ show x ++ " | " ++ show (funcion (fromIntegral x))) [1..valor]

-- 2) Filtrar elementos de una lista según una función booleana
filtrarConFuncion :: (a -> Bool) -> [a] -> [a]
filtrarConFuncion _ [] = []
filtrarConFuncion f (x:xs)
  | f x = x : filtrarConFuncion f xs
  | otherwise = filtrarConFuncion f xs

-- 3) Correspondencia de calificaciones
corresponderCalificaciones :: [Int] -> [String]
corresponderCalificaciones = map calificacion
  where
    calificacion n
      | n >= 95 = "Excelente"
      | n >= 85 = "Notable"
      | n >= 75 = "Bueno"
      | n >= 70 = "Suficiente"
      | otherwise = "Desempeño insuficiente"

-- 4) Asignaturas aprobadas
asignaturasAprobadas :: [(String, Int)] -> [(String, String)]
asignaturasAprobadas = map (\(asignatura, nota) -> (map toUpper asignatura, calificacion nota))
  where
    calificacion n
      | n >= 95 = "Excelente"
      | n >= 85 = "Notable"
      | n >= 75 = "Bueno"
      | n >= 70 = "Suficiente"
      | otherwise = "Desempeño insuficiente"

-- 5) Búsqueda de inmuebles según presupuesto
precioInmueble :: Inmueble -> Float
precioInmueble (anio, metros, habitaciones, garaje, zona)
  | zona == 'A' = fromIntegral (metros * 1000 + habitaciones * 5000 + if garaje then 15000 else 0) * (1 - fromIntegral (2024 - anio) / 100)
  | zona == 'B' = fromIntegral (metros * 1000 + habitaciones * 5000 + if garaje then 15000 else 0) * (1 - fromIntegral (2024 - anio) / 100) * 1.5
  | otherwise = 0

buscarInmuebles :: [Inmueble] -> Float -> [(Inmueble, Float)]
buscarInmuebles inmuebles presupuesto = map agregarPrecio inmueblesFiltrados
  where
    inmueblesFiltrados = filter (\inmueble -> precioInmueble inmueble <= presupuesto) inmuebles
    agregarPrecio inmueble = (inmueble, precioInmueble inmueble)

-- Ejemplo de uso para cada función
main :: IO ()
main = do
    putStrLn "1) Calculadora científica"
    putStrLn "Ingrese un valor para calcular las funciones:"
    valor <- readLn :: IO Int
    putStrLn "¿Qué función desea calcular? (seno, coseno, tangente, exponencial, logaritmo)"
    funcion <- getLine
    case funcion of
        "seno" -> calcularFuncion sin valor
        "coseno" -> calcularFuncion cos valor
        "tangente" -> calcularFuncion tan valor
        "exponencial" -> calcularFuncion exp valor
        "logaritmo" -> calcularFuncion log valor
        _ -> putStrLn "Función no válida"

    putStrLn "\n2) Filtrar elementos de una lista"
    putStrLn "Lista original: [1,2,3,4,5]"
    putStrLn "Elementos pares:"
    print $ filtrarConFuncion even [1,2,3,4,5]

    putStrLn "\n3) Correspondencia de calificaciones"
    putStrLn "Calificaciones: [90, 80, 70, 60, 50]"
    putStrLn "Correspondencia de calificaciones:"
    print $ corresponderCalificaciones [90, 80, 70, 60, 50]

    putStrLn "\n4) Asignaturas aprobadas"
    let notasAlumno = [("matemáticas", 85), ("física", 65)]
    putStrLn "Asignaturas y calificaciones:"
    print notasAlumno
    putStrLn "Asignaturas aprobadas:"
    print $ asignaturasAprobadas notasAlumno

    putStrLn "\n5) Búsqueda de inmuebles según presupuesto"
    let inmuebles = [ (2000, 100, 3, True, 'A')
                    , (2012, 60, 2, True, 'B')
                    , (1980, 120, 4, False, 'A')
                    , (2005, 75, 3, True, 'B')
                    , (2015, 90, 2, False, 'A')]
    putStrLn "Inmuebles:"
    print inmuebles
    putStrLn "Ingrese un presupuesto para buscar inmuebles:"
    presupuesto <- readLn :: IO Float
    putStrLn "Inmuebles encontrados:"
    print $ buscarInmuebles inmuebles presupuesto
