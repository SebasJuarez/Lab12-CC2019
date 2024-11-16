import Data.List (sortBy)
import Data.Ord (comparing)

-- Ejercicio 1
type Diccionario = [(String, String)]

dictionaries :: [Diccionario]
dictionaries =
  [ [("make", "Nokia"), ("model", "216"), ("color", "Black")]
  , [("make", "Apple"), ("model", "2"), ("color", "Silver")]
  , [("make", "Huawei"), ("model", "50"), ("color", "Gold")]
  , [("make", "Samsung"), ("model", "7"), ("color", "Blue")]
  ]

getValue :: String -> Diccionario -> String
getValue key dict = case lookup key dict of
                      Just value -> value
                      Nothing    -> ""

sortDictionaries :: String -> [Diccionario] -> [Diccionario]
sortDictionaries key = sortBy (comparing (getValue key))

runEjercicio1 :: IO ()
runEjercicio1 = do
    let sortedDictionaries = sortDictionaries "model" dictionaries
    print sortedDictionaries

-- Ejercicio 2:
potencias :: [Int] -> Int -> [Int]
potencias lista n = map (\x -> x ^ n) lista

runEjercicio2 :: IO ()
runEjercicio2 = do
    let lista = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    let n = 3
    let resultado = potencias lista n
    print resultado

-- Ejercicio 3
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map (\i -> map (!! i) x) [0..(length (head x) - 1)]

runEjercicio3 :: IO ()
runEjercicio3 = do
    let matriz = [[1, 2, 3, 1],
                  [4, 5, 6, 0],
                  [7, 8, 9, -1]]
    let transpuesta = transpose matriz
    print transpuesta

-- Ejercicio 4
eliminarElementos :: (Eq a) => [a] -> [a] -> [a]
eliminarElementos lista elementosBorrar = filter (\x -> not (x `elem` elementosBorrar)) lista

runEjercicio4 :: IO ()
runEjercicio4 = do
    let listaInicial = ["rojo", "verde", "azul", "amarillo", "gris", "blanco", "negro"]
    let elementosAEliminar = ["amarillo", "café", "blanco"]
    let listaResultante = eliminarElementos listaInicial elementosAEliminar
    print listaResultante

-- Menú principal
main :: IO ()
main = do
    putStrLn "Seleccione el ejercicio que desea ejecutar:"
    putStrLn "1. Ordenar lista de diccionarios por una clave específica"
    putStrLn "2. Calcular la potencia n-ésima de cada elemento en una lista"
    putStrLn "3. Calcular la matriz transpuesta"
    putStrLn "4. Eliminar elementos indicados de una lista"
    putStrLn "5. Salir"
    opcion <- getLine
    case opcion of
        "1" -> runEjercicio1 >> main
        "2" -> runEjercicio2 >> main
        "3" -> runEjercicio3 >> main
        "4" -> runEjercicio4 >> main
        "5" -> putStrLn "Saliendo del programa..."
        _   -> putStrLn "Opción no válida, inténtelo de nuevo." >> main
