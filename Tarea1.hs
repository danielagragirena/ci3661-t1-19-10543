--- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Daniela Gragirena
--  Carnet: 19-10543
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  Se ha utilizado chatGPT de forma exclusiva para dudas acerca del lenguaje
--  Haskell, sintaxis, funcionamiento de funciones, aclaraciones. 
--  No se ha utilizado para generacion de codigo.
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

import Data.Char

-- A1
esPalindromo :: String -> Bool
esPalindromo [] = True
esPalindromo cadena
    | limpiarCadena cadena == limpiarCadena (revertir cadena) = True
    | otherwise = False

limpiarCadena :: String -> String
limpiarCadena [] = []
limpiarCadena (x:xs)
    | isAlphaNum x = [toLower x] ++ limpiarCadena xs
    | isAlphaNum x == False = limpiarCadena xs

revertir :: String -> String
revertir [] = []
revertir (x:xs) = revertir xs ++ [x]

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
-- Mi solución usa una funcion auxiliar que recibe los mayores y menores
-- actuales como argumento y la lista que falta por revisar. De aquí los
-- va comparando simultaneamente con cada elemento de la lista por lo que
-- al recorrer toda la lista una vez ya tiene tanto el mayor como el menor.

mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs)= Just ((((aux xs) x) x))

aux :: Ord a => [a] -> a -> a -> (a, a)
aux [] y z = (y, z)
aux (x:xs) y z = ((((aux xs) (max x y)) (min x z)))

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [a, b,':', c, d]

    | not (isDigit a) || not (isDigit b) || not (isDigit c) || not (isDigit d) 
      = Left "formato inválido"
    | read [a] < 2 && read [b] < 10 && read [c] < 6 && read [d] < 10 
      = Right (read ([a] ++ [b]), read ([c] ++ [d]))
    | read [a] == 2 && read [b] < 4 && read [c] < 6 && read [d] < 10 
      = Right (read ([a] ++ [b]), read ([c] ++ [d]))
    | read [a] > 2 || (read [a] == 2 && read [b] > 3) 
    = Left "hora fuera de rango"
    | read [c] > 5 = Left "minutos fuera de rango"
    
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion hora1 hora2 = case leerHora(hora1) of
    Left mensaje1 -> Left mensaje1
    Right h1 -> case leerHora(hora2) of
        Left mensaje2 -> Left mensaje2
        Right h2 -> Right ((calcular h1) h2)

calcular :: (Int, Int) -> (Int, Int) -> Int
calcular (a, b) (c, d) 
    | a == c && b <= d = (d - b) 
    | a == c && b > d = 1440 + (d - b) 
    | a > c = (24 - a + c) * 60 + (d - b) 
    | otherwise = 60 * (c - a) + (d - b) 

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral _ [] = Nothing
promedioSobreUmbral a x = case (filter (> a) x) of
    [] -> Nothing
    y -> Just ((calcularPromedio y) (largo y))

-- Función obtenida de la clase 3
largo :: [Double] -> Double
largo = foldr (\_ n -> 1 + n) 0

calcularPromedio :: [Double] -> Double -> Double
calcularPromedio lista n = (foldr (+) 0 lista) / n

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo (n, m) = [ (x, y) | x <- [1..8], y <- [1..8], 
    ((x == n - 2 || x == n + 2) && (y == m - 1 || y == m + 1)) 
    || (x == n - 1 || x == m + 1) && (y == m - 2 || y == m + 2) ]

enDosSaltos :: (Int, Int) -> Int
enDosSaltos (a, b) = length (auxEDS dosSaltos)
    where 
        unSalto = movimientosCaballo (a, b)
        lista = map movimientosCaballo unSalto
        todosDosSaltos = foldr (++) [] lista
        dosSaltos = filter (\x -> not (elem x unSalto) && x /= (a, b)) todosDosSaltos

auxEDS :: [(Int, Int)] -> [(Int, Int)]
auxEDS a = foldr (\x acc -> 
    if elem x acc
    then acc
    else x : acc) [] a

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap x = foldr (\a b -> x a : b) []

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter cond = foldr filtro []
  where
    filtro x acc =
        if cond x
            then x : acc
            else acc

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl a b xs = foldr (\x f acc -> f (a acc x)) id xs b

