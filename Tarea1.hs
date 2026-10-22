--- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Daniela Gragirena
--  Carnet: 19-10543
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
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
duracion = undefined

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined
