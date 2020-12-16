-- A1
-- Funktion, die kontrolliert, ob ein Text einen anderen Text enthält

contains' :: String -> String -> String -> String -> Bool
contains' (c0:s0) (c1:s1) (c2:orig0) orig1
  | c0 == c1 = if (null s1) then True else contains' s0 s1 orig0 orig1
  | null s0 = False
  | otherwise = contains' orig0 orig1 orig0 orig1

-- Kontrolliert, ob s0 den String s1 enthält.
contains :: String -> String -> Bool
contains s0 s1 = contains' s0 s1 s0 s1

-- A2
-- Definieren Sie eine Funktion catalanNum, die bei Eingabe einer natürlichen Zahl n die entsprechende Catalan-Zahl berechnet. Verwenden Sie folgende Formel für die Berechnung:
-- C(0) = 1 und C(n+1) = (2 * C(n) * (2*n + 1)) / (n+2) für alle n >= 1

catalanNum 0 = 1
catalanNum n | n >= 1 = (2 * (catalanNum (n-1)) * (2 * (n-1) + 1)) / ((n-1)+2)
             | otherwise = error "n must be >= 1"
--
-- A3
-- Betrachten Sie folgende rekursive Funktion, die die maximale Anzahl der Teilflächen berechnet, die entstehen können, wenn ein Kreis mit n geraden Linien geteilt wird.

maxSurfaces :: Int -> Int
maxSurfaces 0 = 1
maxSurfaces n = maxSurfaces (n - 1) + n

-- a) Definieren Sie eine Funktion, die mit Hilfe einer endrekursiven Funktion genau die gleiche Berechnung realisiert.
maxSurfaces1' :: (Eq t, Num t) => t -> t -> t
maxSurfaces1' 0 acc = acc + 1
maxSurfaces1' n acc = maxSurfaces1' (n-1) (acc + n)

maxSurfaces1 :: Int -> Int
maxSurfaces1 n = maxSurfaces1' n 0

-- b) Welche Vorteile hat die endrekursive Funktion gegenüber der nicht endrekursiven Lösung?
-- Die Endrekursive version ist schneller, da der Haskell-Compiler sie leichter optimieren kann. Außerdem wird sie bei extrem großen inputs nicht abstürzen wie im folgenden Beispiel:
--  maxSurfaces 2000000000
--  *** Exception: stack overflow

-- A4
-- Definieren Sie eine rekursive, polymorphe Funktion mapUntil, die als Argumente eine Funktion f (f :: a -> b), eine Prädikat-Funktion  p  (p :: a->Bool) und eine Liste bekommt und, solange die Elemente der Liste das Prädikat nicht erfüllen, die Funktion fauf die Elemente der Liste anwendet und diese in der Ergebnisliste einfügt.
-- Anwendungsbeispiel:
-- mapUntil  (*3) (>5) [1,5,5,7,1,5]  =>  [3,15,15]

mapUntil :: (t -> a) -> (t -> Bool) -> [t] -> [a]
mapUntil f p [] = []
mapUntil f p (x:xs) | (p x) = []
                    | otherwise = (f x) : mapUntil f p xs
--
-- A5
-- Definieren Sie eine Funktion sumPowerTwo, die die Summe der Quadrate aller Zahlen zwischen 1 und n berechnet unter Verwendung der foldl und map-Funktionen

sumPowerTwo :: Integer -> Integer
sumPowerTwo n = foldl (+) 0 (map (\n -> n * n) [1..n])

-- A6
-- Definieren Sie eine Funktion pack, die eine Liste von Bits bekommt und diese in kompakter Form zurückgibt, indem sie nebeneinander stehende gleiche Bits mit einer Zahl zusammenfasst. Definieren Sie zuerst einen algebraischen Datentyp Bits dafür.    
-- Anwendungsbeispiele:
-- pack  [One, Zero, Zero, One, One, One, One]  => [1,2,4]
-- pack  [Zero, Zero, Zero, Zero, One, One, Zero, Zero, Zero, Zero] => [4,2,4]

data Bit = One | Zero deriving Eq
type Bits = [Bit]

pack' :: Bits -> Integer -> [Integer] -> [Integer]
pack' []         _ _   = [0]
pack' [b]        n acc = acc ++ [n + 1]
pack' (b0:b1:bs) n acc | b0 == b1 = pack' (b1:bs) (n+1) acc
                      | otherwise = pack' (b1:bs) 0 (acc ++ [n + 1])

pack :: Bits -> [Integer]
pack bs = pack' bs 0 []










