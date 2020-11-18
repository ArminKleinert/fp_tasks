{-
Funktioonale Programmierung übung 1
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

import Data.Char (toUpper, toLower) -- In task 4 to adjust the casing

{- 
1.

a) rem runded in Richtung (-Infinity). (rem (-5) 2) bspw. ist -1, nicht 1. Das Ergebnis ist also nur bei positiven Zahlen wie erwartet.
rem 5 2      == 1
rem 4 2      == 0
rem (-5) 2   == -1
rem (-4) 2   == 0
rem (-111) 2 == -1

b) Die Funktion kann auf 4 Arten berichtigt werden:
-}

-- 1. Mache aus dem möglicherweise negativen Wert n einen positiven Wert:
ungerade1 :: Integer -> Bool
ungerade1 n = rem (abs n) 2 == 1

-- 2. Mache das Ergebnis der Berechnung mit rem positiv:
ungerade2 :: Integer -> Bool
ungerade2 n = abs (rem n 2) == 1

-- 3. Kontrolliere den Gegenfall ((rem n 2) /= 0)
ungerade3 :: Integer -> Bool
ungerade3 n = rem n 2 /= 0

-- 4. Nutze mod statt rem
ungerade4 :: Integer -> Bool
ungerade4 n = mod n 2 == 1

{-
2.
-}

type Center = (Double, Double)
type Radius = Double
type Circle = (Center, Radius)

area :: Circle -> Double
area (_, r) = pi * (r ** 2)

perimeter :: Circle -> Double
perimeter (_, r) = pi * (2 * r)

equal :: Circle -> Circle -> Bool
equal ((cx0, cy0), r0) ((cx1, cy1), r1) = cx0 == cx1 && cy0 == cy1 && r0 == r1

intersect :: Circle -> Circle -> Bool
intersect ((cx0, cy0), r0) ((cx1, cy1), r1) =
  let dist   = (cx0 - cx1) ** 2 + (cy0 - cy1) ** 2
      radSum = (r0 + r1) ** 2
  in dist < radSum

contain :: Circle -> Circle -> Bool
contain ((cx0, cy0), r0) ((cx1, cy1), r1) =
  let dist = (cx0 - cx1) ** 2 + (cy0 - cy1) ** 2
  in dist <= (abs (r0 - r1))

{-
3.
-}

rectangles :: (Int, Int, Int) -> Char
rectangles (x, y, size) =
  if ((x <= quartSize) || (x > (size - quartSize))) ||
     (((y-1) `mod` quartSize) < (quartSize `div` 2))
    then '.'
    else ' '
  where
    quartSize = size `div` 4

diags :: (Int, Int, Int) -> Char
diags (x, y, size) =
  if ((x `mod` quartSize) == (y `mod` quartSize))
    then ' '
    else '0'
  where
    quartSize = size `div` 4
    
diamonSubUpper :: Int -> Int -> Int -> Char
diamonSubUpper x y halfsize = 
  if (y - x) >= halfsize ||
     (y + x) <= halfsize ||
     (x - y) >= halfsize
    then ' '
    else '0'

diamonSubLower :: Int -> Int -> Int -> Char
diamonSubLower x y0 halfsize =
  let y = abs ((y0 `mod` halfsize) - halfsize)
  in
  if (y - x) >= halfsize ||
     (y + x) <= halfsize ||
     (x - y) >= halfsize ||
     (y0 `div` 2) == halfsize
    then ' '
    else '0'

diamon :: (Int, Int, Int) -> Char
diamon (x, y, size) =
  let halfsize = size `div` 2
  in if y <= halfsize
        then diamonSubUpper x y halfsize
        else diamonSubLower x y halfsize

flag :: (Int, Int, Int) -> Char
flag (x, y, size) = 'a'

circle :: (Int, Int, Int) -> Char
circle (x, y, size) = 'a'

{-
4.
-}

singleDigits :: Int -> [Char]
singleDigits i = words !! i
  where
    words = ["",  "ein", "zwei", "drei", "vier", "fünf", "sechs", "sieben", "acht", "neun"]

twoDigits :: Int -> [Char]
twoDigits i = words !! i
  where
    words = ["zehn", "elf", "zwölf", "dreizehn", "vierzehn", "fünfzehn", "sechszehn", "siebzehn", "achtzehn", "neunzehn"]

tensMultiple :: Int -> [Char]
tensMultiple i = words !! i
  where
    words = ["", "zehn", "zwanzig", "dreizig", "vierzig", "fünfzig", "sechszig", "siebzig", "achtzig", "neunzig"]

capitalized :: [Char] -> [Char]
capitalized (head:tail) = toUpper head : map toLower tail
capitalized []          = []

num2GermanWordElseCase :: Int -> [Char]
num2GermanWordElseCase n = (if (n `mod` 10) == 0
                            then ""
                            else ((num2GermanWordSub (n `mod` 10)) ++ "und"))
                           ++ (tensMultiple (n `div` 10))

num2GermanWordSub :: Int -> [Char]
num2GermanWordSub n | n < 10 = capitalized (singleDigits n)
                    | n < 20 = capitalized (twoDigits (n `mod` 10))
                    | otherwise = capitalized (num2GermanWordElseCase n)

num2GermanWord :: Integer -> [Char]
num2GermanWord n | n == 0 = "Null"
                 | n < 0  = "minus " ++ num2GermanWord (abs n)
                 | n == 1 = "Eins"
                 | otherwise = num2GermanWordSub (fromIntegral n)

--------------------------------------------------------------------------------

{-- Funktionale Programmierung, U1, 2020/2021 Author: M. Esponda --}

paintChars f size = putStrLn (genChars f size)

genChars :: ((Int, Int, Int) -> Char) -> Int -> [Char]
genChars f size = paint size (map f [(x,y,size) | y <- [1..size], x <- [1..size]])
                  where
                  paint 0  []     = []
                  paint 0 (c:cs)  = '\n' : (paint size (c:cs))
                  paint n (c:cs)  = c: (paint (n-1) cs)

{-- Funktionsbeispiele für die 3.Aufgabe des 1.Übungsblattes   --}

diag (x,y,size) = if (x==y) then 'O' else '.'

quad (x,y,size) = if (x>s && x<3*s && y>s && y<3*s) then ' ' else '+'
                  where
                  s = div size 4

gitter (x,y,size) = if k || p  then ' ' else '0'
                    where
                    k = (mod x space)==0
                    p = (mod y space)==0
                    space = div size 4

{- Testfunktionen -}

test_diag = paintChars diag 40
test_quad = paintChars quad 40
test_gitter = paintChars gitter 40









