{-
Funktionale Programmierung übung 1
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
-- Die wohl portabelste Lösung (viele Sprachen haben kein mod, sondern nur rem)
ungerade3 :: Integer -> Bool
ungerade3 n = rem n 2 /= 0

-- 4. Nutze mod statt rem
-- Die wohl bevorzugte Lösung
ungerade4 :: Integer -> Bool
ungerade4 n = mod n 2 == 1


{-
2.
-}

-- Datentypen vom Übungsblatt

type Center = (Double, Double)
type Radius = Double
type Circle = (Center, Radius)

-- Berechne Fläche eines Kreises
-- Könnte r*r nutzen, aber online wird meistens r**2 genutzt.
-- Beispiel:
--   area ((0, 0), 5)
area :: Circle -> Double
area (_, r) = pi * (r ** 2) 

-- Berechne Perimeter des Kreises
-- Beispiel:
--   perimeter ((0, 0), 5)
perimeter :: Circle -> Double
perimeter (_, r) = pi * 2 * r

-- Kontrolliere, ob die Koordinaten und Radi von zwei Kreisen gleich sind.
-- Beispiel:
--   equal ((0, 0), 5) ((0, 0), 5) -- True
--   equal ((0, 1), 5) ((0, 0), 5) -- False
equal :: Circle -> Circle -> Bool
equal ((cx0, cy0), r0) ((cx1, cy1), r1) = cx0 == cx1 && cy0 == cy1 && r0 == r1

-- Alternative Implementation für equal:
--equal :: Circle -> Circle -> Bool
--equal c1 c2 = c1 == c2

-- Kontrolliere, ob sich zwei Kreise überschneiden
-- False, wenn dich die Kreise berühren aber keine Überschneidene Fläche haben.
-- Beispiel:
--   intersect ((0, 0), 5) ((0, 0), 5) -- True
--   intersect ((0, 1), 5) ((0, 5), 1) -- True
--   intersect ((0, 0), 5) ((0, 10), 5) -- False
--   intersect ((0, 0), 5) ((0, 11), 5) -- False
intersect :: Circle -> Circle -> Bool
intersect ((x1, y1), r1) ((x2, y2), r2) = (r1+r2) > d
  where
    d = sqrt((x2-x1)**2 + (y2-y1)**2)

-- Kontrolliere, ob sich zwei Kreise überschneiden oder berühren
intersectOrTouch :: Circle -> Circle -> Bool
intersectOrTouch ((cx0, cy0), r0) ((cx1, cy1), r1) =
  let dist   = (cx0 - cx1) ** 2 + (cy0 - cy1) ** 2 -- Distance
      radSum = (r0 + r1) ** 2
  in dist <= radSum

-- Kontrolliere, ob ein Kreis einen anderen komplett umfängt
-- Wenn Kreis 1 gleich zu Kreis 2 ist, dann gilt das Ergebnis trotzdem als False.
contain :: Circle -> Circle -> Bool
contain ((x1, y1), r1) ((x2, y2), r2) = (d + r2) < r1
  where
    d = sqrt((x2-x1)**2 + (y2-y1)**2)

{-
-- Alternative Implementation.
-- Wenn Kreis 1 gleich zu Kreis 2 ist, wird True zurückgegeben
contain :: Circle -> Circle -> Bool
contain ((cx0, cy0), r0) ((cx1, cy1), r1) =
  let dist = (cx0 - cx1) ** 2 + (cy0 - cy1) ** 2 -- Distance
  in dist <= (abs (r0 - r1))
-}

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

--flag :: (Int, Int, Int) -> Char
--flag (x, y, size) = 'a'

--circle :: (Int, Int, Int) -> Char
--circle (x, y, size) = 'a'

{-
4.
-}

-- Helper for num2GermanWord (n < 10 && n /= 0)
-- Achtung! (1 => "ein", nicht "eins")
singleDigits :: Int -> [Char]
singleDigits i = words !! i
  where
    words = ["",  "ein", "zwei", "drei", "vier", "fünf", "sechs", "sieben", "acht", "neun"]

-- Helper for num2GermanWord (n >= 10 && n < 20)
twoDigits :: Int -> [Char]
twoDigits i = words !! i
  where
    words = ["zehn", "elf", "zwölf", "dreizehn", "vierzehn", "fünfzehn", "sechszehn", "siebzehn", "achtzehn", "neunzehn"]

-- Helper for num2GermanWord (multiples of 10)
tensMultiple :: Int -> [Char]
tensMultiple i = words !! i
  where
    words = ["", "zehn", "zwanzig", "dreizig", "vierzig", "fünfzig", "sechszig", "siebzig", "achtzig", "neunzig"]

-- Capitalize first letter of string.
capitalized :: [Char] -> [Char]
capitalized (head:tail) = toUpper head : map toLower tail
capitalized []          = []

-- Helper for num2GermanWordSub
-- Called if n>=20
-- if n%10/=0 <singledigit>und<tensmultiple>
-- if n%10/=0 <tensMultiple>
num2GermanWordElseCase :: Int -> [Char]
num2GermanWordElseCase n = (if (n `mod` 10) == 0
                            then ""
                            else ((num2GermanWordSub (n `mod` 10)) ++ "und"))
                           ++ (tensMultiple (n `div` 10))

-- Helper for num2GermanWord
-- Called if n>1
-- n is an int. Overflows are not a concern, since numbers need to have max 2 digits
num2GermanWordSub :: Int -> [Char]
num2GermanWordSub n | n < 10 = singleDigits n
                    | n < 20 = twoDigits (n `mod` 10)
                    | otherwise = num2GermanWordElseCase n

-- Turn integer (2 digits + potential sign) into german words
-- If n == 0 => "Null"
-- If n == 1 => "Eins"
-- If n <  0 => "minus <word>"
-- Otherwise call num2GermanWordSub
num2GermanWord :: Integer -> [Char]
num2GermanWord n | n == 0 = "Null"
                 | n < 0  = "minus " ++ num2GermanWord (abs n)
                 | n == 1 = "Eins" -- Special case because of the 's'
                 | otherwise = capitalized (num2GermanWordSub (fromIntegral n))

--------------------------------------------------------------------------------

-- Test Task 1

ungerade1Test :: Bool
ungerade1Test = 
  ungerade1 (-5)     == True &&
  ungerade1 (-4)     == False &&
  ungerade1 (-3)     == True &&
  ungerade1 (-2)     == False &&
  ungerade1 (-1)     == True &&
  ungerade1 0        == False &&
  ungerade1 1        == True &&
  ungerade1 2        == False &&
  ungerade1 3        == True &&
  ungerade1 4        == False &&
  ungerade1 5        == True &&
  ungerade1 6        == False &&
  ungerade1 7        == True &&
  ungerade1 8        == False &&
  ungerade1 9        == True &&
  ungerade1 9000     == False &&
  ungerade1 (-90001) == True

ungerade2Test :: Bool
ungerade2Test = 
  ungerade2 (-5)     == True &&
  ungerade2 (-4)     == False &&
  ungerade2 (-3)     == True &&
  ungerade2 (-2)     == False &&
  ungerade2 (-1)     == True &&
  ungerade2 0        == False &&
  ungerade2 1        == True &&
  ungerade2 2        == False &&
  ungerade2 3        == True &&
  ungerade2 4        == False &&
  ungerade2 5        == True &&
  ungerade2 6        == False &&
  ungerade2 7        == True &&
  ungerade2 8        == False &&
  ungerade2 9        == True &&
  ungerade2 9000     == False &&
  ungerade2 (-90001) == True

ungerade3Test :: Bool
ungerade3Test = 
  ungerade3 (-5)     == True &&
  ungerade3 (-4)     == False &&
  ungerade3 (-3)     == True &&
  ungerade3 (-2)     == False &&
  ungerade3 (-1)     == True &&
  ungerade3 0        == False &&
  ungerade3 1        == True &&
  ungerade3 2        == False &&
  ungerade3 3        == True &&
  ungerade3 4        == False &&
  ungerade3 5        == True &&
  ungerade3 6        == False &&
  ungerade3 7        == True &&
  ungerade3 8        == False &&
  ungerade3 9        == True &&
  ungerade3 9000     == False &&
  ungerade3 (-90001) == True

ungerade4Test :: Bool
ungerade4Test = 
  ungerade4 (-5)     == True &&
  ungerade4 (-4)     == False &&
  ungerade4 (-3)     == True &&
  ungerade4 (-2)     == False &&
  ungerade4 (-1)     == True &&
  ungerade4 0        == False &&
  ungerade4 1        == True &&
  ungerade4 2        == False &&
  ungerade4 3        == True &&
  ungerade4 4        == False &&
  ungerade4 5        == True &&
  ungerade4 6        == False &&
  ungerade4 7        == True &&
  ungerade4 8        == False &&
  ungerade4 9        == True &&
  ungerade4 9000     == False &&
  ungerade4 (-90001) == True

-- Test Task 2

areaTest :: Bool
areaTest =
  area ((0,0),1)    == pi &&
  area ((0,0),0.5)  == 0.7853981633974483 &&
  area ((5000,1),1) == pi && -- Center is ignored
  area ((0,0),5)    == 78.53981633974483 &&
  area ((0,0),0.1)  == pi/100 &&
  area ((0,0),0)    == 0

perimeterTest :: Bool
perimeterTest =
  perimeter ((0,0),1)     == pi*2 &&
  perimeter ((0,0),0.5)   == pi &&
  perimeter ((5000,1),1)  == pi * 2 && -- Center is ignored
  perimeter ((0,0),5)     == pi * 10 &&
  perimeter ((0,0),0.1)   == pi / 5 &&
  perimeter ((0,0),0)     == 0

equalTest :: Bool
equalTest =
   equal ((0, 0), 5) ((0, 0), 5)  == True &&
   equal ((0, 1), 5) ((0, 0), 5)  == False &&
   equal ((0, 0), 5) ((0, 0), 10) == False

intersectTest :: Bool
intersectTest =
   intersect ((0, 0), 5) ((0, 0), 5)  == True  && -- Equal circles 
   intersect ((0, 1), 5) ((0, 5), 1)  == True  &&
   intersect ((0, 0), 5) ((0, 10), 5) == False && -- Touching, but not intersecting
   intersect ((0, 0), 5) ((0, 11), 5) == False

containTest :: Bool
containTest =
   contain ((0, 0), 5) ((0, 0), 5)   == False && -- Equal circles 
   contain ((0, 1), 5) ((0, 3), 1)   == True  &&
   contain ((0, 0), 5) ((0, 10), 5)  == False &&
   contain ((0, 0), 5) ((0, 11), 5)  == False &&
   contain ((0, 0), 50) ((0, 11), 5) == True  &&
   contain ((0, 0), 0) ((0, 0), 0)   == False 

-- Test Task 3
{-
paintChars rectangles 24
paintChars rectangles 40
paintChars rectangles 41

paintChars diags 24
paintChars diags 40
paintChars diags 41

paintChars diamon 24
paintChars diamon 40
paintChars diamon 41
-}

-- Test Task 4

-- -99 to -90
num2GermanWordTest1 :: Bool
num2GermanWordTest1 =
  map num2GermanWord [-99 .. -90] ==
  ["minus Neunundneunzig","minus Achtundneunzig","minus Siebenundneunzig","minus Sechsundneunzig","minus F\252nfundneunzig","minus Vierundneunzig","minus Dreiundneunzig","minus Zweiundneunzig","minus Einundneunzig","minus Neunzig"]

-- -20 to -10
num2GermanWordTest2 :: Bool
num2GermanWordTest2 =
  map num2GermanWord [-20 .. -10] ==
  ["minus Zwanzig","minus Neunzehn","minus Achtzehn","minus Siebzehn","minus Sechszehn","minus F\252nfzehn","minus Vierzehn","minus Dreizehn","minus Zw\246lf","minus Elf","minus Zehn"]

-- 0 to 1
num2GermanWordTest3 :: Bool
num2GermanWordTest3 =
  num2GermanWord (-0) == "Null" &&
  num2GermanWord 0    == "Null" &&
  num2GermanWord 1    == "Eins"

-- 2 to 19
num2GermanWordTest4 :: Bool
num2GermanWordTest4 =
  map num2GermanWord [2 .. 19] == 
  ["Zwei","Drei","Vier","F\252nf","Sechs","Sieben","Acht","Neun","Zehn","Elf","Zw\246lf","Dreizehn","Vierzehn","F\252nfzehn","Sechszehn","Siebzehn","Achtzehn","Neunzehn"]

-- 90 to 99
num2GermanWordTest5 :: Bool
num2GermanWordTest5 =
  map num2GermanWord [90 .. 99] == 
  ["Neunzig","Einundneunzig","Zweiundneunzig","Dreiundneunzig","Vierundneunzig","F\252nfundneunzig","Sechsundneunzig","Siebenundneunzig","Achtundneunzig","Neunundneunzig"]

-- Combined tests

testAll :: Bool
testAll = ungerade1Test && ungerade2Test && ungerade3Test && ungerade4Test && 
          areaTest && perimeterTest && equalTest && intersectTest && containTest && 
          num2GermanWordTest1 && num2GermanWordTest2 && 
          num2GermanWordTest3 && num2GermanWordTest4 && num2GermanWordTest5

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









