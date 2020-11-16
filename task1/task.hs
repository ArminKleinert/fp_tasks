
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

-- 4. Nutze mod statt remperimeter
ungerade4 :: Integer -> Bool
ungerade4 n = mod n 2 == 1

{-
2.
-}

type Center = (Double, Double)
type Radius = Double
type Circle = (Center, Radius)

area :: Circle -> Double
area c = 0.0

perimeter :: Circle -> Double
perimeter c = 0.0

-- Original Signatur: equal :: Circle -> Circle 
equal :: Circle -> Circle -> Bool
equal c0 c1 = True

intersect :: Circle -> Circle -> Bool
intersect c0 c1 = True

contain :: Circle -> Circle -> Bool
intersect c0 c1 = True

{-
3.
-}

rectangles :: (Int, Int, Int) -> Char
rectangles r = 'a'

diags :: (Int, Int, Int) -> Char
diags r = 'a'

diamon :: (Int, Int, Int) -> Char
diamon r = 'a'

flag :: (Int, Int, Int) -> Char
flag r = 'a'

circle :: (Int, Int, Int) -> Char
circle r = 'a'

{-
4.
-}

num2GermanWord :: Integer -> [Char]
num2GermanWord n = ""






