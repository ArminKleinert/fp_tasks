-- Aufgabe 1

-- a)
--Da rem, falls der Wert von n kleiner ist als 2 n wiedergibt.
--

-- b)

ungerade :: Integer -> Bool
ungerade n = mod n 2 == 1

-- Aufgabe 2

type Center = (Double, Double)
type Radius = Double
type Circle = (Center, Radius)

area :: Circle -> Double
area (c, r) = pi * (r)**2

perimeter :: Circle -> Double
perimeter (c, r) = 2*pi*r

equal :: Circle -> Circle -> Bool
equal ((x1, y1), r1) ((x2, y2), r2) = (x1==x2) && (y1==y2) && (r1==r2)

intersect :: Circle -> Circle -> Bool
intersect ((x1, y1), r1) ((x2, y2), r2) = (r1+r2) > d
                                          where
                                            d = sqrt((x2-x1)**2 + (y2-y1)**2)

contain :: Circle -> Circle -> Bool
contain ((x1, y1), r1) ((x2, y2), r2) = (d + r2) < r1
                            where
                              d = sqrt((x2-x1)**2 + (y2-y1)**2)

-- Aufgabe 4
{-
helperFunc :: Integer -> String
helperFunc x = case x of
                    0   -> "null"
                    1   -> "ein"
                    2   -> "zwei"
                    3   -> "drei"
                    4   -> "vier"
                    5   -> "fünf"
                    6   -> "sechs"
                    7   -> "sieben"
                    8   -> "acht"
                    9   -> "neun"
                    10  -> "zehn"
                    11  -> "elf"
                    12  -> "zwoelf"
                    20  -> "zwanzig"
                    30  -> "dreißig"
                    40  -> "vierzig"
                    50  -> "fuenfzig"
                    60  -> "sechzig"
                    70  -> "siebzig"
                    80  -> "achtzig"
                    90  -> "neunzig"
                    _        -> error "not a real number"

num2GermanWords :: Integer -> String
num2GermanWords x = if (n1 ) && (n1 /= 0)
                    then if (x < 0)
                         then ((++) "minus " ((++) (helperFunc n1) ((++) "und" (helperFunc n2))))
                         else ((++) (helperFunc m1) ((++) "und" (helperFunc m2)))
                    else if (x < 0)
                         then ((++) "minus " (helperFunc y))
                         else (helperFunc x)
                  where y = x*(-1)
                        n1 = (mod y 10)
                        n2 = y - n1
                        m1 = (mod x 10)
                        m2 = x - m1
                        -}


