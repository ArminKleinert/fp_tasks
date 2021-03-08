-- 1.
isDigit1 :: Char -> Bool
isDigit1 c = c >= '0' && c <= '9'

-- 2.
isDivisor :: Integer -> Integer -> Bool
isDivisor a b = (b `mod` a) == 0

isDivisor2 :: Integer -> Integer -> Bool
isDivisor2 a b = if (b `mod` a) == 0 then True else False

isDivisor3 :: Integer -> Integer -> Bool
isDivisor3 a b | ((b `mod` a) == 0) = True
               | otherwise = False

isDivisor4 :: Integer -> Integer -> Bool
isDivisor4 a b = let res = b `mod` a
                 in case res of
                         0 -> True
                         _ -> False

-- 3.

bogen2winkel :: Double -> Double
bogen2winkel b = (b / pi) * 180

winkel2bogen :: Double -> Double
winkel2bogen w = (w / 180) * pi

-- 4.

-- FIXME
gke :: Double -> Double -> Double -> Double -> Double
gke _x1 _x2 _y1 _y2 = c * (bogen2winkel (acos((sin x1) * (sin x2) + (cos x1) * (cos x2) * (cos ((-) y1 y2)))))
  where c    = 111.2225685
        x1   = winkel2bogen _x1
        x2   = winkel2bogen _x2
        y1   = winkel2bogen _y1
        y2   = winkel2bogen _y2

{-
c :: Double
c = 111.2225685
gke x1 y1 x2 y2 = c*bogen2winkel(acos(sin(winkel2bogen(x1))*sin(winkel2bogen(x2)) 
        + cos(winkel2bogen(x1))*cos(winkel2bogen(x2))*cos(winkel2bogen(y1)-winkel2bogen(y2))))
-}

-- 5.

pythagoras_tripel :: Integer -> Integer -> Integer -> Bool
pythagoras_tripel x y z = if (z > x && z > y)
                             then ((x ^ 2) + (y ^ 2)) == (z ^ 2)
                             else error "Z muss größer sein als x und y :)"
