{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A6

{-
w = max( R/255, G/255, B/255)
C = (w - (R/255))/w
M = (w - (G/255))/w
Y = (w - (B/255))/w
K = 1 - w
-}

maxDouble :: Double -> Double -> Double -> Double
maxDouble d0 d1 d2 | d0 > d1 && d0 > d2 = d0
                   | d1 > d0 && d1 > d2 = d1
                   | otherwise = d2

selfOrZeroIfNaN :: Double -> Double
selfOrZeroIfNaN n = if n /= n then 0 else n

rgb2cmyk :: (Int,Int,Int) -> (Double,Double,Double,Double)
rgb2cmyk (r,g,b) = let rf = (fromIntegral r) / 255.0
                       gf = (fromIntegral g) / 255.0
                       bf = (fromIntegral b) / 255.0
                       w = maxDouble rf gf bf
                       c = selfOrZeroIfNaN ((w - rf) / w)
                       m = selfOrZeroIfNaN ((w - gf) / w)
                       y = selfOrZeroIfNaN ((w - bf) / w)
                       k = 1.0 - w
                   in (c,m,y,k)

-- Tests

test :: IO ()
test = putStrLn ("rgb2cmyk..." ++
                 "\n (0,0,0): " ++ (show (rgb2cmyk (0,0,0))) ++
                 "\n (255,255,255): " ++ (show (rgb2cmyk (255,255,255))))
