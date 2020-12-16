{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A1

a11 :: [Integer]
a11 = iterate (*(-1)) 1

a12 :: [Integer]
a12 = iterate ((+1).(*2)) 0

a13 :: [(Integer,Integer)]
a13 = iterate (\(x,y) -> (y, x+y)) (0,1)

test :: IO ()
test = putStrLn ("a11: " ++ (show (take 10 a11)) ++
                 "\na12: " ++ (show (take 10 a12)) ++
                 "\na13: " ++ (show (take 10 a13)))
