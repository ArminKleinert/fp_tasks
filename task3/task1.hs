{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A1

-- Output 3 3 => Erwatet: 7625597484987 
--               Ist:     7625597484987
(°) :: Integer -> Integer -> Integer
(°) k 0 = 1
(°) k n = k ^ ((°) k (n-1))

--

test :: IO ()
test = putStrLn ("(3 ° 3) ist:     " ++ (show (3 ° 3)) ++
                 "\n        soll:    " ++ (show 7625597484987) ++
                 "\n(4°3)==(4°3):    " ++ (show ((4 ° 3)==(4 ° 3))) ++
                 "\n(2°4):           " ++ (show (2 ° 4)) ++
                 "\n(2 ^ 2 ^ 2 ^ 2): " ++ (show (2^2^2^2)) ++
                 "\n(1 ° 10000):     " ++ (show (1 ° 10000)))
