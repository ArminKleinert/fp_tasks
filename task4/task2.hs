{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A2

-- Vorgegebene Funktion, die eine unendliche Liste von Primzahlen berechnet.
primzahlen :: [Integer]
primzahlen =  sieb [2..]
  where sieb (p:xs) = p:sieb[k | k<-xs, (mod k p)>0]

-- TODO Doks
weakGoldbachTriples :: Integer -> [(Integer,Integer,Integer)]
weakGoldbachTriples n | n <= 5 || even n = error "n must be >5 and not even."
                      | otherwise = ls
  where
    n1 = (fromIntegral n) - 2
    prms = take n1 primzahlen -- The first n primes
    ls = [(x,y,z) | x <- prms, y <- prms, z <- prms, x+y+z == n && x <= y && y <= z]

-- TODO Doks
wGTriplesUntil :: Integer -> Bool
wGTriplesUntil n = all (\x -> not (null x))
                       (map weakGoldbachTriples (filter validNum [6 .. n]))
  where validNum :: Integer -> Bool
        validNum n = (not (even n))



-- TODO Tests
test :: IO ()
test = putStrLn ""
