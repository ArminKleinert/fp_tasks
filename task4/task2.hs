{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A2

-- Vorgegebene Funktion, die eine unendliche Liste von Primzahlen berechnet.
primzahlen :: [Integer]
primzahlen =  sieb [2..]
  where sieb (p:xs) = p:sieb[k | k<-xs, (mod k p)>0]

-- Berechnet Goldbach Triples für eine gegebene Zahl.
-- Ein Fehler wird angezeigt, wenn die Zahl <= 5 oder gerade ist.
--   weakGoldbachTriples 19 => [(3,3,13),(3,5,11),(5,7,7)]
weakGoldbachTriples :: Integer -> [(Integer,Integer,Integer)]
weakGoldbachTriples n | n <= 5 || even n = error "n must be >5 and not even."
                      | otherwise = ls
  where
    n1 = fromIntegral n
    prms = take n1 primzahlen -- The first n primes
    ls = [(x,y,z) | x <- prms, y <- prms, z <- prms, x+y+z == n && x <= y && y <= z]

-- Kontrolliert, ob die Goldbach-Behauptung bis zur gegebenen Zahl gilt.
--   wGTriplesUntil 500 => True
wGTriplesUntil :: Integer -> Bool
wGTriplesUntil n = all (\x -> not (null x))
                       (map weakGoldbachTriples (filter validNum [6 .. n]))
  where validNum :: Integer -> Bool
        validNum n = (not (even n))


test :: IO ()
test = putStrLn ("weakGoldbachTriples..." ++
                 "\n19 => " ++ (show (weakGoldbachTriples 19)) ++
                 "\n21 => " ++ (show (weakGoldbachTriples 21)) ++
                 "\n23 => " ++ (show (weakGoldbachTriples 23)) ++
                 "\n51 => " ++ (show (weakGoldbachTriples 51)) ++
                 "\n\nwGTriplesUntil..." ++
                 "\n19 => " ++ (show (wGTriplesUntil 19)) ++
                 "\n101 => " ++ (show (wGTriplesUntil 101)) ++
                 "\n300 => " ++ (show (wGTriplesUntil 300)) ++
                 "\n501 => " ++ (show (wGTriplesUntil 501)))
