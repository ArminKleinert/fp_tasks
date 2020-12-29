{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A5

-- Calculates all pythagoras triples of a natural number.
-- O(n^3)
pyth_tripels :: Int -> [(Int, Int, Int)]
pyth_tripels n = [(a,b,c) | c <- [1..n], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

-- Tests

test_help :: Int -> Int -> [Char]
test_help n c | c > n = ""
              | otherwise = "\n" ++ (show c) ++ ": " ++ (show (pyth_tripels c)) ++ (test_help n (c+1))

test :: IO ()
test = putStrLn ("pyth_tripels..." ++ test_help 25 1)







