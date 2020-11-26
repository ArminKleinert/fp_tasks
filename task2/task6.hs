{-
Funktionale Programmierung Übung 2 
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- 6.

-- Recursively converts a number into binary.
-- No padding is prepended afterwards.
toBin :: Integer -> [Integer]
toBin 0 = [0]
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

-- Uses toBin and sums the 1s in the result.
binaryQSum :: Integer -> Integer
binaryQSum n = sum (toBin n)

----------

test :: IO ()
test = putStrLn ("binaryQSum 0: " ++ (show (binaryQSum 0)) ++
                 "\nbinaryQSum 15: " ++ (show (binaryQSum 15)) ++
                 "\nbinaryQSum 16: " ++ (show (binaryQSum 16)) ++
                 "\nbinaryQSum 2 ^ 32: " ++ (show (binaryQSum 2 ^ 32)) ++
                 "\nbinaryQSum (2 ^ 32 - 1): " ++ (show (binaryQSum (2 ^ 32 - 1))) ++
                 "\nbinaryQSum (2 ^ 128 - 1): " ++ (show (binaryQSum (2 ^ 128 - 1))))
