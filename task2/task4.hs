{-
Funktionale Programmierung Übung 2 
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- 4. 

-- Returns a listlist of the first n hexagonal nuumbers.
-- Such a number can be found with the formula `2 * (n ^ 2) - n`.
-- Does not work with negative numbers.
hexagonalNums :: Integer -> [Integer]
hexagonalNums 0 = [0] -- Stop when 0 is reached.
hexagonalNums n = (hexagonalNums (n - 1)) ++ [((2 * n ^ 2) - n)]

--

test :: IO ()
test = putStrLn (
       "hexagonalNums 9: " ++ (show (hexagonalNums 9)) ++
       "\nhexagonalNums 0: " ++ (show (hexagonalNums 0)) ++
       "\nlength (hexagonalNums 500): " ++ (show (length (hexagonalNums 500))) ++
       "\nlength (hexagonalNums 5000): " ++ (show (length (hexagonalNums 5000))))

