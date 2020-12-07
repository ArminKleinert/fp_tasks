{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert, Anna Sophie Pipperr und Alexandra Wolfers
-}

toDecFrom :: [Integer] -> Integer -> Integer
toDecFrom [] b = if (b <= 0 || b > 10) then error "Basis out of range!" else 0 
toDecFrom (x:xs) b | b <= 0 || b > 10 = error "Basis out of range!"
                   | x >= b || x < 0 = error "At least one digit out of range!"
                   | otherwise = b^(length xs) * x + toDecFrom xs b

test :: IO ()
test = putStrLn ("toDecFrom..."        ++
                 "\n[1,1,1,1] 2:     " ++ (show (toDecFrom [1,1,1,1] 2)) ++
                 "\n[0,0,1,0] 2:     " ++ (show (toDecFrom [0,0,1,0] 2)) ++
                 "\n[1, 7] 8:        " ++ (show (toDecFrom [1, 7] 8)) ++
                 "\n[9,9] 10:        " ++ (show (toDecFrom [9,9] 10)))
--   toDecFrom []          => error
--   toDecFrom [1,1,1,1] 1 => error
