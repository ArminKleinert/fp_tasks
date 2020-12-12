{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A4

-- TODO Docs
toDecFrom' :: Int -> Int -> Int -> Int
toDecFrom' ac d base
          | d >= base = error "Invalid digit for given base."
          | d <  0    = error "Invalid digit for given base."
          | otherwise = base * ac + d

-- TODO Docs
toDecFrom :: [Int] -> Int -> Int
toDecFrom digs base | base <= 0 || base > 10 = error "Invalid base."
                    | otherwise = foldl (\ac n -> toDecFrom' ac n base) 0 digs


test :: IO ()
test = putStrLn ("toDecFrom..."        ++
                 "\n[1,1,1,1] 2:     " ++ (show (toDecFrom [1,1,1,1] 2)) ++
                 "\n[0,0,1,0] 2:     " ++ (show (toDecFrom [0,0,1,0] 2)) ++
                 "\n[1, 7] 8:        " ++ (show (toDecFrom [1, 7] 8)) ++
                 "\n[9,9] 10:        " ++ (show (toDecFrom [9,9] 10)))
