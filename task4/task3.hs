{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A3

-- TODO Docs
diffList :: (Foldable t, Eq a) => [a] -> t a -> [a]
diffList [] _  = []
diffList (x:xs) ys | elem x ys = diffList xs ys
                   | otherwise = x : diffList xs ys

-- TODO Docs
firstNatNotIn :: [Integer] -> Integer
firstNatNotIn xs = head (diffList [0 .. (maximum xs) + 1] xs)

-- TODO Tests
test :: IO ()
test = putStrLn ""
