{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A3

-- Given 2 lists, removes all elements from the first list which are also in the 
-- second list.
--  diffList [0,1,2,3,4,5,6,7,8,9] [(-1), 0, 5]
--          => [1,2,3,4,6,7,8,9]
diffList :: (Foldable t, Eq a) => [a] -> t a -> [a]
diffList [] _  = []
diffList (x:xs) ys | elem x ys = diffList xs ys
                   | otherwise = x : diffList xs ys

-- Uses the diffList function to find the first natural number (0 and up) which is
-- NOT in the list.
--  firstNatNotIn [1,2,3,4,5] => 0
--  firstNatNotIn [0,1,2,3,4,5] => 6
--  firstNatNotIn [0 .. 100] => 101
firstNatNotIn :: [Integer] -> Integer
firstNatNotIn xs = head (diffList [0 .. (length xs) + 1] xs)


test :: IO ()
test = putStrLn ("diffList..." ++
                 "\n[0,1,2,3,4,5,6,7,8,9] [(-1), 0, 5] => " ++ (show (diffList [0,1,2,3,4,5,6,7,8,9] [(-1), 0, 5])) ++
                 "\n\"Sebastian Meyer\" \"aaaaennn\" => " ++ (show (diffList "Sebastian Meyer" "aaaaennn")) ++
                 "\n\nfirstNatNotIn..." ++
                 "\n[1,2,3,4,5] => " ++ (show (firstNatNotIn [1,2,3,4,5])) ++
                 "\n[0,1,2,3,4,5] => " ++ (show (firstNatNotIn [0,1,2,3,4,5])) ++
                 "\n[0 .. 100] => " ++ (show (firstNatNotIn [0 .. 100])) ++
                 "\n[1 .. 100] => " ++ (show (firstNatNotIn [1 .. 100])))
