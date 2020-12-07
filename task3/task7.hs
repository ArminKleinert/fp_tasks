{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert, Anna Sophie Pipperr und Alexandra Wolfers
-}

-- A7

-- Selects groups of equal elements in a list of a generic type.
-- groupEquals [1,1,1,2,3,4,1,1] => [[1,1,1],[2],[3],[4],[1,1]]
groupEquals :: Eq a => [a] -> [[a]]
groupEquals list | list == group = group:[]
                 | otherwise = group:(groupEquals (drop index list))
                  where (group, index) = gWhileEq 1 list list
                  
gWhileEq :: Eq a => Int -> [a] -> [a] -> ([a],Int)
gWhileEq i list [] = (list,i)
gWhileEq i list [x] = (list,i)
gWhileEq i list (x:xs) | x == y = gWhileEq (i+1) list xs
                       | otherwise = (take i list, i)
                        where y = xs !! 0

--

test :: IO ()
test = putStrLn ("groupEquals..."        ++
                 "\n[1,1,1,1]:  " ++ (show (groupEquals [1,1,1,1])) ++
                 "\n[1,1,1,2]:  " ++ (show (groupEquals [1,1,1,2])) ++
                 "\n[1,2,1,2]:  " ++ (show (groupEquals [1,2,1,2])) ++
                 "\n[3,2,2,1]:  " ++ (show (groupEquals [3,2,2,1])) ++
                 "\n\"abbccdde\": " ++ (show (groupEquals "abbccdde")) ++
                 "\ngroupEquals [1,1,1,2,3,4,1,1]: " ++ (show (groupEquals [1,1,1,2,3,4,1,1])))
