{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert, Anna Sophie Pipperr und Alexandra Wolfers
-}

-- A2

-- Given a multi-dimensional list of a generic type, flattens the list:
-- flatten [[8, 2], [3], [], [4, 5, 0, 1], [1]] => [8,2,3,4,5,0,1,1]
flatten :: [[a]] -> [a]
flatten []  = []                   -- Stop when the input is empty
flatten (l:ls) = (l ++ flatten ls) -- Use ++ instead of : to do a "flat" append here.

--

-- Attention: Tests with empty brackets like (flatten [[],[],[]]) do not 
-- work with (show ...). Please try them in the console yourself.
-- (flatten [[],[],[]]) == []
test :: IO ()
test = putStrLn ("flatten ..."         ++
                 "\n[['a']]:         " ++ (show (flatten [['a']])) ++
                 "\n[[1,1],[2]]:     " ++ (show (flatten [[1,1],[2]])) ++
                 "\n[\"abc\",\"d\"]:     " ++ (show (flatten ["abc","d"])) ++
                 "\n[[1,1,1,1,1,1]]: " ++ (show (flatten [[1,1,1,1,1,1]])) ++
                 "\n[[1],[1],[1]]:   " ++ (show ((flatten [[1],[1],[1]]) == [1,1,1])))
