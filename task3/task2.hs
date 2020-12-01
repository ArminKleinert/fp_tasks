{-
Funktionale Programmierung Übung 2 
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A2

-- flatten [[8, 2], [3], [], [4, 5, 0, 1], [1]] => [8,2,3,4,5,0,1,1]
flatten :: [[a]] -> [a]
flatten []  = []
flatten (l:ls) = l ++ flatten ls
