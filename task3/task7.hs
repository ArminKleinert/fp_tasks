{-
Funktionale Programmierung Übung 2 
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A7

groupEquals' :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
groupEquals' []     acc res = if null acc
                              then res 
                              else res ++ [acc]
groupEquals' (x:xs) []  res = groupEquals' xs [x] res
groupEquals' (x:xs) (a:acc) res = if x == a
                                  then groupEquals' xs (x:a:acc) res
                                  else groupEquals' xs [x] (res ++ [a:acc])

groupEquals :: Eq a => [a] -> [[a]]
groupEquals lst = groupEquals' lst [] []
