{-
Funktionale Programmierung Übung 2 
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A5

removeRepsSub :: Eq a => [a] -> [a] -> [a] -> [a]
removeRepsSub [] mem acc     = acc
removeRepsSub (x:xs) mem acc = if (elem x mem)
                               then removeRepsSub xs mem acc
                               else removeRepsSub xs (x:mem) (acc ++ [x])

removeReps :: Eq a => [a] -> [a]
removeReps xs = removeRepsSub xs [] []

