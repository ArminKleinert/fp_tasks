{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A1

{-
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs | isSorted (<=) xs = xs
              | otherwise = bubbleSort (moveBubble xs)
                where 
                moveBubble [] = []
                moveBubble [x] = [x]
                moveBubble (x:y:rest) | (<=) x y   = x: moveBubble (y:rest)
                                      | otherwise  = y: moveBubble (x:rest)
-}

-- Kontrolliert mit einem Prädikat f und einer Liste, ob diese Liste sortiert ist.
isSorted :: (a -> a -> Bool) -> [a] -> Bool
isSorted _ []       = True
isSorted _ [x]      = True
isSorted f (x:y:zs) = (f x y) && (isSorted f (y:zs))

-- Helper für traceBubbleSort.
-- Speichert das Ergebnis der Iterationen von moveBubble in einer 2-dimensionalen
-- Liste. Die erste Eingabe wird in traceBubbleSort gespeichert, nicht in dieser 
-- Hilfsfunktion.
traceBubbleSort' :: (Ord a) => [a] -> [[a]]
traceBubbleSort' xs
  | isSorted (<=) xs = [xs]
  | otherwise = (traceBubbleSort' (moveBubble xs)) ++ [(moveBubble xs)]
                where
                  moveBubble [] = []
                  moveBubble [x] = [x]
                  moveBubble (x:y:rest) | (<=) x y   = x: moveBubble (y:rest)
                                        | otherwise  = y: moveBubble (x:rest)

-- Speichert das Ergebnis der Iterationen von moveBubble in einer 2-dimensionalen
-- Liste.
-- 
traceBubbleSort :: (Ord a) => [a] -> [[a]]
traceBubbleSort xs = (traceBubbleSort' xs) ++ [xs]


test :: IO ()
test = putStrLn ("traceBubbleSort" ++
                 "\n[0,1,2,3,4,5,6,7,8,9] " ++
                 (show (traceBubbleSort [0,1,2,3,4,5,6,7,8,9])) ++
                 "\n[9,8,7,6,5,4,3,2,1,0] " ++ 
                 (show (traceBubbleSort [9,8,7,6,5,4,3,2,1,0])) ++
                 "\n [0,1,3,8,0]          " ++
                 (show (traceBubbleSort [0,1,3,8,0])))


