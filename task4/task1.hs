{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A1

isSorted f []       = True
isSorted f [x]      = True
isSorted f (x:y:zs) = (f x y) && (isSorted f (y:zs))

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs | isSorted (<=) xs = xs
              | otherwise = bubbleSort (moveBubble xs)
                where 
                moveBubble [] = []
                moveBubble [x] = [x]
                moveBubble (x:y:rest) | (<=) x y   = x: moveBubble (y:rest)
                                      | otherwise  = y: moveBubble (x:rest)








traceBubbleSort' :: (Ord a) => [a] -> [[a]]
traceBubbleSort' xs
  | isSorted (<=) xs = [xs]
  | otherwise = (traceBubbleSort' (moveBubble xs)) ++ [(moveBubble xs)]
                where
                  moveBubble [] = []
                  moveBubble [x] = [x]
                  moveBubble (x:y:rest) | (<=) x y   = x: moveBubble (y:rest)
                                        | otherwise  = y: moveBubble (x:rest)

traceBubbleSort :: (Ord a) => [a] -> [[a]]
traceBubbleSort xs = (traceBubbleSort' xs) ++ [xs]


-- TODO Tests
test :: IO ()
test = putStrLn ""


