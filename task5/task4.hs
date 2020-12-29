{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A4

-- Calculates "biggest" element by a comparator.
-- calculateFirst (<) [1,2,3,4,5] => 1
-- calculateFirst (>) [1,2,3,4,5] => 5
-- calculateFirst (<) [2,3,4,5,1] => 1
-- O(n)
calculateFirst :: (a -> a -> Bool) -> [a] -> a
calculateFirst _ [x] = x 
calculateFirst c (a:b:xs) = help_maxi c a (b:xs) 
  where                                  
    help_maxi _ a []  = a                                   
    help_maxi c a (b:xs)                                              
      | c a b       = help_maxi c a xs
      | otherwise   = help_maxi c b xs

-- Remove first occurance of an element from a list.
-- O(n)
deleteElem :: Eq t => t -> [t] -> [t]
deleteElem _ []     = []
deleteElem x (y:ys) | x == y = ys
                    | otherwise = y : deleteElem x ys

-- Tail-recursive helper for selectionSort.
-- O(n*log(n))
selectionSort' :: Ord t => (t -> t -> Bool) -> [t] -> [t] -> [t]
selectionSort' _ sorted [] = sorted
selectionSort' c sorted unsorted = selectionSort' c (mx : sorted) (deleteElem mx unsorted) -- O(n*log(n))
  where mx = calculateFirst (\x y -> c y x) unsorted -- O(n)

-- O(n*log(n))
selectionSort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
selectionSort c xs = selectionSort' c [] xs -- O(n*log(n))


test :: IO ()
test = putStrLn ("selectionSort...\n" ++ 
                 "\n(<) [2,1,5,0,4,3,7]: " ++ (show (selectionSort (<) [2,1,5,0,4,3,7])) ++
                 "\n(>) [2,1,0,4,3,7]: " ++ (show (selectionSort (>) [2,1,0,4,3,7])) ++
                 "\n(>) [2,1,5,0,4,3,7,1,1,1,1,2,2,2,2]: " ++ (show (selectionSort (>) [2,1,5,0,4,3,7,1,1,1,1,2,2,2,2])) ++
                 "\n(>=) [2,1,5,0,4,3,7,1,1,1,1,2,2,2,2]: " ++ (show (selectionSort (>) [2,1,5,0,4,3,7,1,1,1,1,2,2,2,2])) ++
                 "\n(<) (reverse [1 .. 15]): " ++ (show (selectionSort (<) (reverse [1 .. 15]))) ++
                 "\n(>) (reverse [1 .. 15]): " ++ (show (selectionSort (>) (reverse [1 .. 15]))))
