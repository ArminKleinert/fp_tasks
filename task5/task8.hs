{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A8

-- Find first element which fullfills a given predicatein a list.
-- O(n)
first_by_pred :: (a -> Bool) -> [a] -> Maybe a
first_by_pred _ []     = Nothing
first_by_pred p (x:xs) | p x = Just x -- O(1)
                       | otherwise = first_by_pred p xs -- O(1) -> O(n)

-- Count occurences of an element in a list.
-- O(n)
countOf :: (Num p, Eq t) => t -> [t] -> p
countOf _ []     = 0
countOf x (y:ys) | x == y = 1 + countOf x ys
                 | otherwise = countOf x ys

-- uniquify a list, keeping only one of each element
-- O(n^2)
unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : unique (filter (/=x) xs)

-- O(n+1+1 + n^2 + n^2)
-- O(n^2)
majority :: (Eq a) => [a] -> Maybe a
majority xs = first_by_pred (\x -> num <= countOf x xs) uxs -- O(n * n)
  where num = (fromIntegral (length xs)) / 2 + 1 -- O(n + 1 + 1)
        uxs = unique xs -- O(n^2); In worst case uxs is ==xs

-- Test

test :: IO ()
test = putStrLn ("majority..." ++
  "\n [1,1,1,1,1,1]: " ++ (show (majority  [1,1,1,1,1,1])) ++
  "\n [1,1,1,1,1,1,2]: " ++ (show (majority  [1,1,1,1,1,1,2])) ++
  "\n [1,1,1,1,1,1,2,2,2,2,2,2,2]: " ++ (show (majority  [1,1,1,1,1,1,2,2,2,2,2,2,2])) ++
  "\n [1,1,1,1,1,1,2,2,2,2,2,2,2,2]: " ++ (show (majority  [1,1,1,1,1,1,2,2,2,2,2,2,2,2])) ++
  "\n [1 .. 15]: " ++ (show (majority  [1 .. 15])) ++
  "\n ([1 .. 15] ++ take 15 (repeat 15)): " ++ (show (majority  ([1 .. 15] ++ take 15 (repeat 15)))) ++
  "\n ([1 .. 15] ++ take 13 (repeat 15)): " ++ (show (majority  ([1 .. 15] ++ take 13 (repeat 15)))))







