{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A3

-- Splits words in a string at ' ', '.', ',', '?', '!', '\n' and '\t'
--  (O(length(curr))
-- + O(1) -- ' ' oder '.'
-- + O(1)) -- Vom Ende
-- * n -- Länge des Satzes
-- = O(((length curr) + 1 + 1) * n)
-- = O(((length curr) + 2) * n)
-- = O(n*(length curr) + 2n)
-- = O(n)
splitWords :: [Char] -> [Char] -> [[Char]]
splitWords "" ""   = [] -- O(1)
splitWords "" curr = [curr] -- O(1)
splitWords (c:cs) curr | c == ' ' = curr : splitWords cs "" -- O(1) + ?
                       | c == '.' = curr : splitWords cs "" -- O(1) + ?
                       | c == ',' = curr : splitWords cs "" -- O(1) + ?
                       | c == '?' = curr : splitWords cs "" -- O(1) + ?
                       | c == '!' = curr : splitWords cs "" -- O(1) + ?
                       | c == '\n' = curr : splitWords cs "" -- O(1) + ?
                       | c == '\t' = curr : splitWords cs "" -- O(1) + ?
                       | otherwise= splitWords cs (curr ++ [c]) -- ? + O(length(curr))

-- Selektiert die letzten 3 Zeichen eines Strings
-- O(n + n + 3 + n)
-- O(3n + 3)
-- O(3n)
-- O(n)
getLastThree :: [a] -> [a]
getLastThree lst | len > 3   = reverse (take 3 (reverse lst)) -- O(n + 3 + n)
                 | otherwise = lst -- O(1)
  where len = length lst -- O(n)

-- Kopiert von https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.List.html
-- O(n * (O(select p x))
-- O(n * p)
partition :: (a -> Bool) -> [a] -> ([a],[a])
partition p xs = foldr (select p) ([],[]) xs

-- Kopiert von https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.List.html
-- O(p + 1 + 1 + 1)
-- O(p + 3)
-- O(p)
select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
select p x ~(ts,fs) | p x       = (x:ts,fs) -- O(O(p) + 1 + 1 + 1)
                    | otherwise = (ts, x:fs) -- O(1 + 1 + 1)

-- Takes a list of strings (words) and partitions them by their last 3 letters, 
-- eventually making a list of lists of strings.
-- O(n * (1+1 + (n * n + 3)))
-- O(n * (2 + (n^2 + 3)))
-- O(n * (n ^ 2 + 5)
-- O(n * n^2)
-- O(n(n^2))
partitionWordsBySuffix :: Eq a => [[a]] -> [[[a]]]
partitionWordsBySuffix [] = [] -- O(1)
partitionWordsBySuffix (x:xs) =
  let xSuffix = getLastThree x -- Take last 3 letters of a word (O(n)
      p = partition (\y -> (getLastThree y) == xSuffix) xs -- O(n * (n + 3))
  in (x:(fst p)) : (partitionWordsBySuffix (snd p)) -- O(1+1 + (n * (n + 3))

-- Takes a string and partitions the words by their last 3 letters.
-- Partitioning starts from the left.
-- O((n * (n ^ 2)) + n)
-- O(n * (n ^ 2))
-- O(n(n^2))
classifyRymeWords :: [Char] -> [[[Char]]]
classifyRymeWords s = partitionWordsBySuffix words -- O(n(n^2))
  where words = splitWords s "" -- O(n)




test :: IO ()
test = putStrLn ("\"Nikolaus baut ein Haus aus Holz und klaut dabei ein Bauhaus.\": " ++ 
  (show (classifyRymeWords "Nikolaus baut ein Haus aus Holz und klaut dabei ein Bauhaus.")))

