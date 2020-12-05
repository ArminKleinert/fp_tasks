{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert, Anna Sophie Pipperr und Alexandra Wolfers
-}

-- A7

-- Tail-recursive helper function for groupEquals.
-- Internally, an accumulator (acc) is used to "memorize" equal elements.
-- The generic type "a" needs to implement Eq so that it can be compared to the 
-- elements in the accumulator.
-- Example:
--  groupEquals' [1,1,1,2,3] [] [] -- list, accumulator (acc) and result
--  groupEquals' [1,1,2,3] [1] [] -- put 1 into accumulator
--  groupEquals' [1,2,3] [1,1] [] -- The next 1 is equal to the first in acc, so add it
--  groupEquals' [2,3] [1,1,1] [] -- Ditto
--  groupEquals' [3] [2] [[1,1,1]] -- 2 is not ==1, so add the acc to res and put 2
--                                 -- into a new accumulator
--  groupEquals' [] [3] [[1,1,1], [2]] -- 3/=2, so repeat the above
--  groupEquals' [] [] [[1,1,1], [2], [3]] -- Input is empty, so add acc to res
--                                         -- and return
-- When an empty list is given, an empty list is returned:
--   groupEquals' [] [] [] => []
groupEquals' :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
groupEquals' []     acc res = -- List is empty
  if null acc
  then res -- If the accumulator is empty, don't append it.
  else res ++ [acc]
groupEquals' (x:xs) []  res = groupEquals' xs [x] res -- When the accumulator is empty
groupEquals' (x:xs) (a:acc) res =
  if x == a
  then groupEquals' xs (x:a:acc) res -- Append element to accumulator
  else groupEquals' xs [x] (res ++ [a:acc]) -- Element does not belong to group

-- Selects groups of equal elements in a list of a generic type.
-- groupEquals [1,1,1,2,3,4,1,1] => [[1,1,1],[2],[3],[4],[1,1]]
-- See groupEquals' for a more in-depth explanation.
groupEquals :: Eq a => [a] -> [[a]]
groupEquals lst = groupEquals' lst [] []

--

test :: IO ()
test = putStrLn ("groupEquals..."        ++
                 "\n[1,1,1,1]:  " ++ (show (groupEquals [1,1,1,1])) ++
                 "\n[1,1,1,2]:  " ++ (show (groupEquals [1,1,1,2])) ++
                 "\n[1,2,1,2]:  " ++ (show (groupEquals [1,2,1,2])) ++
                 "\n[3,2,2,1]:  " ++ (show (groupEquals [3,2,2,1])) ++
                 "\n\"abbccdde\": " ++ (show (groupEquals "abbccdde")) ++
                 "\ngroupEquals [1,1,1,2,3,4,1,1]: " ++ (show (groupEquals [1,1,1,2,3,4,1,1])))
