{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert, Anna Sophie Pipperr und Alexandra Wolfers
-}

-- A5

-- Helper for removeReps.
--   1st argument: list of generic elements (as described in removeReps).
--   2nd argument: "memory" which "remembers" all used elements.
--   3rd argument: holds the final result.
-- Uses tail recursion.
removeRepsSub :: Eq a => [a] -> [a] -> [a] -> [a]
removeRepsSub [] mem acc     = acc
removeRepsSub (x:xs) mem acc = if (elem x mem)
                               then removeRepsSub xs mem acc
                               else removeRepsSub xs (x:mem) (acc ++ [x])
--
-- Removes all duplicates (repeated elements) from a list.
--   removeReps [1,1,1,1] => [1]
--   removeReps [1]       => [1]
--   removeReps []        => []
--   removeReps [1,2,3,4,5] => [1,2,3,4,5]
-- Elements in the list need to be derived from Eq, so that a comparison
-- via == is possible.
removeReps :: Eq a => [a] -> [a]
removeReps xs = removeRepsSub xs [] []

-- Attention: Haskell doesn't let me check empty lists because of the
-- generic typing.
--   removeReps []
test :: IO ()
test = putStrLn ("toDecFrom..."        ++
                 "\n[1,1,1,1]:       " ++ (show (removeReps [1,1,1,1])) ++
                 "\n[1]:             " ++ (show (removeReps [1])) ++
                 "\n[1,2,3,4,5]:     " ++ (show (removeReps [1,2,3,4,5])) ++
                 "\n\n{-Funktionale Programmierung Übung 3Abgabe von Armin Kleinert und Anna Sophie Pipperr-}:       " ++ (show (removeReps "{-Funktionale Programmierung Übung 3Abgabe von Armin Kleinert und Anna Sophie Pipperr-}")))


