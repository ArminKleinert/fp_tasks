{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert, Anna Sophie Pipperr und Alexandra Wolfers
-}

-- A5

-- Removes all duplicates (repeated elements) from a list.
--   removeReps [1,1,1,1] => [1]
--   removeReps [1]       => [1]
--   removeReps []        => []
--   removeReps [1,2,3,4,5] => [1,2,3,4,5]
-- Elements in the list need to be derived from Eq, so that a comparison
-- via == is possible.
removeReps :: Eq a => [a] -> [a]
removeReps [] = []
removeReps (x:xs) = [x] ++ (removeReps (filter (/=x) xs))

-- Attention: Haskell doesn't let me check empty lists because of the
-- generic typing.
--   removeReps []
test :: IO ()
test = putStrLn ("toDecFrom..."        ++
                 "\n[1,1,1,1]:       " ++ (show (removeReps [1,1,1,1])) ++
                 "\n[1]:             " ++ (show (removeReps [1])) ++
                 "\n[1,2,3,4,5]:     " ++ (show (removeReps [1,2,3,4,5])) ++
                 "\n\n{-Funktionale Programmierung Übung 3Abgabe von Armin Kleinert und Anna Sophie Pipperr-}:       " ++ (show (removeReps "{-Funktionale Programmierung Übung 3Abgabe von Armin Kleinert und Anna Sophie Pipperr-}")))


