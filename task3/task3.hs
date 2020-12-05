{-
Funktionale Programmierung Übung 2
Abgabe von Armin Kleinert, Anna Sophie Pipperr und Alexandra Wolfers
-}

-- A3

-- Inserts an element at a given index in a list
-- If the index is too big, an error is shown instead.
-- insert '3' 3 "Hello" => "Hel3lo"
insert :: t2 -> Integer -> [t2] -> [t2]
insert e 0 l      = e : l
insert e i []     = error "Index too big."
insert e i (s:ls) = s : insert e (i-1) ls


test :: IO ()
test = putStrLn ("insert ..."          ++
                 "\n0 'a' \"bcd\":       " ++ (show (insert 'a' 0 "bcd")) ++
                 "\n1 [3] [[1],[2]]:   " ++ (show (insert [3] 1 [[1],[2]])) ++
                 "\n2 \"e\" [\"abc\",\"d\"]: " ++ (show (insert "e" 2 ["abc","d"])))
