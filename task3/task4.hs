{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert, Anna Sophie Pipperr und Alexandra Wolfers
-}

-- A4

-- Helper for toDecFrom. Uses an accumulator to achieve tail recursion.
-- An error is shown if a digit is invalid for the given base.
-- (For example, 9 is not a valid digit for base 9).
-- Since this is a helper, it relies on toDecFrom for initial error 
-- checking: The base is expected to be >0 and <=10. The list of digits
-- has to have at least 1 element.
toDecFrom' :: Int -> [Int] -> Int -> Int
toDecFrom' ac (d:ds) base
          | d >= base = error "Invalid digit for given base."
          | null ds   = base * ac + d
          | otherwise = toDecFrom' (base*ac + d) ds base

-- Takes a list of integers and another integer (base), treats the list
-- as a sequence of digits of the given base. The return value is a 
-- decimal number.
-- Example:
--   toDecFrom [1,1,1,1] 2 => 15 -- base 2 (binary)
--   toDecFrom [1,7] 9     => 15 -- base 8 (octal)
--   toDecFrom []          => error
--   toDecFrom [1,1,1,1] 1 => error
--   toDecFrom [9,9] 10    => 99 -- From base 10 to base 10 :)
toDecFrom :: [Int] -> Int -> Int
toDecFrom digs base = if ((0<base && base<=10) || (null digs))
                      then toDecFrom' 0 digs base
                      else error "Invalid base or empty list."

test :: IO ()
test = putStrLn ("toDecFrom..."        ++
                 "\n[1,1,1,1] 2:     " ++ (show (toDecFrom [1,1,1,1] 2)) ++
                 "\n[0,0,1,0] 2:     " ++ (show (toDecFrom [0,0,1,0] 2)) ++
                 "\n[1, 7] 8:        " ++ (show (toDecFrom [1, 7] 8)) ++
                 "\n[1, 5] 10:       " ++ (show (toDecFrom [9,9] 10)))
--   toDecFrom []          => error
--   toDecFrom [1,1,1,1] 1 => error
