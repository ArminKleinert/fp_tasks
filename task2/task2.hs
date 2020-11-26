{-
Funktionale Programmierung Übung 2 
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- 2.

-- Helper for roughlyPIsub
fact :: Integer -> Integer -> Integer
fact 0 _  = 1
fact 1 ac = ac
fact n ac = fact (n-1) (n*ac)

-- Helper for roughlyPI
roughlyPIsub :: Integer -> Rational -> Rational
roughlyPIsub (-1) acc = acc
roughlyPIsub n acc =
  let divident = fromIntegral (2 ^ (n+1) * (fact n 1) ^ 2)
      divisor  = fromIntegral (fact (2 * n + 1) 1)
  in roughlyPIsub (n-1) (acc + (divident / divisor))

-- Given an Integer, approximates PI
roughlyPI :: Integer -> Double
roughlyPI k = fromRational (roughlyPIsub k 0)

----------

-- Test

test :: IO ()
test = putStrLn ("roughlyPI 300:  " ++ (show (roughlyPI 300)) ++ 
                 "\nroughlyPI 0:    " ++ (show (roughlyPI 0)) ++
                 "\nroughlyPI 1:    " ++ (show (roughlyPI 1)) ++
                 "\nroughlyPI 1000: " ++ (show (roughlyPI 1000)))

