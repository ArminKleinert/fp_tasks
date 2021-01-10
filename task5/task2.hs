{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A2

-- O(m*1) = O(m)
mult :: Integer -> Integer -> Integer
mult n 0 = 0                          -- O(m+1)
mult n m = mult n (m-1) + n           -- O(2) -> O(m*2)

-- O(log(m))
russMult :: Integer -> Integer -> Integer
russMult n 0 = 0 -- O(1)
russMult n m | (mod m 2) == 0 = russMult (n+n) (div m 2) -- O(4) -> O(log m)
             | otherwise      = russMult (n+n) (div m 2) + n -- O(3) -> O(log m)
