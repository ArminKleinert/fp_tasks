{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A4

toDecFrom' :: Int -> [Int] -> Int -> Int
toDecFrom' ac [d] base = base * ac + d
toDecFrom' ac (d:ds) base = toDecFrom' (base*ac + d) ds base

toDecFrom :: [Int] -> Int -> Int
toDecFrom digs base = if (0<base && base<=10)
                      then toDecFrom' 0 digs base
                      else error "Invalid base."

