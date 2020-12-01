{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A1

-- Output 3 3 => Erwatet: 7625597484987 
--               Ist:     7625597484987
-- Output 4 2 => Erwatet: 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084096
--               Ist:     340282366920938463463374607431768211456
--               (Ist 4°4 gemeint???)
(°) :: Integer -> Integer -> Integer
(°) k n = dsub k n k
          where dsub _ 0 acc = acc
                dsub k n acc = dsub k (n-1) (acc ^ k)
