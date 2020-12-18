data Weight = Kilo Double | Pfund Double
                deriving Show

kilo2Pfund :: Weight -> Weight
kilo2Pfund (Kilo k)  = Pfund (k * 2.205)

pfund2Kilo :: Weight -> Weight
pfund2Kilo (Pfund p) = Kilo (p / 2.205)

map2 :: ((a -> b), [a]) -> [b]
map2 (_, [])     = []
map2 (f, (x:xs)) = (f x) : map2 (f, xs)
