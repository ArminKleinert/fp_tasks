-- A1

dsub :: Integer -> Integer -> Integer -> Integer
dsub _ 0 acc = acc
dsub k n acc = dsub k (n-1) (acc ^ k)

-- FIXME
-- Output 3 3 => Erwatet: 7625597484987 
--               Ist:     7625597484987
-- Output 2 4 => Erwatet: 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084096
--               Ist:     65536
d :: Integer -> Integer -> Integer
d k n = dsub k n k

-- A2

-- flatten [[8, 2], [3], [], [4, 5, 0, 1], [1]] => [8,2,3,4,5,0,1,1]
flatten :: [[a]] -> [a]
flatten []  = []
flatten (l:ls) = l ++ flatten ls

-- A3

-- insert '3' 3 "Hello" => "Hel3lo"
insert :: t2 -> Integer -> [t2] -> [t2]
insert e 0 l      = e : l
insert e i []     = error "Index too big."
insert e i (s:ls) = s : insert e (i-1) ls

-- A4

toDecFrom' :: Int -> [Int] -> Int -> Int
toDecFrom' ac [d] base = base * ac + d
toDecFrom' ac (d:ds) base = toDecFrom' (base*ac + d) ds base

-- TODO
-- toDecFrom [1,3,2,1] 4  => Erwatet: 124 Ist: 121 (irb> "1321".to_i(4) gibt 121)
toDecFrom :: [Int] -> Int -> Int
toDecFrom digs base = if (0<base && base<=10)
                      then toDecFrom' 0 digs base
                      else error "Invalid base."

-- A5

removeRepsSub :: Eq a => [a] -> [a] -> [a] -> [a]
removeRepsSub [] mem acc     = acc
removeRepsSub (x:xs) mem acc = if (elem x mem)
                               then removeRepsSub xs mem acc
                               else removeRepsSub xs (x:mem) (acc ++ [x])

removeReps :: Eq a => [a] -> [a]
removeReps xs = removeRepsSub xs [] []

-- A6

isDivider :: Char -> Bool
isDivider c = elem c ",.? "

tokenizerSub :: [Char] -> [Char] -> [[Char]] -> [[Char]]
tokenizerSub ""    curr acc = reverse (acc ++ [curr])
tokenizerSub (c:s) curr acc = if isDivider c
                              then tokenizerSub s "" (acc ++ [curr])
                              else tokenizerSub s (c:curr) acc

tokenizer :: [Char] -> [[Char]]
tokenizer s = tokenizerSub (reverse s) "" []

-- A7

-- A8

