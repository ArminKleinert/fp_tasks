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

groupEquals' :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
groupEquals' []     acc res = if null acc
                              then res 
                              else res ++ [acc]
groupEquals' (x:xs) []  res = groupEquals' xs [x] res
groupEquals' (x:xs) (a:acc) res = if x == a
                                  then groupEquals' xs (x:a:acc) res
                                  else groupEquals' xs [x] (res ++ [a:acc])

groupEquals :: Eq a => [a] -> [[a]]
groupEquals lst = groupEquals' lst [] []

-- A8


-- Helper for inserting padding on the left of a list.
lpad :: Int -> Int -> [Int] -> [Int]
lpad m v xs = if m == length xs
              then xs
              else replicate padlen v ++ xs
  where padlen = m - ((length xs) `mod` m) -- Calculate number length of padding

addBits' :: [Int] -> [Int] -> (Int, [Int])
addBits' [] ys = (0, ys)
addBits' xs [] = (0, xs)
addBits' (x:xs) (y:ys) = (carry, z:zs)
  where (c, zs) = addBits' xs ys
        (carry, z) = (x + y + c) `divMod` 2
  
addBits :: [Int] -> [Int] -> [Int]
addBits xs ys =
  let al = length xs
      bl = length ys
      gl = if al >= bl then al else bl
      xl0 = (lpad gl 0 xs)
      yl0 = (lpad gl 0 ys)
  in case addBits' xl0 yl0 of
          (0, zs) -> zs
          (1, zs) -> 1:zs

mbCollect :: [Int] -> [Int] -> [[Int]] -> [[Int]]
mbCollect []        _     acc = acc
mbCollect (b:bits0) bits1 acc
  | b == 0 = mbCollect bits0 (bits1++[0]) acc
  | b == 1 = mbCollect bits0 (bits1++[0]) (bits1:acc)

addMany :: [[Int]] -> [Int]
addMany [x]      = x
addMany (x:y:zs) = addMany ((addBits x y) : zs)

multBits :: [Int] -> [Int] -> [Int]
multBits bits0 bits1 = addMany (mbCollect (reverse bits0) bits1 [])

