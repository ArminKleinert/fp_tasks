{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

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
