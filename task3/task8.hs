{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert, Anna Sophie Pipperr und Alexandra Wolfers
-}

-- A8

-- Helper for inserting padding on the left of a list.
lpad :: Int -> a -> [a] -> [a]
lpad m v xs = if m == length xs
              then xs
              else replicate padlen v ++ xs
  where padlen = m - ((length xs) `mod` m) -- Calculate number length of padding

-- Binary addition helper. Treat as a black box :)
-- Given 2 lists of 1s and 0s, returns a tuple containing an Int and a list of Ints.
-- The first Int is the carry, which can be either 1 or 0. The rest is the result
-- of the binary addition.
addBits' :: [Int] -> [Int] -> (Int, [Int])
addBits' [] ys = (0, ys)
addBits' xs [] = (0, xs)
addBits' (x:xs) (y:ys) = (carry, z:zs)
  where (c, zs) = addBits' xs ys
        (carry, z) = (x + y + c) `divMod` 2

-- Performs binary addition on 2 lists of Ints.
-- Uses lpad to make both lists have the same length and then calls addBits'
addBits :: [Int] -> [Int] -> [Int]
addBits xs ys =
  let al = length xs
      bl = length ys
      gl = if al >= bl then al else bl
      xl0 = (lpad gl 0 xs) -- Do padding so that both lists have the same length
      yl0 = (lpad gl 0 ys) -- Do padding so that both lists have the same length
  in case addBits' xl0 yl0 of
          (0, zs) -> zs
          (1, zs) -> 1:zs

-- Helper for multBits.
-- Imagine binary multiplication like this:
-- 0010 * 1010 (Let's call 0010 "n" and 1010 "m")
--   +    0000 -- Rightmost bit of m is 0, so put 0 here, shifted left by 0
--   +   0010  -- Rightmost bit of m is 1, so put n here, shifted left by 1
--   +  0000   -- Same as in the first case but shifted by 2
--   + 0010    -- Same as second case, but shifted by 3
--   = 0010100 -- Add all the above.
-- What mbCollect does is build lists of those bit-patterns:
-- Example:
--   mbCollect [0,0,1,0] [1,0,1,0]
--   will return [[0,0,1,0,0], [0,0,1,0,0,0]] (as described above, but ignoring all 
--   the 0s).
-- Attention! If the input of the first argument was all 0s, this will return an 
-- empty list!
mbCollect :: [Int] -> [Int] -> [[Int]] -> [[Int]]
mbCollect []        _     acc = acc
mbCollect (b:bits0) bits1 acc
  | b == 0 = mbCollect bits0 (bits1++[0]) acc
  | b == 1 = mbCollect bits0 (bits1++[0]) (bits1:acc)

-- Same as addBits, but can add more than only 2 lists.
addMany :: [[Int]] -> [Int]
addMany []       = [0] 
addMany [x]      = x
addMany (x:y:zs) = addMany ((addBits x y) : zs)

-- Multiplication for two lists of Ints, which are expected to be all 1s and 0s.
-- See mbCollect and addBits for in-depth descriptions.
multBits :: [Int] -> [Int] -> [Int]
multBits bits0 bits1 = addMany (mbCollect (reverse bits0) bits1 [])

--

test :: IO ()
test = putStrLn ("multBits..."        ++
                 "\n[1,0] [1,0,1,0]:  " ++ (show (multBits [1,0] [1,0,1,0])) ++
                 "\n[1,1,1,1] [1,1]:  " ++ (show (multBits [1,1,1,1] [1,1])) ++
                 "\n[0] [1,1,1,1,1]:  " ++ (show (multBits [0] [1,1,1,1,1])) ++
                 "\n[1] [1,1,1,1,1]:  " ++ (show (multBits [1] [1,1,1,1,1])) ++
                 "\n[1,1,1,1,1] [0]:  " ++ (show (multBits [1,1,1,1,1] [0])))



