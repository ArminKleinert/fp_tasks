{-
Funktionale Programmierung Übung 2 
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- 1.

bin2dec :: [Int] -> Int
bin2dec bits = bin2dec' 0 bits
               where
                 bin2dec' ac [b]    = 2*ac + b
                 bin2dec' ac (b:bs) = bin2dec' (2*ac + b) bs

{-
bin2dec [0,1,0,1,1,0]
bin2dec' 0  [0,1,0,1,1,0] => bin2dec' (2 * 0 + 0) [0,1,0,1,1,0]
bin2dec' 0  [1,0,1,1,0]   => bin2dec' (2 * 0 + 1) [0,1,1,0]
bin2dec' 1  [0,1,1,0]     => bin2dec' (2 * 1 + 0) [1,1,0]
bin2dec' 2  [1,1,0]       => bin2dec' (2 * 2 + 1) [1,0]
bin2dec' 5  [1,0]         => bin2dec' (2 * 5 + 1) [0]
bin2dec' 11 [0]           => (2 * 11 + 0)
                          => 22
-}

-- 2.

fact :: Integer -> Integer -> Integer
fact 0 _  = 1
fact 1 ac = ac
fact n ac = fact (n-1) (n*ac)

roughlyPIsub :: Integer -> Rational -> Rational
roughlyPIsub (-1) acc = acc
roughlyPIsub n acc =
  let divident = fromIntegral (2 ^ (n+1) * (fact n 1) ^ 2)
      divisor  = fromIntegral (fact (2 * n + 1) 1)
  in roughlyPIsub (n-1) (acc + (divident / divisor))

roughlyPI :: Integer -> Double
roughlyPI k = fromRational (roughlyPIsub k 0)

-- 3.

onlyParenthesisSub :: [Char] -> [Char] -> [Char]
onlyParenthesisSub [] res = res
onlyParenthesisSub (fc:rest) res  =
  let allParens = "({[]})"
  in onlyParenthesisSub rest (if elem fc allParens then (res ++ [fc]) else res)

onlyParenthesis :: [Char] -> [Char]
onlyParenthesis s = onlyParenthesisSub s ""

-- 4. 

hexagonalNums :: Integer -> [Integer]
hexagonalNums 0 = [0]
hexagonalNums n = (hexagonalNums (n - 1)) ++ [((2 * n ^ 2) - n)]

-- 5.

averageInIntervalSub :: Double -> Double -> [Double] -> [Double] -> [Double]
averageInIntervalSub n m [] res  = res
averageInIntervalSub n m (f : lst) res =
  let doadd = n <= f && f <= m
      mres = if doadd then (f : res) else res
  in averageInIntervalSub n m lst mres

averageInInterval :: Integer -> Integer -> [Double] -> Double
averageInInterval n m lst =
  let nums = averageInIntervalSub (fromIntegral n) (fromIntegral m) lst []
  in if null nums then 0 else (sum nums) / fromIntegral (length nums)

-- 6.

toBin :: Integer -> [Integer]
toBin 0 = [0]
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

binaryQSum :: Integer -> Integer
binaryQSum n = sum (toBin n)


