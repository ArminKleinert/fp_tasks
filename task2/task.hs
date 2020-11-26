{-
Funktionale Programmierung Übung 2 
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

import Data.Char (digitToInt) -- Used in task 7
import Data.Char (intToDigit) -- Used in task 7

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

-- 3.

-- Recursively iterates a string and returns a list of chars containing only the chars
-- '(', '{', '[', ']', '}', ')' in the order in which they appear in the string.
-- For the result, a second string is used as a buffer to achieve tail-recursion.
onlyParenthesisSub :: [Char] -> [Char] -> [Char]
onlyParenthesisSub [] res = res
onlyParenthesisSub (fc:rest) res  =
  let allParens = "({[]})"
  in onlyParenthesisSub rest (if elem fc allParens then (res ++ [fc]) else res)

-- Basically just calls onlyParenthesisSub
onlyParenthesis :: [Char] -> [Char]
onlyParenthesis s = onlyParenthesisSub s ""

-- 4. 

-- Returns a listlist of the first n hexagonal nuumbers.
-- Such a number can be found with the formula `2 * (n ^ 2) - n`.
-- Does not work with negative numbers.
hexagonalNums :: Integer -> [Integer]
hexagonalNums 0 = [0] -- Stop when 0 is reached.
hexagonalNums n = (hexagonalNums (n - 1)) ++ [((2 * n ^ 2) - n)]

-- 5.

-- Takes 2 numbers (n and m) and 2 lists of numbers.
-- Uses the second list as a buffer for tail-recursion.
-- Collects all the numbers between n and m (inclusive) and puts them into the buffer.
-- Afterwards, returns the buffer.
-- This could be shortened a lot by using filter, but has to be recursive...
averageInIntervalSub :: Double -> Double -> [Double] -> [Double] -> [Double]
averageInIntervalSub n m [] res  = res
averageInIntervalSub n m (f : lst) res =
  let doadd = n <= f && f <= m
      mres = if doadd then (f : res) else res
  in averageInIntervalSub n m lst mres

-- Uses averageInIntervalSub with an empty buffer and then returns the average
-- of all returned numbers.
-- If no numbers in the interval n..m were found in the list, returns 0.
-- Examples:
--   averageInInterval 2 5 [2.0, 3.0, 5.0, 1.0, 0.0, 1.0] => 3.3333..
--   averageInInterval 8 9 [1.0, 2.0, 3.0] => 0.0 (non in interval)
--   averageInInterval 0 9 [1.0, 2.0, 3.0] => 2.0 (all in interval)
averageInInterval :: Double -> Double -> [Double] -> Double
averageInInterval n m lst =
  let nums = averageInIntervalSub n m lst []
  in if null nums then 0 else (sum nums) / fromIntegral (length nums)

-- 6.

-- Recursively converts a number into binary.
-- No padding is prepended afterwards.
toBin :: Integer -> [Integer]
toBin 0 = [0]
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

-- Uses toBin and sums the 1s in the result.
binaryQSum :: Integer -> Integer
binaryQSum n = sum (toBin n)

-- 7.

-- Helper for inserting padding on the left of a list.
lpad :: Int -> Int -> [Int] -> [Int]
lpad m v xs = replicate padlen v ++ xs
  where padlen = m - ((length xs) `mod` m) -- Calculate number length of padding

-- Converts an int into a list of binary digits (1s and 0s).
-- No padding will be applied.
--   toBin2 2 => [1, 0]
--   toBin2 15 => [1,1,1,1]
--   toBin2 0 => [] -- <= Be careful!
toBin2 :: Int -> [Int]
toBin2 n | n == 0 = []
         | n `mod` 2 == 1 = toBin2 (n `div` 2) ++ [1]
         | n `mod` 2 == 0 = toBin2 (n `div` 2) ++ [0]

-- Turn a hex digit (0-9 and A-F) into a list of bits.
-- The function will not apply padding and will not make sure that the digit is valid.
-- hexToBits 'F' => [1,1,1,1]
-- hexToBits 'A' => [1,0,1,0]
-- hexToBits '0' => [0] -- Special case
-- hexToBits 'Z' => <Error>
hexToBits :: Char -> [Int]
hexToBits c | c == '0'             = [0]
            | '0' <= c && '9' >= c = toBin2 (digitToInt c)
            | 'A' <= c && 'F' >= c = toBin2 (digitToInt c)
            | otherwise            = error "No a valid hex digit."

-- The function will assume that the list's length is a multiple of 3.
bitsToOct :: [Int] -> [Char]
bitsToOct [] = "" -- If no more bits, return empty string
bitsToOct bs = let tbits = intToDigit (bin2dec (take 3 bs))
               in tbits : bitsToOct (drop 3 bs) -- Prepend to Rest

-- Convert Hexadecimal string to Octal string
hex2okt :: [Char] -> [Char]
hex2okt s = let bitlist = concat (map hexToBits s) -- Create bit-list
                paddedbits = lpad 3 0 bitlist -- Prepend padding
            in (bitsToOct paddedbits) -- Make oct string

-- Tests

-- 2.

roughlyPiTest :: Bool
roughlyPiTest = roughlyPI 300  == roughlyPI 300 &&
                roughlyPI 0    == roughlyPI 0 &&
                roughlyPI 1000 ==3.141592653589793

onlyParenthesisTest :: Bool
onlyParenthesisTest = onlyParenthesis "" == "" &&
                      onlyParenthesis "[(2+7.0)*a-(xyz), {word}]"  ==  "[()(){}]" &&
                      onlyParenthesis (replicate 50 '(') == replicate 50 '('

hexagonalNumsTest :: Bool
hexagonalNumsTest = hexagonalNums 9 == [0, 1, 6, 15, 28, 45, 66, 91, 120, 153] &&
                    hexagonalNums 0 == [0] &&
                    length (hexagonalNums 500) == 501 &&
                    hexagonalNums 5000 == hexagonalNums 5000

averageInIntervalTest :: Bool
averageInIntervalTest = averageInInterval 0 0 [] == 0 &&
                        averageInInterval 2 5 [2.0, 3.0, 5.0, 1.0, 0.0, 1.0] == (2.0 + 3.0 + 5.0) / 3.0 &&
                        averageInInterval 0 0 [1,2,3,4,5,6,7,8,9,10] == 0

binaryQSumTest :: Bool
binaryQSumTest = binaryQSum 0 == 0 &&
                 binaryQSum 15 == 4 &&
                 binaryQSum 16 == 1 &&
                 binaryQSum 2 ^ 32 == 1 &&
                 binaryQSum (2 ^ 32 - 1) == 32 &&
                 binaryQSum (2 ^ 128 - 1) == 128

hex2oktTest :: Bool
hex2oktTest = hex2okt "" == "0" &&
              hex2okt "F" == "17" &&
              hex2okt "1F81F8" == "07700770"

testAll :: Bool
testAll = roughlyPiTest && onlyParenthesis && hexagonalNums && averageInInterval && binaryQSum && hex2okt



