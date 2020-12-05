{-
Funktionale Programmierung Übung 2 
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

import Data.Char (digitToInt) -- Used in task 7
import Data.Char (intToDigit) -- Used in task 7

-- Funktion von Fr. Prof. Esponda, als Hilfsfunktion genutzt.
bin2dec :: [Int] -> Int
bin2dec bits = bin2dec' 0 bits
               where
                 bin2dec' ac [b]    = 2*ac + b
                 bin2dec' ac (b:bs) = bin2dec' (2*ac + b) bs

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
            | 'a' <= c && 'f' >= c = toBin2 (digitToInt c)
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

----------

test :: IO ()
test = putStrLn("hex2okt \"\": " ++ (show (hex2okt "")) ++
               "\nhex2okt \"F\": " ++ (show (hex2okt "F")) ++
               "\nhex2okt \"1F81F8\": " ++ (show (hex2okt "1F81F8")))


-- 1F => 01 1111
-- 011111 => 011 => 3 => '3' => '3':
--        => 111 => 7 => '7' => ('3'):('7'):""
