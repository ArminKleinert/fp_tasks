{-
Funktionale Programmierung Übung 2 
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

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

----------

test :: IO ()
test = putStrLn ("averageInInterval 0 0 []: " ++ (show (averageInInterval 0 0 [])) ++
                 "\naverageInInterval 2 5 [2.0, 3.0, 5.0, 1.0, 0.0, 1.0]: " ++ (show (averageInInterval 2 5 [2.0, 3.0, 5.0, 1.0, 0.0, 1.0])) ++
                 "\naverageInInterval 0 0 [1,2,3,4,5,6,7,8,9,10]: " ++ (show (averageInInterval 0 0 [1,2,3,4,5,6,7,8,9,10])))
