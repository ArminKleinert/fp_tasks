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
-- 
