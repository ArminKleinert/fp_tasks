-- 1.

u = let a = 0
        b = a + ( let c = 3
                  in  c + 2*a ) -- c = 3 => b = 3+0
    in b * b -- 3*3 => 9

v n = let x = n
          y = x + ( let x = 3
                        y = z -- n*n
                    in x+y ) -- 3+z => y = 3+n*n
          z = x^2 -- n*n
      in  y*y -- (x + 3+n*n) * (x + 3+n*n) => (n+3+n*n) * (n+3+n*n)

-- 2.

skipSpaces1 :: [Char] -> [Char]
skipspaces1 []      = ""
skipSpaces1 (c:cs)  = if (c == ' ') then (skipSpaces cs) else c : (skipSpaces cs)

skipSpaces :: String -> String
skipSpaces [] = []
skipSpaces (x:xs)
    |x == ' ' = skipSpaces xs
    |otherwise = x : skipSpaces xs
    
skipSpaces2 :: String -> String 
skipSpaces2 s = [c | c <- filter (\c1 -> c1 /= ' ') s]

skipSpaces3 :: String -> String 
skipSpaces3 xs = [a | a <- xs, a /= ' ']

-- 3.

flip' :: [Int] -> [Int]
flip' [] = []
flip' (x:xs)
    |x == 1 = 0 : flip' xs
    |x == 0 = 1 : flip' xs

twoComplement :: [Int] -> [Int]
twoComplement bits = []
