{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert, Anna Sophie Pipperr und Alexandra Wolfers
-}

 --8. Aufgabe (Zusatz)

multBits :: [Int] -> [Int] -> [Int]
multBits a b | length a <= length b = multiply 0 a b
             | length a > length b =  multiply 0 b a

multiply :: Int -> [Int] -> [Int] -> [Int]             
multiply i [] y = []
multiply i [x] y | x == 1 = reverse (pad i y)
                 | x == 0 = (pad i [0])
multiply i (x:xs) y | x == 1 = reverse (sumBits 0 (reverse (pad i y)) (multiply (i+1) xs y))
                    | x == 0 = multiply (i+1) xs y

sumBits :: Int -> [Int] -> [Int] -> [Int]
sumBits 0 [] [] = []
sumBits 1 [] [] = [1]                     
sumBits carry (x:xs) (y:ys) | length xs == length ys = (mod (x+y+carry) 2):(sumBits (div (x+y+carry) 2) xs ys)
                            | length xs < length ys = sumBits 0 (pad (length ys - length xs) (x:xs)) (y:ys)
                            | otherwise = sumBits 0 (x:xs) (pad (length xs - length ys) (y:ys))
                            
pad :: Int -> [Int] -> [Int]
pad 0 x = x
pad i x = pad (i-1) x ++ [0] 

--

test :: IO ()
test = putStrLn ("multBits..."        ++
                 "\n[1,0] [1,0,1,0]:  " ++ (show (multBits [1,0] [1,0,1,0])) ++
                 "\n[1,1,1,1] [1,1]:  " ++ (show (multBits [1,1,1,1] [1,1])) ++
                 "\n[0] [1,1,1,1,1]:  " ++ (show (multBits [0] [1,1,1,1,1])) ++
                 "\n[1] [1,1,1,1,1]:  " ++ (show (multBits [1] [1,1,1,1,1])) ++
                 "\n[1,1,1,1,1] [0]:  " ++ (show (multBits [1,1,1,1,1] [0])))



