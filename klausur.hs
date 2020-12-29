
paintChars f size = putStrLn (genChars f size)
genChars f size = paint size (map f [(x,y,size) | y <- [1..size], x <- [1..size]])
                  where
                  paint 0  []     = []
                  paint 0 (c:cs)  = '\n' : (paint size (c:cs))
                  paint n (c:cs)  = c: (paint (n-1) cs)


squares :: (Int, Int, Int) -> Char
squares (x, y, size) | y >= quartSize && y <= (size-quartSize) = if xinrange
                                                                    then ' '
                                                                    else '.'
                     | otherwise = '.'
  where
    quartSize = size `div` 6
    xinrange = x >= quartSize && x <= (size-quartSize)

-- Task 2

natFold :: (a->a) -> a -> Integer -> a
natFold  h  c  0 = c
natFold  h  c  n = h (natFold  h  c  (n-1))

g :: Integer -> Integer -> Integer 
g  n  m  =  natFold (*n) 1 m

iterate1 :: (a -> a) -> a -> [a]
iterate1 f a = a : (iterate1 f (f a))

{-
a)
  take 4 (iterate (g 2) 1)
= take 4 (1 : iterate (g 2) (g 2 1))
= take 4 (1 : 2 : iterate (g 2) (g 2 2))
= take 4 (1 : 2 : 4 : iterate (g 2) (g 2 4))
= take 4 (1 : 2 : 4 : 16 : iterate (g 2) (g 2 16))
= [1, 2, 4, 16]
Durch (iterate (g 2) 1) wird eine unendliche Sequenz erschaffen, die mit 1, 2 (g 2 1), 4 (g 2 2) und 16 (g 2 4) beginnt. Mit take 4 werden die ersten 4 Elemente eingezogen und so ergibt sich die Ergebnisliste [1,2,4,16].

b)
  ((foldl (+) 0) . (map (g 2))) [1..4]
= foldl (+) 0 (map (g 2) [1..4])
= foldl (+) 0 (map (g 2) [1,2,3,4])
= foldl (+) 0 ((g 2 1) : (map (g 2) [2,3,4]))
= foldl (+) 0 ((g 2 1) : (g 2 2) : (map (g 2) [3,4]))
= foldl (+) 0 ((g 2 1) : (g 2 2) : (g 2 3) : (map (g 2) [4]))
= foldl (+) 0 ((g 2 1) : (g 2 2) : (g 2 3) : (g 2 4) : (map (g 2) []))
= foldl (+) 0 ((g 2 1) : (g 2 2) : (g 2 3) : (g 2 4) : [])
= foldl (+) 0 (2 : 4 : 8 : 16 : [])
= foldl (+) 0 [2, 4, 8, 16]
= foldl (+) 2 [4, 8, 16]
= ...
= 30
Der Listengenerator [1..4] wird zur Liste [1,2,3,4]. Auf jedes dieser Elemente wird die Funktion (g 2) angewendet. Dadurch ergibt sich die Liste [2,4,8,16]. Diese Elemente werden durch (foldl (+) 0) der Reihe nach auf den Initialwert 0 addiert.
Quasi (0+2+4+8+16). Das Ergebnis ist 30.


c)
  length [x | xs<-["one", "two", "three"], x<-xs]
= length [x | x <- ("one" ++ "two" ++ "three")]
= length [x | x <- "onetwothree"]
= length "onetwothree"
= 11
Der Listengenerator konkatiniert die Listen "one", "two" und "three". Danach wird die Länge der neuen Liste errechnet. Das Ergebnis ist 11.
-}

primes = sieb [2..]
               where
                   sieb (p:xs) = p:sieb[k | k<-xs, (mod k p)>0]

primes3 :: Int -> [Integer]
primes3 n = take n (filter (\x -> (x `mod` 10) == 3) primes)


applyWhile :: (a -> b) -> (a -> Bool) -> [a] -> [b]
applyWhile _ _ [] = []
applyWhile f p (x:xs) | p x = (f x) : applyWhile f p xs
                      | otherwise = []

                      
nats :: Int -> [Int]
nats n = n : map (+1) (nats n)

-- (take n (map (*k) (nats 1)) -- n
-- (filter (\x -> x<n) ...) -- n*n
-- (take n ...) -- n (obwohl hier nie wirklich n Elemente genommen werden)
-- sum -- n
-- O(n + n*n + n + n) = O(3n + n^2) = O(n^2)
sumMultiple :: Int -> Int -> Int
sumMultiple k n = sum (take n (filter (\x -> x<n) (take n (map (*k) (nats 1)))))


filter2 :: ((a -> Bool), [a]) -> [a]
filter2 (_, []) = [] 
filter2 (p, (x:xs)) | p x = x : filter2 (p, xs)
                    | otherwise = filter2 (p, xs)



data Bit = One | Zero deriving Eq
type Bits = [Bit]

compress :: Bits -> [Integer]
compress bs = compress' bs 0 []
  where
    compress' []         _ _   = [0]
    compress' [b]        n acc = acc ++ [n + 1]
    compress' (b0:b1:bs) n acc | b0 == b1 = compress' (b1:bs) (n+1) acc
                               | otherwise = compress' (b1:bs) 0 (acc ++ [n + 1])



-- Die Komplexität ist konstant: O(1)
listLocalMins :: Ord a => [a] -> [a]
listLocalMins (x:y:z:ss) | y < x && y < z = y : listLocalMins (y:z:ss) -- O(1+1+1+1+1)
                         | otherwise = listLocalMins (y:z:ss) -- O(1)
listLocalMins _ = [] -- O(1)



--serie :: Double -> Int -> Double





unfold p f g x | p x = []
               | otherwise = f x : unfold  p  f  g  (g x)

{-
Zur Erinnerung:
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) g f x = (g (f x))


b)
  unfold (4<) ((*2).(`mod`3)) (1+) 1
= ((1 `mod` 3) * 2) : unfold (4<) ((*2).(`mod`3)) (1+) (1+1)
= ((1 `mod` 3) * 2) : (((1+1) `mod` 3) * 2) : unfold (4<) ((*2).(`mod`3)) (1+) ((1+1)+1)
= ((1 `mod` 3) * 2) : (((1+1) `mod` 3) * 2) : (((1+1+1) `mod` 3) * 2) : unfold (4<) ((*2).(`mod`3)) (1+) ((1+1)+1)
= ((1 `mod` 3) * 2) : (((1+1) `mod` 3) * 2) : (((1+1+1) `mod` 3) * 2) : (((1+1+1+1) `mod` 3) * 2) : unfold (4<) ((*2).(`mod`3)) (1+) ((1+1)+1+1)
= [2,4,0,2]
-- 









-}
