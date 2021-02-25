{-# LANGUAGE NPlusKPatterns #-}

{-
-- 1) Reduzieren.

cross ((mod 2), (==1).(div 9)) (4,7)
apply ((mod 2) . fst, (==1).(div 9).snd) (4,7)
(((mod 2) . fst) (4,7), ((==1).(div 9).snd)(4,7))
((mod 2 (fst (4,7))), ((==1).(div 9)) (snd(4,7)))
((mod 2 4), ((==1).(div 9)) 7)
((mod 2 4), ((div 9 7)==1))
(2, (1==1))
(2, True)
-}

{-
foldl (\ys x-> x:ys) [] (take 3 [1..])
foldl (\ys x-> x:ys) [] [1,2,3]
foldl (\ys x-> x:ys) 1:[1] [2,3]
foldl (\ys x-> x:ys) 2:[1] [3]
foldl (\ys x-> x:ys) 3:[2,1] []
[3,2,1]
-}

-- 2)

zipWith1 :: ((a->b->c), [a], [b]) -> [c]
zipWith1 (f, (x:xs), (y:ys)) = (f x y) : zipWith1 (f, xs, ys)
zipWith1 _ = []

zipWithLg :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithLg f xs ys = [f (xs !! i) (ys !! i) | i <- [0 .. (min (length xs) (length ys)) - 1]]

-- 3)

freq :: (Eq a) => a -> [a] -> Int
freq e xs = foldl (\x y -> x + (if (y==e) then 1 else 0)) 0 xs

-- 4)

type Set = [Int]

-- O(n)
inSet :: Int -> Set -> Bool
inSet e  []  = False -- 1
inSet e (x:xs) | e == x = True -- 1
               | otherwise = inSet e xs -- 1 -> N

(\\) :: Set -> Set -> Set
(\\) setA setB = [x| x<-setA, not (inSet x setB)]

