f :: (Num a) => a -> a
f n = let  a = 2*n
           b = a + ( let  a = 1
                          b = 1
                      in  a+b )
         in  b*a


{- 
f 1 -- a = 2*1 b = a+1+1
((a=(2 * n)) + ((a=1)+(b=1))) * (2*n)
((2*n) + (1+1)) * (2*n)
((2*1) + (1+1)) * (2*1)
(2 + 2) * 2
8
-}

{-
take 5 (iterate (*2) 1)
take 5 [(1*2), ((2*2)*1), (((2*2)*2)*1), ((((2*2)*2)*2)*1), (((((2*2)*2)*2)*2)*1), ...]
take 5 [2, 2*2, 2*2*2, 2*2*2*2, 2*2*2*2*2, ...]
take 5 [2, 4, 4*2, 4*2*2, 4*2*2*2, ...]
take 5 [2, 4, 8, 8*2, 8*2*2, ...]
take 5 [2, 4, 8, 16, 16*2, ...]
take 5 [2, 4, 8, 16, 32, ...]
[2, 4, 8, 16, 32]
-}

{-
(filter ((<2) . (mod 5)) . map (^2)) [2,1,4,1]
-- TODO
-}

subList :: [a] -> Int -> Int -> [a]
subList xs m n = take n (drop m xs)

applyUntil :: (a -> b) -> (a -> Bool) -> [a] -> [b]
applyUntil _ _ [] = []
applyUntil f p (x:xs) = if (p x) then [] else (f x) : applyUntil f p xs

maxSurfaces :: Int -> Int
maxSurfaces 0 = 1
maxSurfaces n = maxSurfaces (n - 1) + n

maxSurfacesTr :: Int -> Int
maxSurfacesTr n = maxSurfacesTr' n 0  
  where maxSurfacesTr' 0 acc = acc + 1
        maxSurfacesTr' n acc = maxSurfacesTr' (n-1) (acc + n)

-- Die Endrekursive Version läuft wesentlich schneller.

freq :: Eq t => t -> [t] -> Integer
freq x []     = 0
freq x (y:ys) | x == y = 1 + (freq x ys)
              | otherwise = freq x ys

-- Un-currifiziertes map
map2 :: ((a -> b), [a]) -> [b]
map2 (_, [])     = []
map2 (f, (x:xs)) = (f x) : map2 (f, xs)

--

sumPowerTwo :: Integer -> Integer
sumPowerTwo n = foldl (+) 0 (map (^2) [1 .. n])

--
{-
w = max( R/255, G/255, B/255)
C = (w - (R/255))/w
M = (w - (G/255))/w
Y = (w - (B/255))/w
K = 1 - w
-}

maxDouble :: Double -> Double -> Double -> Double
maxDouble d0 d1 d2 | d0 > d1 && d0 > d2 = d0
                   | d1 > d0 && d1 > d2 = d1
                   | otherwise = d2

selfOrZeroIfNaN :: Double -> Double
selfOrZeroIfNaN n = if n /= n then 0 else n

rgb2cmyk :: (Int,Int,Int) -> (Double,Double,Double,Double)
rgb2cmyk (r,g,b) = let rf = (fromIntegral r) / 255.0
                       gf = (fromIntegral g) / 255.0
                       bf = (fromIntegral b) / 255.0
                       w = maxDouble rf gf bf
                       c = selfOrZeroIfNaN ((w - rf) / w)
                       m = selfOrZeroIfNaN ((w - gf) / w)
                       y = selfOrZeroIfNaN ((w - bf) / w)
                       k = 1.0 - w
                   in (c,m,y,k)

--

-- O(n)
delete :: Eq t => t -> [t] -> [t]
delete _ []                 = []
delete x (y:ys) | x == y    = ys
                | otherwise = y : delete x ys

-- O(n)
maxByComp :: (t -> t -> Bool) -> t -> [t] -> t
maxByComp _ x []     = x
maxByComp c x (y:ys) | (c x y)   = maxByComp c x ys
                     | otherwise = maxByComp c y ys
          
          
-- O(n^2)
selectionSort' :: Ord t => (t -> t -> Bool) -> [t] -> [t] -> [t]
selectionSort' _ sorted [] = sorted
selectionSort' c sorted unsorted = selectionSort' c ([mx] ++ sorted) (delete mx unsorted) -- O(n^2)
  where mx = maxByComp (\x y -> not (c x y)) (head unsorted) (drop 1 unsorted) -- O(n)

-- O(n^2)
selectSort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
selectSort c xs = selectionSort' c [] xs

-- O(n^2 + 1) = O(n^2)
calculateFirst :: Ord a => (a -> a -> Bool) -> [a] -> a
calculateFirst c xs = head (selectSort c xs)

-- O(n^2 + n) = O(n^2)
deleteElem :: Ord t => (t -> t -> Bool) -> [t] -> [t]
deleteElem c xs = delete (calculateFirst c xs) xs
