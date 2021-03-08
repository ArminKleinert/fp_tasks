{-# LANGUAGE NPlusKPatterns #-}
{-
- 1 möglich 6
- 2 meh 10
- 3 meh 6
- 4 yay 10
- 5 yay 10
- 6 möglich 14
- 7 nope 12 (vllt. 1)
- 8 yay 10
- 9 möglich 10
- 10 yay 12
-}

{-
Tranformationsregeln:
T[λx.λy.xy] = I
T[λx.T[λy.xy]] = I -- Tranformationsregel 7
T[λx.T[x]] = I -- Transformationsregel 5
T[λx.x] = I -- Transformationsregel 1
I = I -- Transformationsregel 3

Eliminierungsregeln:
λx.λy.xy = I
T[λx.λy.xy] = I -- 
elim. x [λy.xy] = I -- 2
elim. x [elim. y [xy]] = I -- 5
elim. x [T[x]] = I -- 6
elim. x [x] = I -- 0
I = I -- 3
-}

-- 1.
{-
iterate1 :: (a -> a) -> a -> [a]
iterate1 f a = a : iterate1 f (f a) 

foldr (+) 1 (map (*3) (take 5 (iterate (*2) 1)))
foldr (+) 1 (map (*3) (take 5 (1: (iterate (*2) (1*2)))))
foldr (+) 1 (map (*3) (take 5 (1:(2:(iterate (*2) (2*2))))))
foldr (+) 1 (map (*3) (take 5 (1:(2:(4:(iterate (*2) (4*2)))))))
foldr (+) 1 (map (*3) (take 5 (1:(2:(4:(8:(iterate (*2) (8*2))))))))
foldr (+) 1 (map (*3) (take 5 (1:(2:(4:(8:(16:(iterate (*2) (16*2)))))))))
foldr (+) 1 (map (*3) [1,2,4,8,16)) -- nur 5 Elemente wegen take 5
foldr (+) 1 ((1*3) : (map (*3) [2,4,8,16]))
... -- map (*3) multipliziert alle Elemente mit 3
foldr (+) 1 [3,6,12,24,48]
((+) 3 (foldr (+) 1 [6,12,24,48]))
... -- Reduktion via foldr
94 -- foldr (+) 1 hat alle Elemente mit Startwert 1 summiert
-}

-- 2.

rects :: ((Int, Int), Int) -> Char
rects ((x,y), size) | isO = 'O'
                    | isDot = '.'
                    | otherwise = ' '
  where 
    third = (div size 3)
    isO = ((x <= third) || (x > (size-third))) && ((y < third) || (y > (size-third)))
    sixth = div third 2
    isDot = ((y >= sixth) && (y < (size-sixth))) || ((x >= sixth) && (x < (size-sixth)))
  
{-
Korrektur:
rects :: ((Int, Int), Int) -> Char
rects ((x,y), size) | isO = 'O'
                    | isDot = '.'
                    | otherwise = ' '
  where 
    third = (div size 3)
    isO = ((x <= third) || (x > (size-third))) && ((y < third) || (y > (size-third)))
    sixth = div third 2
    isDot = ((y >= sixth) && (y < (size-sixth))) && ((x >= sixth) && (x < (size-sixth)))
-}

printPicture f size = putStrLn (genChars f size)
genChars f size = matrix size (map f [((x,y),size) | y <- [1..size], x <- [1..size]])
                  where
                  matrix 0  []     = []
                  matrix 0 (c:cs)  = '\n' : (matrix size (c:cs))
                  matrix n (c:cs)  = c: (matrix (n-1) cs)

-- 3.

-- cos1 :: Float a => a -> a -> a

cos1 ::  Float -> Integer -> Float
cos1 x 0 = 0
cos1 x n = (((fromIntegral((-1)^n)) / (fromIntegral (fact (2*n)))) * (x ** (fromIntegral (2*n))))-- + (cos1 x (n-1))
  where
    fact 0 = 1
    fact n = n * fact (n - 1)
--

-- 4.

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) ->  a -> [b]
unfold p f g x | p x = []
               | otherwise = f x : unfold p f g (g x)


{-
unfold (\x -> x == 0) (\x -> mod x 2) (\x -> div x 2) 23

(mod 23 2) : unfold (\x -> x == 0) (\x -> mod x 2) (\x -> div x 2) (div 23 2)
(mod 23 2) : unfold (\x -> x == 0) (\x -> mod x 2) (\x -> div x 2) 11

(mod 23 2) : (mod 11 2) : unfold (\x -> x == 0) (\x -> mod x 2) (\x -> div x 2) (div 11 2)
(mod 23 2) : (mod 11 2) : unfold (\x -> x == 0) (\x -> mod x 2) (\x -> div x 2) 5

(mod 23 2) : (mod 11 2) : (mod 5 2) : unfold (\x -> x == 0) (\x -> mod x 2) (\x -> div x 2) (div 5 2)
(mod 23 2) : (mod 11 2) : (mod 5 2) : unfold (\x -> x == 0) (\x -> mod x 2) (\x -> div x 2) 2

(mod 23 2) : (mod 11 2) : (mod 5 2) : (mod 2 2) : unfold (\x -> x == 0) (\x -> mod x 2) (\x -> div x 2) (div 2 2)
(mod 23 2) : (mod 11 2) : (mod 5 2) : (mod 2 2) : unfold (\x -> x == 0) (\x -> mod x 2) (\x -> div x 2) 1

(mod 23 2) : (mod 11 2) : (mod 5 2) : (mod 2 2) : (mod 1 2) : unfold (\x -> x == 0) (\x -> mod x 2) (\x -> div x 2) (div 1 2)
(mod 23 2) : (mod 11 2) : (mod 5 2) : (mod 2 2) : (mod 1 2) : unfold (\x -> x == 0) (\x -> mod x 2) (\x -> div x 2) 0

(mod 23 2) : (mod 11 2) : (mod 5 2) : (mod 2 2) : (mod 1 2) : []
(mod 23 2) : (mod 11 2) : (mod 5 2) : (mod 2 2) : [1]
(mod 23 2) : (mod 11 2) : [1,0,1]
(mod 23 2) : [1,1,0,1]
[1,1,1,0,1]

-}

-- 5.

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p [] = []
dropUntil p (x:xs) | p x = x:xs -- O(1)
                   | otherwise = dropUntil p xs -- O(n)
                   
-- 6.

-- O(n*log(n))
-- separateAt ist logarithmisch, da die kontrollierte Menge mit jedem Aufruf von separateAt weiter abnimmt.
separateAt :: Eq a => [a] -> a -> [[a]]
separateAt [] e = [] -- O(1)
separateAt xs e = (fst inter) : separateAt (snd (inter)) e -- O(log(n)*n)
  where
    inter = sub xs e -- T(sub)
    
    sub [] _ = ([],[]) --O(1)
    sub (x:xs) e | x == e = ([],xs) -- O(1)
                 | otherwise = let temp = (sub xs e) -- O(n)
                               in (x : (fst temp), snd temp) -- O(1)

-- 8.

data HTree = Leaf Char Int | Node Int HTree HTree
  deriving (Show)

pfadlength :: HTree -> Integer
pfadlength (Leaf _ _) = 0
pfadlength t@(Node _ tl tr) = (pfadlength tl) + (pfadlength tr) + (height t) - 1

height :: HTree -> Integer
height (Leaf _ _) = 1
height (Node _ tl tr) = (max (height tl) (height tr)) + 1
{-
(Node 1 
  (Node 2 
    (Node 3 (Leaf ' ' 0) (Leaf ' ' 0))
    (Node 3 (Node 3 (Leaf ' ' 0) (Leaf ' ' 0)) (Node 3 (Leaf ' ' 0) (Leaf ' ' 0))))
  (Node 3
    (Node 3 (Leaf ' ' 0) (Leaf ' ' 0))
    (Node 3 (Node 3 (Leaf ' ' 0) (Leaf ' ' 0)) (Leaf ' ' 0))))
    -}
    
--(Node 1  (Node 2 (Node 3 (Leaf ' ' 0) (Leaf ' ' 0)) (Node 3 (Node 3 (Leaf ' ' 0) (Leaf ' ' 0)) (Node 3 (Leaf ' ' 0) (Leaf ' ' 0)))) (Node 3 (Node 3 (Leaf ' ' 0) (Leaf ' ' 0))(Node 3 (Node 3 (Leaf ' ' 0) (Leaf ' ' 0)) (Leaf ' ' 0))))
-- π(t) = π(tl) + π(tr) + |t| −1


{-
9. 

a)

(λxy.xy(λxy.y))(λxy.x)(λxy.x)(λxy.x) 
((λxy.x)(λxy.x)(λxy.y))(λxy.x) -- Setze (λxy.x)(λxy.x) in (λxy.xy(λxy.y)) ein
((λxy.x))(λxy.x) -- Löse ((λxy.x)(λxy.x)(λxy.y)) auf. Wie bei TTF bleibt nur der 2. Ausdruck (λxy.x) stehen
((λxz.x))(λxy.x) -- Umbenennung zur Verständlichkeit
(λz.(λxy.x)) -- Setze (λxy.x) in (λxz.x) ein.
(λzxy.x) -- Lösung

b)
F ≡ λxy.y
T ≡ λxy.x
S ≡ λnab.a(nab)
Z ≡ λn.n (λx.F) T

λrn.(Zn) 0 ((Z(Pn)) 2 (S({SUB} (M (r (Pn)) 4) (r ({SUB} n 2)))))
-}

-- 10.

{-
Mittels Transformationsregeln:
λx.λy.zzy ≡ K(zz)
T[λx.λy.zzy] ≡ K(zz)
T[λx.T[λy.zzy]] ≡ K(zz) -- 7
T[λx.T[zz]] ≡ K(zz) -- Regel 5
T[λx.(T[z] T[z])] ≡ K(zz) -- Regel 2
T[λx.zz] ≡ K(zz) -- Regel 1
(K T[zz]) ≡ K(zz) -- Regel 4
(K (T[z] T[z])) ≡ K(zz) -- Regel 2
K (zz) ≡ K(zz) -- Regel 1

b)
-- Wende 5 und 4 als Argumente an.
(λx.λy.zzy) 5 4 ≡ K(zz) 5 4
-- 5 wird in x geschrieben und verschwindet. K absorbiert das Argument auch:
(λy.zzy) 4 ≡ (zz) 4
-- 4 wird links in y eingesetzt.
zz 4 ≡ (zz) 4
-- Klammern können wegen der Assoziativität aufgelöst werden:
zz 4 ≡ zz 4
-}











