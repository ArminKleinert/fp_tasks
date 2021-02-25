{-# LANGUAGE NPlusKPatterns #-}

{-
2. 
a) length [(x,y) | x<-[1..5], y<-[5,4..1], x>y]
length [(2,1),(3,2),(3,1),(4,3),(4,2),(4,1),(5,4),(5,3),(5,2),(5,1)]
1+length[(3,2),(3,1),(4,3),(4,2),(4,1),(5,4),(5,3),(5,2),(5,1)]
1+1+length[(3,1),(4,3),(4,2),(4,1),(5,4),(5,3),(5,2),(5,1)]
1+1+8
10

b)
((foldr (+) 5).(map (\x -> div x 3))) [1..8]
(foldr (+) 5 (map (\x -> div x 3))) [1..8]
(foldr (+) 5 (map (\x -> div x 3) [1..8]))
(foldr (+) 5 (0 : map (\x -> div x 3) [2,3,4,5,6,7,8]))
(foldr (+) 5 [0,0,1,1,1,2,2,2])
(foldr ((+) ((+) 5 0)) [0,1,1,1,2,2,2])
14
-}

-- 3.

primes = sieb [2..]
  where
    sieb (p:xs) = p:sieb[k | k<-xs, (mod k p)>0]

primeOne n = take n (filter (\x -> ((mod x 10) == 1)) (primes))

-- O(n**2)
max1 [x]   = x -- 1
max1 (x:xs) | all1 (<=x) xs = x -- O(n) => O(n**n)
            | otherwise    = max1 xs -- O(1)

-- O(n)
all1 p xs = foldl (&&) True (map p xs) -- T(map) => O(n)

-- O(n)
max2 [x] = x -- 1
max2 (a:b:xs) = aux a (b:xs) -- T(aux)
  where
    aux a [] = a -- 1
    aux a (b:xs) | a>b = aux a xs -- 1 => n
                 | otherwise = aux b x -- 1 => n

{-
I ≡ Identitätsfunktion λx.x
K ≡ λab.a
S ≡ λxyz.xz(yz)

9.

b)

SI(KIS)(SKI)K
SI(I)(SKI)K | Auflösung KIS
(I(SKI)((I)(SKI)))K | Ersetzung für SI(..)(..)
((SKI)((I)(SKI)))K | Auflösung für I(SKI) durch Definition von I
((SKI)(SKI))K | Auflösung für ((I)(SKI)) durch Definition von I
(SKI)(SKI)K | Auflösung der Klammern, da sonst eine weitere Reduktion schwierig wird
(K(SKI)(I(SKI)))K | Ausweitung (SKI)(SKI) durch Definition von S
(SKI)K | K in (K(SKI)(I(SKI))) behält nur das erste Element (SKI)
(KK(IK)) | Auflösung (SKI)K
(KK(K)) | Anwendung von I
K | K bhält nur das erste Argument

c)

λx.λy.(xx) ≡ S(KK)((SI)I)
T[λx.λy.(xx)] 
= T[λx.T[λy.(xx)]] | Regel 7
= T[λx.K T[(xx)]] | Regel 4
= T[λx.K T[x] T[x]] | 2
= T[λx.K xx] | 1 und 1
= S T[λx.K] T[λx.xx] | 6
= S T[λx.K] (S T[λx.x] T[λx.x]) | 6
= S (K T[K]) (S T[λx.x] T[λx.x]) | 4
= S (KK) (S T[λx.x] T[λx.x]) | 1
= S (KK) (S I I) | 3
= S (KK) ((SI)I)
-}




