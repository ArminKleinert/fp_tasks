{-# LANGUAGE NPlusKPatterns #-}

{- Abgabe von Anna Sophie Pipperr und Armin Kleinert -}


{- Aufgabe 1 -}

-- a)
pow3 :: Double -> Double
pow3 = \x -> 3**x

fun :: (t1 -> t2) -> (t3 -> t4) -> (t4 -> t1) -> t3 -> t2
fun f g h = \x -> f (h (g x))

remove :: (a -> Bool) -> [a] -> [a]
remove f = filter (not . f)

-- b)´

reverse2 :: Foldable t => t a -> [a]
reverse2 l = foldl (\x y -> [y] ++ x) [] l

{- Aufgabe 2 -}

-- Z.B.
-- cnt r n = if (n==0) then 0 else n+(r (n-1))
-- `(fix cnt) 6`
fix :: (t -> t) -> t
fix = (\x -> x (fix x))

collatz_help :: Integral a => a -> a
collatz_help n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise      = 3 * n + 1

collatz :: Integral t => (t -> [t]) -> t -> [t]
collatz r n
    | n <= 0 = error "The number must be positive."
    | n == 1 = [n] -- Anker
    | otherwise = n:r (collatz_help n)

{- Aufgabe 3 -}

-- a) 

-- Eg. length0 [1,2,3,4,5] => 5
length0 :: (Foldable t, Integral a1) => t a2 -> a1
length0 l = foldl (\r _ -> r+1) 0 l

-- Eg. filter0 (even) [1,2,3,4,5] => [1,2]
filter0 :: Foldable t => (a -> Bool) -> t a -> [a]
filter0 p l = foldl (\r x -> if (p x) then r++[x] else r) [] l

-- b)

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldl (\r x -> insert x r) []
  where                        
    insert a []    = [a]
    insert a (b:y) | a<= b     = a:(b:y)                                            
                   | otherwise = b:(insert a y)

{- Aufgabe 4 -}

-- a)

{-
I = Identitätsfunktion λx.x
K = λab.a
S = λxyz.xz(yz)

S(KK)I ≡ K
(λxyz.xz(yz))((λab.a)(λab.a))(λx.x) ≡ λab.a
(λxyz.xz(yz))(λb.(λab.a))(λx.x) ≡ λab.a
(λxyz.xz(yz))(λb.(λcd.c))(λx.x) ≡ λab.a
(λxyz.xz(yz))(λbcd.c)(λx.x) ≡ λab.a
(λyz.(λbcd.c)z(yz))(λx.x) ≡ λab.a
(λz.(λbcd.c)z((λx.x)z)) ≡ λab.a
(λz.(λbcd.c)zz) ≡ λab.a
(λz.(λcd.c)z) ≡ λab.a
(λz.(λd.z)) ≡ λab.a
(λzd.z) ≡ λab.a
λab.a ≡ λab.a
-}

-- b)

{-
SS(SI(K(KI)))(KK(S(KK)I))(KI) -- Ursprüngliche Formel
SS(SI(K(KI)))(K)(KI) -- (KK(S(KK)I)) zu K auflösen, da K nur das 1. Argument behält
((SK)((SI(K(KI)))K))(KI) -- (SS(SI(K(KI)))(K)) erweitern
((SK)(((λxyz.xz(yz))I(K(KI)))K))(KI) -- 
((SK)(((λyz.(Iz)(yz))(K(KI)))K))(KI) -- 
((SK)(((λyz.z(yz))(K(KI)))K))(KI) -- 
((SK)((λz.z((K(KI))z))K))(KI) -- 
((SK)(K((K(KI))K)))(KI) -- 
((SK)(K(KI)))(KI) -- 
((SK)((λb.(KI))))(KI) -- 
((SK)((λb.(KI))))(λa.I) -- 
((λyz.Kz(yz))(λb.(KI)))(λa.I)
((λyz.z)(λb.(KI)))(λa.I)
(λz.z)(λa.I)
(λa.I)
(λa.(λx.x))
(λax.x)
-}

{- Aufgabe 5 -}

{-
T[λx.y(xy)]                     ≡ S(Ky)(SI(Ky))
elim. x [y (xy)]                ≡ S(Ky)(SI(Ky)) -- Regel 2
S(elim. x [y]) (elim. x [(xy)]) ≡ S(Ky)(SI(Ky)) -- Regel 7
S(Ky)(elim. x [xy])             ≡ S(Ky)(SI(Ky)) -- Regel 4
S(Ky)(S(elim. x [x])(elim. x [y])) ≡ S(Ky)(SI(Ky)) -- Regel 7
S(Ky)(SI(elim. x [y]))          ≡ S(Ky)(SI(Ky)) -- Regel 3
S(Ky)(SI(Ky))                   ≡ S(Ky)(SI(Ky)) -- Regel 4
-}

{- Aufgabe 6 -}

{-
λs.λx.s(s(s(x))) ≡ (S(S(KS)K)(S(S(KS)K)I))

λs.λx.s(s(s(x))) 1 2
λx.1(1(1(x))) 2
1(1(1(2)))

(S(S(KS)K)(S(S(KS)K)I)) 1 2
((λabc.ac(bc))(S(KS)K)(S(S(KS)K)I)) 1 2
(λc.(S(KS)K)c((S(S(KS)K)I)c)) 1 2
(λc.((λxyz.xz(yz))(KS)K)c((S(S(KS)K)I)c)) 1 2
(λc.(λz.(KS)z(Kz))c((S(S(KS)K)I)c)) 1 2
(λc.(λz.(KS)z(Kz))c(((λxyz.xz(yz))(S(KS)K)I)c)) 1 2
(λc.(λz.(KS)z(Kz))c((S(KS)K)c(Ic))) 1 2
(λc.(λz.(KS)z(Kz))c((S(KS)K)cc)) 1 2
(λc.(λz.(KS)z(Kz))c(((λxyz.xz(yz))(KS)K)cc)) 1 2
(λc.(λz.(KS)z(Kz))c((λz.(KS)z(Kz))cc)) 1 2
(λc.(λz.(KS)z(Kz))c(((KS)c(Kc))c)) 1 2
(λc.(λz.(KS)z(Kz))c((((λab.a)S)c(Kc))c)) 1 2
(λc.(λz.(KS)z(Kz))c(((λb.S)c(Kc))c)) 1 2
(λc.(λz.(KS)z(Kz))c((S(Kc))c)) 1 2
(λc.(λz.(KS)z(Kz))c(((λdef.df(ef))(Kc))c)) 1 2
(λc.(λz.(KS)z(Kz))c(λf.(Kc)f(cf))) 1 2
(λc.(λz.((λab.a)S)z((λab.a)z))c(λf.(Kc)f(cf))) 1 2
(λc.(λz.(λb.S)z(λb.z))c(λf.(Kc)f(cf))) 1 2
(λc.(λz.(λb.(λdef.df(ef)))z(λb.z))c(λf.(Kc)f(cf))) 1 2
(λc.(λz.(λbdef.df(ef))z(λb.z))c(λf.(Kc)f(cf))) 1 2
(λc.(λz.(λbdef.df(ef))z(λb.z))c(λf.((λab.a)c)f(cf))) 1 2
(λc.(λz.(λbdef.df(ef))z(λb.z))c(λf.(λb.c)f(cf))) 1 2
(λc.(λz.(λef.(λb.z)f(ef)))c(λf.(λb.c)f(cf))) 1 2
(λc.(λef.(λb.c)f(ef))(λx.(λy.c)f(cf))) 1 2 -- Umbennung
(λc.(λf.(λb.c)f((λx.(λy.c)f(cf))f))) 1 2
(λc.(λf.(λb.c)f((λy.c)f(cf)))) 1 2
((λb.1)2((λy.1)2(1(2))))
(1((λy.1)2(1(2))))
(1(1(1(2))))

1(1(1(2))) ≡ 1(1(1(2)))

-}


