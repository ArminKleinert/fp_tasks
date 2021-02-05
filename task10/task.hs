{-# LANGUAGE NPlusKPatterns #-}

{- Abgabe von Anna Sophie Pipperr und Armin Kleinert -}


{- Aufgabe 1 -}

-- a)
-- TODO

pow3 = (\x -> 3**x)



-- b)
--fix = (\y -> (\x -> y (x x)) (\z -> y (z z)))

fix :: (t -> t) -> t
fix = (\x -> x (fix x))
-- Z.B.
-- cnt r n = if (n==0) then 0 else n+(r (n-1))
-- `(fix cnt) 6`

-- Eg. (fix reverse0) [1,2,3,4,5] => [5,4,3,2,1]
reverse0 :: [a] -> [a]
reverse0 = fix (\r xs -> if (null xs) then [] else (r (tail xs)) ++ [(head xs)])

{- Aufgabe 2 -}

collatz_help n
    | n `mod` 2 == 0 = n `div` 2
    | otherwise      = 3 * n + 1

collatz r n
    | n <= 0 = error "The number must be positive."
    | n == 1 = [n]
    | otherwise = n:r (collatz_help n)

{- Aufgabe 3 -}

-- a) 

-- Eg. length0 [1,2,3,4,5] => 5
length0 :: (Foldable t, Integral a1) => t a2 -> a1
length0 l = foldl (\r x -> r+1) 0 l

-- Eg. filter0 (even) [1,2,3,4,5] => [1,2]
filter0 :: Foldable t => (a -> Bool) -> t a -> [a]
filter0 p l = foldl (\r x -> if (p x) then r++[x] else r) [] l

-- b)
insertSort [] = [] 
insertSort xs = foldl (\r x -> insert x r) [] xs
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
I = Identitätsfunktion λx.x
K = λab.a
S = λxyz.xz(yz)

λs.λx.s(s(s(x))) ≡ (S(S(KS)K)(S(S(KS)K)I))

λs.λx.s(s(s(x))) 1 2
λx.1(1(1(x))) 2
1(1(1(2)))
(λs.λx.s(x))(1(1(1(2))))
(λx.(1(1(1(2))))(x))
(λx.((λsa.s(a))(1(1(2))))(x))
(λx.((λa.(1(1(2)))(a)))(x))
(λx.((λb.(1(1(2)))(b)))(x))
(λx.((λb.((λsc.s(c))(1(2)))(b)))(x))
(λx.((λb.((λc.(1(2))(c)))(b)))(x))
(λx.((λb.((λc.((λsd.s(d))(2))(c)))(b)))(x))
(λx.((λb.((λc.((λd.2(d)))(c)))(b)))(x))
(λx.((λb.((λc.((λd.(λsa.s(s(a)))(d)))(c)))(b)))(x))
(λx.((λb.((λc.((λd.(λa.d(d(a)))))(c)))(b)))(x))
(λx.((λb.((λc.(λa.c(c(a)))))(b)))(x))
(λx.((λb.((λa.b(b(a))))))(x))
(λx.((((λa.x(x(a)))))))
(λx.λa.x(x(a))) -- Semantisch equivalent zur 2

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

(λc.(λf.(λb.c)f((λy.c)f(cf)))) 1 2
((λb.1)2((λy.1)2(1(2))))
(1((λy.1)2(1(2))))
(1(1(1(2)))) -- Dieser Schritt kam auch bei 

-}

Kombinator: Freie Lambda-Abstraktion, die keine freien Variablen hat und das Lambda-Kalkül vereinfachen soll
- Lambda-Abstraktion
- Keine freien Variablen
- Primitiv


(S(S(KS)K)(S(S(KS)K)I)) 1 2
((λabc.ac(bc))(S(KS)K)(S(S(KS)K)I)) 1 2
(λc.(S(KS)K)c((S(S(KS)K)I)c)) 1 2
(λc.((λdef.df(ef))(KS)K)c((S(S(KS)K)I)c)) 1 2
(λc.(λf.(KS)f(Kf))c((S(S(KS)K)I)c)) 1 2
(λc.((KS)c(Kc))((S(S(KS)K)I)c)) 1 2
(λc.((λb.S)c(λd.c))((S(S(KS)K)I)c)) 1 2
(λc.(S(λd.c))((S(S(KS)K)I)c)) 1 2
(λc.((λefg.eg(fg))(λd.c))((S(S(KS)K)I)c)) 1 2
(λc.(λfg.(λd.c)g(fg))((S(S(KS)K)I)c)) 1 2
(λc.(λfg.fg)((S(S(KS)K)I)c)) 1 2
(λc.(λg.((S(S(KS)K)I)c)g)) 1 2
(((S(S(KS)K)I)1)2)
((((λxyz.xz(yz))(S(KS)K)I)1)2)
(((λz.(S(KS)K)z(Iz)) 1) 2)
(((λz.(S(KS)K)zz) 1) 2)
(((λz.((λabc.ac(bc))(KS)K)zz) 1) 2)
(((λz.(λc.(KS)c(Kc))zz) 1) 2)
(((λz.(KS)z(Kz)z) 1) 2)
(((λz.((λab.a)S)z((λab.a)z)z) 1) 2)
(((λz.(λb.S)z(λb.z)z) 1) 2)
(((λz.S(λb.z)z) 1) 2)
(((λz.(λdef.df(ef))(λb.z)z) 1) 2)
(((λz.(λf.(λb.z)f(zf))) 1) 2)
(((λz.(λf.z(zf))) 1) 2)
