{- Abgabe von Anna Sophie Pipperr und Armin Kleinert -}

{- Aufgabe 1 -}

-- Die Definition von const ist
-- const x _ = x
--
-- 1 zu 1 im Lambda-Kalkül:
-- λxy.x

{- Aufgabe 2 -}

{-
Formel:
    (λn . (λg . gng) (λng. Zn 1 (S ((λxya.x(y a)) 3 (g (1Pn) g)))))

(λg . gng) wendet eine Funktion g auf n und g an, erzeugt also Rekursion für g.
(λxya.x(y a)) ist die Multiplikation.
(g (1Pn) g) wendet g auf den Vorgänger von n und g an.


Test mit 2
(λn . (λg . gng) (λng. Zn 1 (S ((λxya.x(y a)) 3 (g (1Pn) g))))) 2
((λg . g2g) (λng. Zn 1 (S ((λxya.x(y a)) 3 (g (1Pn) g)))))
((λng. Zn 1 (S ((λxya.x(y a)) 3 (g (1Pn) g)))) 2 g) -- Funktion ab jetzt auch "g" genannt, um es kurz zu halten.

(Z2 1 (S ((λxya.x(y a)) 3 (g (1P2) g)))) -- Z2 ist F und 1P2 ist 1
(F 1 (S ((λxya.x(y a)) 3 (g 1 g)))) -- (F 1 ...) kann gekürzt werden
(S ((λxya.x(y a)) 3 (g 1 g))) -- (g 1 g) erweitern:
(S ((λxya.x(y a)) 3 ((λng. Zn 1 (S ((λxya.x(y a)) 3 (g (1Pn) g)))) 1 g)))

(S ((λxya.x(y a)) 3 ((λng. Zn 1 (S ((λxya.x(y a)) 3 (g (1Pn) g)))) 1 g))) -- Argumente anwenden:
(S ((λxya.x(y a)) 3 (Z1 1 (S ((λxya.x(y a)) 3 (g (1P1) g)))))) -- Z1 und 1Pn kürzen:
(S ((λxya.x(y a)) 3 (F 1 (S ((λxya.x(y a)) 3 (g 0 g))))))
(S ((λxya.x(y a)) 3 (S ((λxya.x(y a)) 3 (g 0 g))))) -- g wieder einsetzen:
(S ((λxya.x(y a)) 3 (S ((λxya.x(y a)) 3 ((λng. Zn 1 (S ((λxya.x(y a)) 3 (g (1Pn) g)))) 0 g)))))

(S ((λxya.x(y a)) 3 (S ((λxya.x(y a)) 3 ((λng. Zn 1 (S ((λxya.x(y a)) 3 (g (1Pn) g)))) 0 g))))) -- Argumente einsetzen:
(S ((λxya.x(y a)) 3 (S ((λxya.x(y a)) 3 (Z0 1 (S ((λxya.x(y a)) 3 (g (1P0) g)))))))) -- Z0 wird zu T
(S ((λxya.x(y a)) 3 (S ((λxya.x(y a)) 3 (T 1 (S ((λxya.x(y a)) 3 (g (1P0) g)))))))) -- durch T bleibt nur die 1:
(S ((λxya.x(y a)) 3 (S ((λxya.x(y a)) 3 1))))

(S ((λxya.x(y a)) 3 (S ((λxya.x(y a)) 3 1)))) -- Multiplikation 3 mit 1:
(S ((λxya.x(y a)) 3 (S ((λxya.x(y a)) (λbv.b(b(b(v)))) (λcu.c(u))))))
(S ((λxya.x(y a)) 3 (S ((λya.(λbv.b(b(b(v))))(y a)) (λcu.c(u))))))
(S ((λxya.x(y a)) 3 (S ((λa.(λbv.b(b(b(v))))((λcu.c(u)) a))))))

(S ((λxya.x(y a)) 3 (S ((λa.(λbv.b(b(b(v))))(λu.a(u)))))))
(S ((λxya.x(y a)) 3 (S ((λa.(λv.(λu.a(u))((λu.a(u))((λu.a(u))(v)))))))))
(S ((λxya.x(y a)) 3 (S ((λa.(λv.(λu.a(u))((λu.a(u))(a(v)))))))))
(S ((λxya.x(y a)) 3 (S ((λa.(λv.(λu.a(u))(a(a(v)))))))))
(S ((λxya.x(y a)) 3 (S ((λa.(λv.(a(a(a(v))))))))))
(S ((λxya.x(y a)) 3 (S ((λav.(a(a(a(v))))))))) -- (λav.(a(a(a(v))))) ist semantisch gleich 3
(S ((λxya.x(y a)) 3 (S 3)))

(S ((λxya.x(y a)) 3 4))
(S 12)
13 -- <- Ergebnis

-}
(λav.a(a(a(a(v)))))

{- Aufgabe 3 -}

{-
a)

-- Parameter z: Die Zahl
-- Parameter f: Parameter für die ganze Zahl
λzf . f (z (λab . b)) (z (λab . a))

λzf . (z F) (z T) -- <- In Kurzschreibweise

Beispiel mit 1 (λz.z 0 1):
(λzf . f (z (λab . b)) (z (λab . a))) (λz.z 0 1)
(λf . f ((λz.z 0 1) (λab . b)) ((λz.z 0 1) (λab . a)))
(λf . f (1) (0))
(λf . f 1 0) -- <- Umkehrung von (λf . f 0 1), also Umkehrung von 1, also (-1)
-}

{-
b)


-}

{-
c)

λzf . f () ()
-}

{- Aufgabe 4 -}



{- Aufgabe 5 -}



{- Aufgabe 6 -}



{- Aufgabe 7 -}






{-
Extras:

Multiplikation
(λxya.x(y a)) 2 2
(λa.(λbv.b(b(v)))((λbu.b(b(u))) a))
(λa.(λbv.b(b(v)))(λu.a(a(u))))
(λa.(λv.(λu.a(a(u)))((λu.a(a(u)))(v))))
(λav.(λu.a(a(u)))(a(a(v))))
(λav.a(a(a(a(v)))))
-}

