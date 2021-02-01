{- Abgabe von Anna Sophie Pipperr und Armin Kleinert -}

{-
Genutzte Formeln:
∧ ≡ λx y . x y F 
∨ ≡ λxy.xTy
N ≡ Formel aus 3b zur "Normalisierung" einer ganzen Zahl

E ≡ (λxy.∧(Z(xPy))(Z(yPx))) -- Check auf Gleichheit aus Vorlesung 18
G ≡ (λxy.Z(xPy)) -- Formel für (>=) aus Vorlesung 18
¬ ≡ λx.xFT -- Boolsche Negation aus Vorlesung 18
< ≡ (λxy.∧ (Z(yPx)) (¬(E x y))) -- Aus Übung 8. Wir gehen hier von der Richtigkeit aus, da die Abgabe noch nicht bewertet wurde.
{CMP} ≡ Hilfsfunktion aus 5. (nimmt 2 Werte, vergleicht sie und gibt 1 für >, 0 für = oder -1 für < zurück)
{>=} ≡ (Z(yPx))

{PAIR} ≡ λxy.λz.zxy
{LIST2}≡ λxy.λf.fx({PAIR}y{NIL})
{LIST3}≡ λxyz.λf.fx({PAIR}y({PAIR}z{NIL}))
{NIL}  ≡ λx.xTFF
{NIL?} ≡ {TNIL} ≡ λx.x(λabc.a)
{HEAD} ≡ λx.x(λabc.b)
{TAIL} ≡ λx.x(λabc.c)
{LEN}  ≡ λrx.{TNIL} x 0 (S (r r ({TAIL} x)))
{LEN2} ≡ λl.(λlf.flf) l (λrx.{TNIL} x 0 (S (r r ({TAIL} x))))
-}

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
-}

{- Aufgabe 3 -}

{-
-- a)

-- Parameter z: Die Zahl
-- Parameter f: Parameter für die ganze Zahl
λzf . f (z (λab . b)) (z (λab . a))

λzf . f (z F) (z T) -- <- In Kurzschreibweise


-- b)
-- x und y sind Zahlen, f ist der Parameter für die neue Zahl.
(λxyf . f ((xT) S (yF)) ((xF) S (yT)))

-- c)

-- Zieht die beiden Teile des Zahlenpaares voneinander ab.
-- Es funktioniert, da die Predecessor (P) -Funktion bei 0 abbricht.
U ≡ (λxz . z ((xF)P(xT)) ((xT)P(xF)))



-}

{- Aufgabe 4 -}

{-

(U ist definiert in 3c)
  
H ≡ λxy.∧ (Z(xPy)) (¬(Z(yPx)))
{>} ≡ λxy . H((Ux)F) ((Uy)F))
-}

{-
Pseudocode:
  (lambda ((a,b) (c,d)) (not (= (+ a d) (+ b c))))
  (lambda ((a,b) (c,d)) (¬ (= (aSd) (bSc))))
  (lambda (x y) (¬ (= ((xT)S(yF)) ((xF)S(yT)))))

{/=} ≡ (λxy.¬ (E ((xT)S(yF)) ((xF)S(yT))))
-}

{- Aufgabe 5 -}

{-
-- Zahlen wie in Aufgabe vorgeschrieben:
-- Hilfsfunktion. 0 wenn x=y, -1 wenn x>y oder 1 wenn x<y
{CMP} ≡ λxy.({>=}xy) ((Exy) (λz.z00) (λz.z10)) (λz.z01)
--                    ^x=y           ^x>y      ^x<y

-- Pseudocode:
  (lambda (x y) (cmp (length x) (length y)))

-- Lösung
{LSTCMP} ≡ λxy.{CMP} ({LEN2}x) ({LEN2}y)

-- Ausformuliert:
{LSTCMP} ≡ λxy.{CMP} ((λl.(λlf.ffl) l (λrx.{TNIL} x 0 (S (r r ({TAIL} x))))) x) ((λl.(λlf.ffl) l (λrx.{TNIL} x 0 (S (r ({TAIL} x))))) y)
-}

-- (λrx.{TNIL} x 0 (S (r r ({TAIL} x))))
-- Rekursive Funktion, gibt 0 wenn NIL gefunden wurde oder addiert 1 auf das Ergebnis eines rekursiven Aufrufs mit dem Rest von x.



{- Aufgabe 6 -}

{-
a)
-- In Pseudocode:
  (lambda (e l) (if (empty? l) #f (if (= (head l) e) #t (recur e (tail l)))))

-- Code:
(λel. (λfel.ffel) (λrel.({TNIL} l) F ((E ({HEAD} l) e) T (r r e ({TAIL} l)))))
-}

{-
b)
-- In Pseudocode:
(lambda (e l)
  (if (empty? l)
    NIL
    (if (= (head l) e)
      (tail l)
      (pair (head l) (recur e (tail l))))))

-- Code:
(λel.(λfel.ffel)
(λrel.({TNIL} l) {NIL} ((E ({HEAD} l) e) ({TAIL} l) ({PAIR} ({HEAD} l) (r r e ({TAIL} l)))))
e l)
-}



{-
Tests:

{- Aufgabe 2 -}

----- Test mit 2 -----
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

----------

{- Aufgabe 3 -}

----- a) -----

-- Beispiel mit 1 (λz.z 0 1):
(λzf . f (z (λab . b)) (z (λab . a))) (λz.z 0 1)
(λf . f ((λz.z 0 1) (λab . b)) ((λz.z 0 1) (λab . a)))
(λf . f (1) (0))
(λf . f 1 0) -- <- Umkehrung von (λf . f 0 1), also Umkehrung von 1, also (-1)

----- b) -----

-- Beispiel mit 2 und 1 (Jeweils in reduzierter Form)
(λxyf . f ((xT) S (yF)) ((xF) S (yT))) (λa.a02) (λb.b01)
(λyf . f (((λa.a02)T) S (yF)) (((λa.a02)F) S (yT))) (λb.b01)
(λf . f (((λa.a02)T) S ((λb.b01)F)) (((λa.a02)F) S ((λb.b01)T)))
(λf . f ((T02) S (F01)) ((F02) S (T01)))
(λf . f (0 S 1) (2 S 0))
(λf . f 1 2) -- <- Valide Darstellung für die 1 als Integer im Lambda-Kalkül

----- c) -----

-- Test mit (λz.z23), also 1
(λn . (λg . gng) (λng.(∨ (Z (nT)) (Z (nF))) n (g (λz.z(P(nT))(P(nF))) g))) (λz.z23)
(λg . g(λz.z23)g) (λng.(∨ (Z (nT)) (Z (nF))) n (g (λz.z(P(nT))(P(nF))) g))

-- Die Funktion wird ab hier für Rekursive Aufrufe "g" genannt
((∨ (Z ((λz.z23)T)) (Z ((λz.z23)F))) (λz.z23) (g (λz.z(P((λz.z23)T))(P((λz.z23)F))) g))

-- T und F in Zahl einsetzen und Aufteilen:
((∨ (Z (T23)) (Z (F23))) (λz.z23) (g (λz.z(P(T23))(P(F23))) g))
((∨ (Z 2) (Z 3)) (λz.z23) (g (λz.z(P 2)(P 3)) g))

-- Z2 und Z3 ausführen, ∨ anwenden, P2 und P3 ausführen:
((∨ F F) (λz.z23) (g (λz.z(P 2)(P 3)) g))
(F (λz.z23) (g (λz.z(P 2)(P 3)) g))
(g (λz.z(P 2)(P 3)) g)
(g (λz.z12) g)

-- Rekursiver Aufruf:
((λng.(∨ (Z (nT)) (Z (nF))) n (g (λz.z(P(nT))(P(nF))) g)) (λz.z12) g)
((∨ (Z ((λz.z12)T)) (Z ((λz.z12)F))) (λz.z12) (g (λz.z(P((λz.z12)T))(P((λz.z12)F))) g))
((∨ (Z (T12)) (Z (F12))) (λz.z12) (g (λz.z(P(T12))(P(F12))) g))
((∨ (Z 1) (Z 2)) (λz.z12) (g (λz.z(P 1)(P 2)) g))
((∨ F F) (λz.z12) (g (λz.z(P 1)(P 2)) g))
(F (λz.z12) (g (λz.z(P 1)(P 2)) g))
(g (λz.z(P 1)(P 2)) g)
(g (λz.z01) g)

-- Nächster rekursiver Aufruf:
((λng.(∨ (Z (nT)) (Z (nF))) n (g (λz.z(P(nT))(P(nF))) g)) (λz.z01) g)
((∨ (Z ((λz.z01)T)) (Z ((λz.z01)F))) (λz.z01) (g (λz.z(P((λz.z01)T))(P((λz.z01)F))) g))
((∨ (Z (T01)) (Z (F01))) (λz.z01) (g (λz.z(P(T01))(P(F01))) g))
((∨ (Z 0) (Z 1)) (λz.z01) (g (λz.z(P(T01))(P(F01))) g))
((∨ T F) (λz.z01) (g (λz.z(P(T01))(P(F01))) g))
(T (λz.z01) (g (λz.z(P(T01))(P(F01))) g))
(λz.z01) -- <- Ergebnis.

----------

{- Aufgabe 4 -}

----- Test für < mit 1 und 2: -----

(λxy.> ((xT)S(yF)) ((xF)S(yT))) 1 2
(λxy.> ((xT)S(yF)) ((xF)S(yT))) (λz.z12) (λz.z02) -- 1 hier absichtlich nicht reduziert.
(> (((λz.z12)T)S((λz.z02)F)) (((λz.z12)F)S((λz.z02)T)))
(> ((T12)S(F02)) ((F12)S(T02)))
(> (1S2) (2S0))
(> 3 2)
F

----- Test für < mit 2 und 1: -----

(λxy.> ((xT)S(yF)) ((xF)S(yT))) 2 1
(λxy.> ((xT)S(yF)) ((xF)S(yT))) (λz.z02) (λz.z12) -- 1 hier absichtlich nicht reduziert.
(> (((λz.z02)T)S((λz.z12)F)) (((λz.z02)F)S((λz.z12)T)))
(> ((T02)S(F12)) ((F02)S(T12)))
(> (0S2) (2S1))
(> 2 3)
T

----- Test für /= mit 1 und 2: -----

(λxy.¬ (E ((xT)S(yF)) ((xF)S(yT)))) 1 2
(λxy.¬ (E ((xT)S(yF)) ((xF)S(yT)))) (λz.z12) (λz.z02) -- 1 hier absichtlich nicht reduziert.
(¬ (E (((λz.z12)T)S((λz.z02)F)) (((λz.z12)F)S((λz.z02)T))))
(¬ (E ((T12)S(F02)) ((F12)S(T02))))
(¬ (E (1S2) (2S0)))
(¬ (E 3 2))
(¬ F)
T

----- Test für /= mit 1 und 1: -----

(λxy.¬ (E ((xT)S(yF)) ((xF)S(yT)))) 1 1
(λxy.¬ (E ((xT)S(yF)) ((xF)S(yT)))) (λz.z12) (λz.z01) -- 1 hier absichtlich nicht reduziert.
(¬ (E (((λz.z12)T)S((λz.z01)F)) (((λz.z12)F)S((λz.z01)T))))
(¬ (E ((T12)S(F01)) ((F12)S(T01))))
(¬ (E (1S1) (2S0)))
(¬ (E 2 2))
(¬ T)

----- Test für /= mit -1 und -1: -----
(λxy.¬ (E ((xT)S(yF)) ((xF)S(yT)))) (-1) (-1)
(λxy.¬ (E ((xT)S(yF)) ((xF)S(yT)))) (λz.z21) (λz.z10) -- -1 hier absichtlich nicht reduziert.
(¬ (E (((λz.z21)T)S((λz.z10)F)) (((λz.z21)F)S((λz.z10)T))))
(¬ (E ((T21)S(F10)) ((F21)S(T10))))
(¬ (E (2S0) (1S1)))
(¬ (E 2 2))
(¬ T)
F

----------

{- Aufgabe 5 -}

----- Test mit [1] und []: -----
{LSTCMP} (λz.z1{NIL}) {NIL}

(λxy.{CMP} ({LEN2}x) ({LEN2}y)) (λz.z1{NIL}) {NIL}

(λxy.{CMP} ((λlf.ffl) x (λrx.{TNIL} x 0 (S (r r ({TAIL} x))))) ({LEN2}y)) (λz.z1{NIL}) {NIL}

(λy.{CMP} ((λlf.ffl) (λz.z1{NIL}) (λrx.{TNIL} x 0 (S (r r ({TAIL} x))))) ({LEN2}y)) {NIL}

(λy.{CMP} ({TNIL} (λz.z1{NIL}) 0 (S (r r ({TAIL} (λz.z1{NIL}))))) ({LEN2}y)) {NIL}

(λy.{CMP} (F 0 (S (r r ({TAIL} (λz.z1{NIL}))))) ({LEN2}y)) {NIL}

(λy.{CMP} (F 0 (S (r r {NIL}))) ({LEN2}y)) {NIL}

(λy.{CMP} (F 0 (S ((λrx.{TNIL} x 0 (S (r r ({TAIL} x)))) r {NIL}))) ({LEN2}y)) {NIL}

(λy.{CMP} (F 0 (S ({TNIL} {NIL} 0 (S (r r ({TAIL} {NIL})))))) ({LEN2}y)) {NIL}

(λy.{CMP} (F 0 (S (T 0 (S (r r ({TAIL} {NIL})))))) ({LEN2}y)) {NIL}

(λy.{CMP} (F 0 (S 0)) ({LEN2}y)) {NIL}

(λy.{CMP} (F 0 1) ({LEN2}y)) {NIL}

(λy.{CMP} 1 ({LEN2}y)) {NIL}

({CMP} 1 ({LEN2} {NIL}))

({CMP} 1 0) -- <- Kürze ({LEN2} {NIL}) ab

((λxy.({>=}xy) ((Exy) (λz.z00) (λz.z01)) (λz.z10)) 1 0)

(({>=}10) ((E10) (λz.z00) (λz.z01)) (λz.z10))

(T ((E10) (λz.z00) (λz.z01)) (λz.z10))

((E10) (λz.z00) (λz.z01))

(F (λz.z00) (λz.z01))

(λz.z01) -- <- Ergebnis; Equivalent zu 1, da [1] länger als [] ist.

----------

{- Aufgabe 6 -}

----- a) -----

-- Test mit 1 und [0,1]

(λel. (λfel.ffel) (λrel.({TNIL} l) F ((E ({HEAD} l) e) T (r r e ({TAIL} l)))) e l) 1 (λf.f0(λg.g1{NIL}))

((λfel.ffel) (λrel.({TNIL} l) F ((E ({HEAD} l) e) T (r r e ({TAIL} l)))) 1 (λf.fx(λg.g1{NIL})))

((λrel.({TNIL} l) F ((E ({HEAD} l) e) T (r r e ({TAIL} l)))) {...} 1 (λf.f0(λg.g1{NIL})))

(({TNIL} (λf.f0(λg.g1{NIL}))) F ((E ({HEAD} (λf.f0(λg.g1{NIL}))) 1) T (r r 1 ({TAIL} (λf.f0(λg.g1{NIL}))))))
(F F ((E ({HEAD} (λf.f0(λg.g1{NIL}))) 1) T (r r 1 ({TAIL} (λf.f0(λg.g1{NIL}))))))
((E ({HEAD} (λf.f0(λg.g1{NIL}))) 1) T (r r 1 ({TAIL} (λf.f0(λg.g1{NIL})))))
((E 0 1) T (r r 1 ({TAIL} (λf.f0(λg.g1{NIL})))))
(F T (r r 1 ({TAIL} (λf.f0(λg.g1{NIL})))))
(r r 1 (λg.g1{NIL}))

-- Rekursiver Aufruf:
((λrel.({TNIL} l) F ((E ({HEAD} l) e) T (r e ({TAIL} l)))) r 1 (λg.g1{NIL}))

(({TNIL} (λg.g1{NIL})) F ((E ({HEAD} (λg.g1{NIL})) 1) T (r 1 ({TAIL} (λg.g1{NIL})))))
(F F ((E ({HEAD} (λg.g1{NIL})) 1) T (r 1 ({TAIL} (λg.g1{NIL})))))
((E ({HEAD} (λg.g1{NIL})) 1) T (r 1 ({TAIL} (λg.g1{NIL}))))
((E 1 1) T (r 1 ({TAIL} (λg.g1{NIL}))))
(T T (r 1 ({TAIL} (λg.g1{NIL}))))
T

----- b) -----

-- Test mit 1 und [0,1,2]
(λel. (λfel.ffel) (λrel.({TNIL} l) {NIL} ((E ({HEAD} l) e) ({TAIL} l) ({PAIR} ({HEAD} l) (r r e ({TAIL} l))))) e l) 1 (λf.f0({PAIR}1({PAIR}2{NIL})))

(λel. (λfel.ffel) (λrel.({TNIL} l) {NIL} ((E ({HEAD} l) e) ({TAIL} l) ({PAIR} ({HEAD} l) (r r e ({TAIL} l))))) e l) 1 (λf.f0({PAIR}1({PAIR}2{NIL})))

(λl. (λfel.ffel) (λrel.({TNIL} l) {NIL} ((E ({HEAD} l) e) ({TAIL} l) ({PAIR} ({HEAD} l) (r r e ({TAIL} l))))) 1 l) (λf.f0({PAIR}1({PAIR}2{NIL})))

((λfel.ffel) (λrel.({TNIL} l) {NIL} ((E ({HEAD} l) e) ({TAIL} l) ({PAIR} ({HEAD} l) (r r e ({TAIL} l))))) 1 (λf.f0({PAIR}1({PAIR}2{NIL}))))

-- Die Funktion wird ab hier auch "r" genannt.
((λrel.({TNIL} l) {NIL} ((E ({HEAD} l) e) ({TAIL} l) ({PAIR} ({HEAD} l) (r r e ({TAIL} l))))) r 1 (λf.f0({PAIR}1({PAIR}2{NIL}))))

((λl.({TNIL} l) {NIL} ((E ({HEAD} l) 1) ({TAIL} l) ({PAIR} ({HEAD} l) (r r 1 ({TAIL} l))))) (λf.f0({PAIR}1({PAIR}2{NIL}))))

(({TNIL} (λf.f0({PAIR}1({PAIR}2{NIL})))) {NIL} ((E ({HEAD} (λf.f0({PAIR}1({PAIR}2{NIL})))) 1) ({TAIL} (λf.f0({PAIR}1({PAIR}2{NIL})))) ({PAIR} ({HEAD} (λf.f0({PAIR}1({PAIR}2{NIL})))) (r r 1 ({TAIL} (λf.f0({PAIR}1({PAIR}2{NIL}))))))))

(F {NIL} ((E 0 1) ({TAIL} (λf.f0({PAIR}1({PAIR}2{NIL})))) ({PAIR} ({HEAD} (λf.f0({PAIR}1({PAIR}2{NIL})))) (r r 1 ({TAIL} (λf.f0({PAIR}1({PAIR}2{NIL}))))))))

((E 0 1) ({TAIL} (λf.f0({PAIR}1({PAIR}2{NIL})))) ({PAIR} ({HEAD} (λf.f0({PAIR}1({PAIR}2{NIL})))) (r r 1 ({TAIL} (λf.f0({PAIR}1({PAIR}2{NIL})))))))

(F ({TAIL} (λf.f0({PAIR}1({PAIR}2{NIL})))) ({PAIR} ({HEAD} (λf.f0({PAIR}1({PAIR}2{NIL})))) (r r 1 ({TAIL} (λf.f0({PAIR}1({PAIR}2{NIL})))))))

({PAIR} ({HEAD} (λf.f0({PAIR}1({PAIR}2{NIL})))) (r r 1 ({TAIL} (λf.f0({PAIR}1({PAIR}2{NIL}))))))

({PAIR} 0 (r r 1 ({TAIL} (λf.f0({PAIR}1({PAIR}2{NIL}))))))

({PAIR} 0 (r r 1 (λf.f1({PAIR}2{NIL}))))

-- Rekursiver Aufruf:
({PAIR} 0 ((λrel.({TNIL} l) {NIL} ((E ({HEAD} l) e) ({TAIL} l) ({PAIR} ({HEAD} l) (r r e ({TAIL} l))))) r 1 (λf.f1({PAIR}2{NIL}))))

({PAIR} 0 ((λl.({TNIL} l) {NIL} ((E ({HEAD} l) 1) ({TAIL} l) ({PAIR} ({HEAD} l) (r r 1 ({TAIL} l))))) (λf.f1({PAIR}2{NIL}))))

({PAIR} 0 (({TNIL} (λf.f1({PAIR}2{NIL}))) {NIL} ((E ({HEAD} (λf.f1({PAIR}2{NIL}))) 1) ({TAIL} (λf.f1({PAIR}2{NIL}))) ({PAIR} ({HEAD} (λf.f1({PAIR}2{NIL}))) (r r 1 ({TAIL} (λf.f1({PAIR}2{NIL}))))))))

({PAIR} 0 (F {NIL} ((E ({HEAD} (λf.f1({PAIR}2{NIL}))) 1) ({TAIL} (λf.f1({PAIR}2{NIL}))) ({PAIR} ({HEAD} (λf.f1({PAIR}2{NIL}))) (r r 1 ({TAIL} (λf.f1({PAIR}2{NIL}))))))))

({PAIR} 0 (F {NIL} ((E 1 1) ({TAIL} (λf.f1({PAIR}2{NIL}))) ({PAIR} ({HEAD} (λf.f1({PAIR}2{NIL}))) (r r 1 ({TAIL} (λf.f1({PAIR}2{NIL}))))))))

({PAIR} 0 ((E 1 1) ({TAIL} (λf.f1({PAIR}2{NIL}))) ({PAIR} ({HEAD} (λf.f1({PAIR}2{NIL}))) (r r 1 ({TAIL} (λf.f1({PAIR}2{NIL})))))))

-- 1==1 -> Beende hier
({PAIR} 0 (T ({TAIL} (λf.f1({PAIR}2{NIL}))) ({PAIR} ({HEAD} (λf.f1({PAIR}2{NIL}))) (r r 1 ({TAIL} (λf.f1({PAIR}2{NIL})))))))

-- Entferne 2. Hälfte des Ausdrucks
({PAIR} 0 ({TAIL} (λf.f1({PAIR}2{NIL}))))

-- Liste Verbinden
({PAIR} 0 ({PAIR}2{NIL}))

(λf.f 0 (λg.g2{NIL})) -- <- Equivalent zu [0,2]
-}

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

