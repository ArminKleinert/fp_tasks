{-
1. Yep

2.
7^500
  354013649449525931426279442990642053580432370765307807128294998551122640747634597271084333393795330500587164243140988540373888581863590044622404991728906599366400005917176740377601943975293629949119408598903469298568197261263089787497027712508751288114794103433426230872340717070631044534195535930764662142517697871788941015702182840766509295270854651459881610586893475184126853183587780497947092464128387019611820640300001
8^0 
  1
2**1023
  8.98846567431158e307 
  Die Parameter werden in Floats umgewandelt und das Ergebnis wird mit der e-Notation angezeigt, um es abzukürzen
2**1024 
  Infinity
  Die Zahl ist zu groß für den 64-bit Bereich des größten Floating-Point Formats in Haskell.
div 5 2 
  2
  Die Division von Integern rundet standardmäßig ab.
div 5 (-2)  
  -3
  Das Ergebnis der Division wird abgerundet. -2 muss in Klammern geschrieben werden, da die (-) Funktion eine höhere Präzedenz hat als mod
quot 5 2 
  2
abs (-7) 
  7
5 /= 5  
  False
  5 ist nicht ungleich 5
0.9 == 3*(0.3) 
  False
  Siehe IEEE Floating Point Standard
2^^8   
  256.0
  Wie 2^8 aber 2 wird vorher in einen Fractional (also einen nicht-Integer) umgewandelt. Daher ist auch das Ergebnis kein Integer.
'a' < ‘b' 
  True
  'a' ist ein Char mit ASCII-Wert 97. 'b' hat den Wert 98. Also ist 'a' kleiner als 'b'
'1' < 'a' 
  True
  '1' hat den ASCII-Wert 49. 49 ist kleiner als 97.
mod 5 (-2)     
  -1
  mod (Modulo) von 5 und -2 ist der Rest der Division 5/-2.
  mod soll die folgende Formel erfüllen: (div x y)*y + (mod x y) == x
rem 5 2 
  1
  Rest der Division 5/2
rem 5 (-2) 
  1
  Soll folgende Formel erfüllen: (quot x y)*y + (rem x y) == x
-3 `mod` 5  
  -3
  `mod` hat eine höhere Priorität als (-). Der Rest der Division 3/5 ist 3. Dieser wird dann negiert. Daher -3.
(-3) `mod` 5  
  2
  Das Vorzeichen des 2. Arguments wird übernommen.
sqrt (-1) 
  NaN
  Die Quadratwurzel von -1 ist `i`, eine komplexe Zahl. Sie kann nicht wirklich dargestellt werden.
exp 1
  2.718281828459045
  Steht für (e ** n) wobei n das Argument ist.

3.
(-) ((+) ((+) 1 2) 3) (-2) 
  8
  Kann auch geschrieben werden als ``((1 + 2) + 3) - (-2)`` oder ``1 + 2 + 3 - (-2)``.
(-4 `mod` 5) == (-4 `rem` 5)
  True
  (mod 4 5) und (rem 4 5) haben das gleiche Ergebnis. Also sind auch die Negationen der beiden Ergebnisse gleich.
(4 `mod` (-5)) == (4 `rem` (-5)) 
  False
  Kann aufgeteilt werden in ``(mod 4 (-5))`` (-1) und ``(rem 4 (-5))`` (4). -1 ist ungleich 4.
4 == (div 4 (-3))*(-3) + (mod 4 (-3)) 
  4 == (div 4 (-3))*(-3) + (mod 4 (-3))
  4 == (-2)*(-3) + (mod 4 (-3))
  4 == 6 + (mod 4 (-3))
  4 == 6 + -2
  4 == 4
  True
succ 4 * 8 == succ (4 * 8)
  ``succ 4 * 8`` => ``5 * 8`` => 40
  ``succ (4*8)`` => ``succ 32`` => 33
  ``40 == 33`` => False
(10**17)*((0.1)*3-(0.1)*2-(0.1))
  (10**17)*((0.1)*3-(0.1)*2-(0.1))
  (1.0e17)*((0.1)*3-(0.1)*2-(0.1)) 
  (1.0e17)*((0.3)-(0.1)*2-(0.1)) 
  (1.0e17)*((0.3)-(0.2)-(0.1)) 
  (1.0e17)*((0.1)-(0.1)) 
  (1.0e17)*0.0 
  Theoretisch sollte es 0 ergeben. Aber (0.1 * 3) ist nicht genau 0.3 (und 0.1*2 ist nicht 0.2), wie oben festgestellt. ``(0.1*3)-(0.1*2)-0.1`` ist also 2.7755575615628914e-17.
  Also nochmal:
  (10**17)*((0.1)*3-(0.1)*2-(0.1))
  (10**17) * 2.7755575615628914e-17
  1.0e17 * 2.7755575615628914e-17
  2.7755575615628914
log 0
  -Infinity
  Der Wert ist undefiniert. Der Logarithmus des kleinst möglichen Wertes größer 0 läuft aber auf -Infinity zu. 

4.
Warum ist (min -2 0) kein gültiger Haskell-Ausdruck in Prelude?
  (min -2 0) wird interpretiert als ((min)-2) 0).

Warum ist der Ausdruck (mod 1 0) fehlerhaft? 
  Die Ausführung beinhaltet eine Teilung durch 0.
Warum ist (0.1 == 0.3/3) oder 0.9 == 3*(0.3) gleich False? 
  Teilung und Multiplikation können im Floating Point Format nicht perfekt dargestellt werden.
Warum sind die Ausdrücke  quot 1.0 3  und  3^1.0  fehlerhaft?
  // TODO

5.
Testen Sie folgende Kommandos des GHCI-Compiler. 
:help  
  Zeigt einige Hilfestellungen zu GHCI.
:? 
  Equivalent zu :help.
:browse 
  Zeigt alle verfügbaren Funktionen und Datentypen an.
:info max   
  Zeigt die Signatur der Funktion sowie den Ort wo sie definiert wurde.
::show modules 
  Zeigt geladene Module (Dateien) an (per :load)
:!ls 
  Führt das System-Komando "ls" aus. Das geht auch mit anderen wie :!cat und auch Argumente entgegennehmeen: ``:!ping 8.8.8.8``
:type 0 
  0 :: Num p => p
  Zeigt den Namen und Typ der Variable an.
:type 'a' 
  'a' :: Char
:type '5' 
  '5' :: Char
:type "1"  
  "1" :: [Char]
  [Char] bedeutet Liste von Char.
:type 0.0    
  0.0 :: Fractional p => p
:type (+) 
  (+) :: Num a => a -> a -> a
:load <filename>   
  Lädt eine Datei in den Interpreter.
:reload 
  Lädt alle importierten Module neu.
:quit
  Schließt GHCI
-}

-- 6
body_mass_index :: Double -> Double -> Double
body_mass_index kg me = kg / (me ^ 2)

-- 7
heron :: Double -> Double -> Double -> Double
heron a b c = let s = (a + b + c) / 2
              in sqrt (s * (s - a) * (s - b) * (s - c))
--               where
--                 s = (a + b + c) / 2

-- 8
wct :: Double -> Double -> Double
wct t v = 13.12 + 0.6215 * t - 11.37 * (v ** 0.16) + 0.3965 * t * (v ** 0.16)


