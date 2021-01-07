{- Abgabe von Anna Sophie Pipperr und Armin Kleinert -}

{- Code von Fr. Prof. Esponda -}

data Nat = Zero | S Nat  deriving Show
data B = F | T deriving Show

add :: Nat -> Nat -> Nat
add a Zero  = a
add a (S b) = add (S a) b

nsucc :: Nat -> Nat
nsucc n = S n

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult a (S b) = add a (mult a b)

lt :: Nat -> Nat -> B
lt Zero (S _)  = T
lt (S a) (S b) = lt a b
lt _ _ = F

gt :: Nat -> Nat -> B
gt (S _) Zero  = T
gt (S a) (S b) = gt a b
gt _ _ = F

orB :: B -> B -> B
orB F F = F
orB _ _ = T

andB :: B -> B -> B
andB T T = T
andB _ _ = F

notB :: B -> B
notB T = F
notB F = T

eqN' :: Nat -> Nat -> B
eqN' a b = andB (notB (lt a b)) (notB (gt a b))

factorial :: Nat -> Nat
factorial Zero = S Zero
factorial (S b) = mult (S b) (factorial b)

predN :: Nat -> Nat
predN Zero = Zero
predN (S n) = n

subN :: Nat -> Nat -> Nat
subN m Zero = m
subN m (S n) = subN (predN m) n

minN :: Nat -> Nat -> Nat
minN a b = iff (lt a b) a b

iff :: B -> Nat -> Nat -> Nat
iff T f0 _ = f0
iff F _ f1 = f1

nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (S a) = 1 + (nat2Int a)

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat (n+1) = S (int2Nat n)

data ZInt = Z Nat Nat deriving Show

zadd :: ZInt -> ZInt -> ZInt
zadd (Z a b) (Z c d) = Z (add a c) (add b d)

zsimplify :: ZInt -> ZInt
zsimplify (Z Zero b) = Z Zero b
zsimplify (Z a Zero) = Z a Zero
zsimplify (Z (S a) (S b)) = zsimplify (Z a b)

{- ### Helpers ### -}

-- if with B predicate and output B
ifB :: B -> B -> B -> B
ifB T b0 _ = b0
ifB F _ b1 = b1

-- Helper. Funktion nutzt Standard-Haskell, da sie nur zum schnelleren
-- Schreiben genutzt wird.
-- Uses some standard-haskell. Hopefully, this is allowed as this
-- function is only ever used as a helper in the tests.
iToZ :: Int -> ZInt
iToZ i | i < 0     = Z Zero (int2Nat (abs i))
       | otherwise = Z (int2Nat i) Zero

-- Helper for conversion of ZInt to Int
-- -(b-a) if a < b
-- a-b    if a >= b
-- Uses Haskell's if function. Hopefully, this is allowed as this
-- function is only used as a helper for tests...
zToI :: ZInt -> Int
zToI (Z n0 n1) | (nat2Int n0) < (nat2Int n1) = (- (nat2Int (subN n1 n0)))
               | otherwise       = nat2Int (subN n0 n1)

-- Check if a number is <0
-- An integer is made of 2 natural numbers a and b.
-- It is negative if a<b
isNegZ :: ZInt -> B
isNegZ (Z n0 n1) = lt n0 n1

-- 0 as ZInt
zZero :: ZInt
zZero = Z Zero Zero

-- 1 as ZInt
zOne :: ZInt
zOne = Z (S Zero) Zero

ifZ :: B -> ZInt -> ZInt -> ZInt
ifZ T z0 _  = z0
ifZ F _  z1 = z1

{- Aufgabe 1 -}

-- Equality for type B
-- A | B | Result
-- F | F | T
-- F | T | F
-- T | F | F
-- T | T | T
eqB :: B -> B -> B
eqB F F = T
eqB T T = T
eqB _ _ = F

-- XOR
-- A | B | Result
-- F | F | F
-- F | T | T
-- T | F | T
-- T | T | F
xorB :: B -> B -> B
xorB F T = T
xorB T F = T
xorB _ _ = F

-- Implication
-- A | B | Result
-- F | F | T
-- F | T | T
-- T | F | F
-- T | T | T
(=>>) :: B -> B -> B
(=>>) F _ = T -- If a is F, b doesn't matter
(=>>) T b = b -- If a is T, b must be T also. Otherwise the result is F

-- Equality of natural numbers
-- Rules:
-- - Zero is equal to Zero
-- - Zero is not equal to any non-zero
-- - If non of the numbers are Zero, reduce both by 1 and retry.
eqN :: Nat -> Nat -> B
eqN Zero Zero = T -- 0==0
eqN Zero _    = F -- 0 /= m
eqN _    Zero = F -- n /= 0
eqN (S n) (S m) = eqN n m -- Recursive call with n-1 and m-1

-- Check whether or not a number is even:
-- - Zero is even
-- - One is not even
-- - If the number is >1, recurse with the n-2
evenN :: Nat -> B
evenN Zero     = T -- 0 is even
evenN (S Zero) = F -- 1 is not even
evenN (S (S n)) = evenN n -- Recursive call with n-2

-- Check whether or not a number 'n' is divisible by a number 'm'
-- isDivisorN 0 0 = F -- Usually undefined, default to F
-- isDivisorN _ 0 = F
-- n==m => T
-- n<m  => F
-- n>m  => Recursive call with (n-m) and m
isDivisorN :: Nat -> Nat -> B
isDivisorN _ Zero = F -- No number can be divided by 0 and 0/0 is undefined, so default to F
isDivisorN n m = ifB (eqN n m) T case1 -- n==m => T
  where case1 = (ifB (lt n m) F case2) -- n<m => F
        case2 = (isDivisorN (subN n m) m) -- n>m => Recursive call as above

-- Helper for halbN
halbN' :: Nat -> Nat -> Nat
halbN' n0 acc = iff (eqN (subN n0 acc) acc) acc (halbN' n0 (nsucc acc))

-- Halves a natural number.
-- If n is 0, return n
-- If n is 1, return n
-- If n is not even, add 1 and continue
-- Go to (halbN' n Zero)
halbN :: Nat -> Nat
halbN n = iff nIsOneOrLower n (halbN' newN Zero)
  where nIsOneOrLower = (lt n (S (S Zero)))
        newN = iff (evenN n) n (predN n) -- Rounding down. To round up, use `iff (evenN n) n (nsucc n)`

-- Great common divisor
-- ggtN n Zero = n (Same for Zero and n; Implies ggtN(Zero,Zero) = Zero)
-- ggtN n n = n
-- ggtN n m = ggtN n-m m if n>m
-- ggtN n m = ggtN n m-n if n<m
-- https://en.wikipedia.org/wiki/Euclidean_algorithm
-- https://mfleck.cs.illinois.edu/building-blocks/version-1.0/number-theory.pdf
ggtN :: Nat -> Nat -> Nat
ggtN n Zero = n
ggtN Zero n = n
ggtN n m = iff (eqN n m)
            n
            (iff (gt n m)
              (ggtN (subN n m) m)
              (ggtN n (subN m n)))

{- ### Aufgabe 2 ### -}

-- maxSurfaces for type Nat
-- maxSurfaces 0 = 1
-- maxSurfaces n-1 = (maxSurfaces n) + n + 1
maxSurfaces :: Nat -> Nat
maxSurfaces Zero  = (S Zero)
maxSurfaces (S n) = nsucc (add (maxSurfaces n) n) -- ms n-1 = 1 + maxSurfaces n + n

{- ### Aufgabe 3 ### -}

-- Subtraction of integers
-- https://en.wikipedia.org/wiki/Integer and MafI2 module
subZ :: ZInt -> ZInt -> ZInt
subZ (Z n0 n1) (Z n2 n3) = Z (add n0 n3) (add n1 n2) 

-- https://en.wikipedia.org/wiki/Integer and MafI2 module
eqZ :: ZInt -> ZInt -> B
eqZ (Z n0 n1) (Z n2 n3) = eqN (add n0 n3) (add n1 n2)

-- Check non-equality
-- https://en.wikipedia.org/wiki/Integer and MafI2 module
neqZ :: ZInt -> ZInt -> B
neqZ z0 z1 = notB (eqZ z0 z1)

-- Helper for (>>>)
-- Operands are expected to be simplified
gtZ' :: ZInt -> ZInt -> B
gtZ' (Z n0 Zero) (Z n2 Zero) = gt n0 n2 -- Both are positive
gtZ' (Z Zero n1) (Z Zero n3) = lt n1 n3 -- Both are negative
gtZ' (Z n0 Zero) (Z Zero n3) = T -- 1st is positiv, 2nd negative
gtZ' _           _           = F -- 1st is negative, 2nd positiv

-- Check z0>z1
-- Simplify numbers and forward to gtZ'
(>>>) :: ZInt -> ZInt -> B
(>>>) z0 z1 = gtZ' (zsimplify z0) (zsimplify z1)

-- Check z0<z1
ltZ :: ZInt -> ZInt -> B
ltZ z0 z1 = notB (z0 >>> z1)

-- Negate z0
negZ :: ZInt -> ZInt
negZ (Z n0 n1) = Z n1 n0

-- Multiplication for ZInt
-- https://en.wikipedia.org/wiki/Integer and MafI2 module
multZ :: ZInt -> ZInt -> ZInt
multZ (Z n0 n1) (Z n2 n3) = Z (add (mult n0 n2) (mult n1 n3))
                              (add (mult n0 n3) (mult n1 n2))

-- Helper for absZ
absZ' :: ZInt -> ZInt
absZ' (Z Zero n0) = Z n0 Zero
absZ' z0          = z0

-- get absolute value of an integer
-- Simplify and forward to absZ'
-- absZ (Z Zero (S Zero)) => Z (S Zero) Zero) (abs (-1))
-- absZ (Z (S Zero) Zero) => Z (S Zero) Zero) (abs 1)
-- absZ (Z Zero Zero)     => Z Zero Zero      (abs 0)
absZ :: ZInt -> ZInt
absZ z0 = absZ' (zsimplify z0)

-- Helper for powZ
powZ' :: ZInt -> ZInt -> ZInt
powZ' z0 z1 = ifZ (eqZ z1 zZero)
                zOne -- 1 if z1==0
                (multZ z0 (powZ' z0 (subZ z1 zOne))) -- n*(n**(m-1)) with recursion

-- Power function for integers.
-- Gives fixed value (Z (S Zero) Zero) (1) if second argument is ==0
-- Error if second argument is <0 because the result would be fractional...
-- powZ (iToZ
powZ :: ZInt -> ZInt -> ZInt
powZ z0 z1 = ifZ (eqZ z1 zZero)
               zOne
               (ifZ (isNegZ z1)
                 (error "second argument must not be negative!")
                 (powZ' z0 z1))

-- Check whether or not an integer is divisble by another integer.
-- It doesn't matter which numbers are positive and which are negative,
-- so after getting the absolute value, the isDivisorN function can be used
-- as for natural numbers.
--   isDivisorZ 6 3       => T (mod 6 3 => 0)
--   isDivisorZ (-6) 3    => T (mod (-6) 3 => 0)
--   isDivisorZ 6 (-3)    => T (mod 6 (-3) => 0)
--   isDivisorZ (-6) (-3) => T (mod (-6) (-3) => 0)
isDivisorZ :: ZInt -> ZInt -> B
isDivisorZ n m = isDivisorN (zToNat (zsimplify n)) (zToNat (zsimplify m))
  where zToNat (Z n0 Zero) = n0
        zToNat z0          = zToNat (absZ z0)


{- ### Tests ### -}


-- Task 1

testEqlB :: [Char]
testEqlB = "eqB F F                              = " ++ (show (eqB F F)) ++
           "\neqB F T                              = " ++ (show (eqB F T)) ++
           "\neqB T F                              = " ++ (show (eqB T F)) ++
           "\neqB T T                              = " ++ (show (eqB T T))

testXorB :: [Char]
testXorB = "xorB F F                             = " ++ (show (xorB F F)) ++
           "\nxorB F T                             = " ++ (show (xorB F T)) ++
           "\nxorB T F                             = " ++ (show (xorB T F)) ++
           "\nxorB T T                             = " ++ (show (xorB T T))

testImpB :: [Char]
testImpB = "F =>> F                              = " ++ (show ((=>>) F F)) ++
           "\nF =>> T                              = " ++ (show ((=>>) F T)) ++
           "\nT =>> F                              = " ++ (show ((=>>) T F)) ++
           "\nT =>> T                              = " ++ (show ((=>>) T T))

testEqlN :: [Char]
testEqlN = "eqN Zero Zero                        = " ++
           (show (eqN Zero Zero)) ++
           "\neqN (S Zero) Zero                    = " ++
           (show (eqN (S Zero) Zero)) ++
           "\neqN Zero (S Zero)                    = " ++
           (show (eqN Zero (S Zero))) ++
           "\neqN (int2Nat 55) (int2Nat 55)        = " ++
           (show (eqN (int2Nat 55) (int2Nat 55))) ++
           "\neqN (int2Nat 99) (int2Nat 99)        = " ++
           (show (eqN (int2Nat 99) (int2Nat 99))) ++
           "\neqN (int2Nat 99) (int2Nat 98)        = " ++
           (show (eqN (int2Nat 99) (int2Nat 98)))

testEvnN :: [Char]
testEvnN = "evenN Zero                           = " ++
            (show (evenN Zero)) ++
            "\nevenN (S Zero)                       = " ++
            (show (evenN (S Zero))) ++
            "\nevenN (int2Nat 55)                   = " ++
            (show (evenN (int2Nat 55))) ++
            "\nevenN (int2Nat 99)                   = " ++
            (show (evenN (int2Nat 99))) ++
            "\nevenN (int2Nat 98)                   = " ++
            (show (evenN (int2Nat 98)))

testIsDN :: [Char]
testIsDN = "isDivisorN Zero Zero                 = " ++
           (show (isDivisorN Zero Zero)) ++
           "\nisDivisorN (S Zero) Zero             = " ++
           (show (isDivisorN (S Zero) Zero)) ++
           "\nisDivisorN Zero (S Zero)             = " ++
           (show (isDivisorN Zero (S Zero))) ++
           "\nisDivisorN (int2Nat 55) (int2Nat 5)  = " ++
           (show (isDivisorN (int2Nat 55) (int2Nat 5))) ++
           "\nisDivisorN (int2Nat 99) (int2Nat 99) = " ++
           (show (isDivisorN (int2Nat 99) (int2Nat 99))) ++
           "\nisDivisorN (int2Nat 99) (int2Nat 5)  = " ++
           (show (isDivisorN (int2Nat 99) (int2Nat 5)))

testHlbN :: [Char]
testHlbN = "halbN Zero                           = " ++
            (show (nat2Int (halbN Zero))) ++
            "\nhalbN (S Zero)                       = " ++
            (show (nat2Int (halbN (S Zero)))) ++
            "\nhalbN (int2Nat 55)                   = " ++
            (show (nat2Int (halbN (int2Nat 55)))) ++
            "\nhalbN (int2Nat 99)                   = " ++
            (show (nat2Int (halbN (int2Nat 99)))) ++
            "\nhalbN (int2Nat 98)                   = " ++
            (show (nat2Int (halbN (int2Nat 98))))

testGgtN :: [Char]
testGgtN = "ggtN Zero Zero                        = " ++
           (show (ggtN Zero Zero)) ++
           "\nggtN (S Zero) Zero                    = " ++
           (show (ggtN (S Zero) Zero)) ++
           "\nggtN Zero (S Zero)                    = " ++
           (show (ggtN Zero (S Zero))) ++
           "\nggtN (int2Nat 55) (int2Nat 5)         = " ++
           (show (nat2Int (ggtN (int2Nat 55) (int2Nat 5)))) ++
           "\nggtN (int2Nat 99) (int2Nat 99)        = " ++
           (show (nat2Int (ggtN (int2Nat 99) (int2Nat 99)))) ++
           "\nggtN (int2Nat 99) (int2Nat 5)         = " ++
           (show (nat2Int (ggtN (int2Nat 99) (int2Nat 5))))

-- Task 2

testMxSN :: [Char]
testMxSN = "maxSurfaces Zero                      = " ++
           (show (maxSurfaces Zero)) ++
           "\nmaxSurfaces (S Zero)                  = " ++
           (show (maxSurfaces (S Zero))) ++
           "\nmaxSurfaces (int2Nat 55)              = " ++
           (show (nat2Int (maxSurfaces (int2Nat 55)))) ++
           "\nmaxSurfaces (int2Nat 99)              = " ++
           (show (nat2Int (maxSurfaces (int2Nat 99)))) ++
           "\nmaxSurfaces (int2Nat 98)              = " ++
           (show (nat2Int (maxSurfaces (int2Nat 98))))

-- Task 3

testIToZ :: [Char]
testIToZ = "iToZ 5                                = " ++
           (show (iToZ 5)) ++
           "\niToZ (-5)                             = " ++
           (show (iToZ (-5))) ++
           "\niToZ 0                                = " ++
           (show (iToZ 0))

testZToI :: [Char]
testZToI = "zToI (Z (S(S(S(S(S Zero))))) Zero)    = " ++
           (show (zToI (Z (S(S(S(S(S Zero))))) Zero))) ++
           "\nzToI (Z Zero (S(S(S(S(S Zero))))))    = " ++
           (show (zToI (Z Zero (S(S(S(S(S Zero)))))))) ++
           "\nzToI (Z Zero Zero)                    = " ++
           (show (zToI (Z Zero Zero)))

testNeqZ :: [Char]
testNeqZ = "neqZ 0 0                              = " ++
           (show (neqZ (iToZ 0) (iToZ 0))) ++
           "\nneqZ 5 0                              = " ++
           (show (neqZ (iToZ 5) (iToZ 0))) ++
           "\nneqZ 0 5                              = " ++
           (show (neqZ (iToZ 0) (iToZ 5))) ++
           "\nneqZ 5 5                              = " ++
           (show (neqZ (iToZ 5) (iToZ 5))) ++
           "\nneqZ (-5) 5                           = " ++
           (show (neqZ (iToZ (-5)) (iToZ 5))) ++
           "\nneqZ (-5) (-5)                        = " ++
           (show (neqZ (iToZ (-5)) (iToZ (-5)))) ++
           "\nneqZ 9999 9999                        = " ++
           (show (neqZ (iToZ 9999) (iToZ 9999))) ++
           "\nneqZ (-9999) (-9999)                  = " ++
           (show (neqZ (iToZ (-9999)) (iToZ (-9999))) ++
           "\nneqZ 9999 9998                        = " ++
           (show (neqZ (iToZ 9999) (iToZ 9998))))

testSymZ :: [Char]
testSymZ = "0 >>> 0                               = " ++
           (show ((>>>) (iToZ 0) (iToZ 0))) ++
           "\n5 >>> 0                               = " ++
           (show ((>>>) (iToZ 5) (iToZ 0))) ++
           "\n0 >>> 5                               = " ++
           (show ((>>>) (iToZ 0) (iToZ 5))) ++
           "\n5 >>> 5                               = " ++
           (show ((>>>) (iToZ 5) (iToZ 5))) ++
           "\n(-5) >>> 5                            = " ++
           (show ((>>>) (iToZ (-5)) (iToZ 5))) ++
           "\n(-5) >>> (-5)                         = " ++
           (show ((>>>) (iToZ (-5)) (iToZ (-5)))) ++
           "\n9999 >>> 9999                         = " ++
           (show ((>>>) (iToZ 9999) (iToZ 9999))) ++
           "\n(-9999) >>> (-9999)                   = " ++
           (show ((>>>) (iToZ (-9999)) (iToZ (-9999)))) ++
           "\n9999 >>> 9998                         = " ++
           (show ((>>>) (iToZ 9999) (iToZ 9998)))

testNegZ :: [Char]
testNegZ = "negZ 0                                = " ++
           (show (zToI (negZ (iToZ 0)))) ++
           "\nnegZ 5                                = " ++
           (show (zToI (negZ (iToZ 5)))) ++
           "\nnegZ (-5)                             = " ++
           (show (zToI (negZ (iToZ (-5))))) ++
           "\nnegZ 9999                             = " ++
           (show (zToI (negZ (iToZ 9999))))  ++
           "\nnegZ (-9999)                          = " ++
           (show (zToI (negZ (iToZ (-9999))))) ++
           "\nnegZ 9998                             = " ++
           (show (zToI (negZ (iToZ 9998))))

testMulZ :: [Char]
testMulZ = "multZ 0 0                             = " ++
           (show (zToI (multZ (iToZ 0) (iToZ 0)))) ++
           "\nmultZ 5 0                             = " ++
           (show (zToI (multZ (iToZ 5) (iToZ 0)))) ++
           "\nmultZ 0 5                             = " ++
           (show (zToI (multZ (iToZ 0) (iToZ 5)))) ++
           "\nmultZ 5 5                             = " ++
           (show (zToI (multZ (iToZ 5) (iToZ 5)))) ++
           "\nmultZ (-5) 5                          = " ++
           (show (zToI (multZ (iToZ (-5)) (iToZ 5)))) ++
           "\nmultZ (-5) (-5)                       = " ++
           (show (zToI (multZ (iToZ (-5)) (iToZ (-5))))) ++
           "\nmultZ 99 12                           = " ++
           (show (zToI (multZ (iToZ 99) (iToZ 12)))) ++
           "\nmultZ (-99) (-12)                     = " ++
           (show (zToI (multZ (iToZ (-99)) (iToZ (-12))))) ++
           "\nmultZ 99 11                           = " ++
           (show (zToI (multZ (iToZ 99) (iToZ 11))))

testAbsZ :: [Char]
testAbsZ = "absZ 0                                = " ++
           (show (zToI (absZ (iToZ 0)))) ++
           "\nabsZ 5                                = " ++
           (show (zToI (absZ (iToZ 5)))) ++
           "\nabsZ (-5)                             = " ++
           (show (zToI (absZ (iToZ (-5))))) ++
           "\nabsZ 9999                             = " ++
           (show (zToI (absZ (iToZ 9999))))  ++
           "\nabsZ (-9999)                          = " ++
           (show (zToI (absZ (iToZ (-9999))))) ++
           "\nabsZ (-9998)                          = " ++
           (show (zToI (absZ (iToZ (-9998))))) ++
           "\nabsZ 9998                             = " ++
           (show (zToI (absZ (iToZ 9998))))

testIsDZ :: [Char]
testIsDZ = "isDivisorZ 0 0                        = " ++
           (show (isDivisorZ (iToZ 0) (iToZ 0))) ++
           "\nisDivisorZ 5 0                        = " ++
           (show (isDivisorZ (iToZ 5) (iToZ 0))) ++
           "\nisDivisorZ 0 5                        = " ++
           (show (isDivisorZ (iToZ 0) (iToZ 5))) ++
           "\nisDivisorZ 5 5                        = " ++
           (show (isDivisorZ (iToZ 5) (iToZ 5))) ++
           "\nisDivisorZ (-5) 5                     = " ++
           (show (isDivisorZ (iToZ (-5)) (iToZ 5))) ++
           "\nisDivisorZ (-5) (-5)                  = " ++
           (show (isDivisorZ (iToZ (-5)) (iToZ (-5)))) ++
           "\nisDivisorZ 9999 9999                  = " ++
           (show (isDivisorZ (iToZ 9999) (iToZ 9999))) ++
           "\nisDivisorZ (-9999) (-9999)            = " ++
           (show (isDivisorZ (iToZ (-9999)) (iToZ (-9999)))) ++
           "\nisDivisorZ 9999 3                     = " ++
           (show (isDivisorZ (iToZ 9999) (iToZ 3))) ++
           "\nisDivisorZ (-9999) (-1)               = " ++
           (show (isDivisorZ (iToZ (-9999)) (iToZ (-1))))








test :: IO ()
test = putStrLn ("Ergebnisse und Inputs über 2 werden in den Tests via. " ++
                 "nat2Int und int2Nat abgekürzt!\n---\n--- Aufgabe 1\n---\n" ++
                 testEqlB ++ "\n---\n" ++ 
                 testXorB ++ "\n---\n" ++ 
                 testImpB ++ "\n---\n" ++ 
                 testEqlN ++ "\n---\n" ++ 
                 testEvnN ++ "\n---\n" ++ 
                 testIsDN ++ "\n---\n" ++
                 testHlbN ++ "\n---\n" ++
                 testGgtN ++ "\n" ++
                 "\n---\n--- Aufgabe 2\n---\n" ++
                 testMxSN ++ "\n\n---\n--- Aufgabe 3\n" ++
                 "(Fast) Alle Zahlen werden ab jetzt via. iToZ und zToI abgekürzt!\n---\n" ++
                 testIToZ ++ "\n---\n" ++
                 testZToI ++ "\n---\n" ++
                 testNeqZ ++ "\n---\n" ++
                 testSymZ ++ "\n---\n" ++
                 testNegZ ++ "\n---\n" ++
                 testMulZ ++ "\n---\n" ++
                 testAbsZ ++ "\n---\n" ++
                 testIsDZ)


























