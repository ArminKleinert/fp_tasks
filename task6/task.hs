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

{- Helpers -}

ifB :: B -> B -> B -> B
ifB T b0 _ = b0
ifB F _ b1 = b1

-- Helper. Funktion nutzt Standard-Haskell, da sie nur zum schnelleren
-- Schreiben genutzt wird.
iToZ :: Int -> ZInt
iToZ i | i < 0 = Z Zero (int2Nat (abs i))
       | otherwise = Z (int2Nat i) Zero

-- Check if a number is <0
isNegZ :: ZInt -> B
isNegZ (Z n0 n1) = lt n0 n1

zZero :: ZInt
zZero = Z Zero Zero

zOne :: ZInt
zOne = Z (S Zero) Zero

ifZ :: B -> ZInt -> ZInt -> ZInt
ifZ T z0 _  = z0
ifZ F _  z1 = z1

{- Aufgabe 1 -}

eqB :: B -> B -> B
eqB F F = T
eqB T T = T
eqB _ _ = F

xorB :: B -> B -> B
xorB F T = T
xorB T F = T
xorB _ _ = F

(=>>) :: B -> B -> B
(=>>) F _ = T
(=>>) T b = b

eqN :: Nat -> Nat -> B
eqN Zero Zero = T
eqN Zero _    = F
eqN _    Zero = F
eqN (S n) (S m) = eqN n m

evenN :: Nat -> B
evenN Zero     = T
evenN (S Zero) = F
evenN (S (S n)) = evenN n


isDivisor :: Nat -> Nat -> B
isDivisor n m = ifB (eqN n m) case0 case1
  where case0 = T
        case1 = (ifB (lt n m) F case2)
        case2 = (isDivisor (subN n m) m)

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
        newN = iff (evenN n) n (nsucc n) -- Rounding up. To round down, use `iff (evenN n) n (predN n)`

-- Helper for ggtN
ggtN' :: Nat -> Nat -> Nat -> Nat
ggtN' n0 n1 Zero = (S Zero)
ggtN' n0 n1 acc = iff (andB (isDivisor n0 acc) (isDivisor n1 acc))
                      acc
                      (ggtN' n0 n1 (predN acc))

ggtN :: Nat -> Nat -> Nat
ggtN n m = ggtN' n m (minN n m)

{- Aufgabe 2 -}

maxSurfaces :: Nat -> Nat
maxSurfaces Zero  = (S Zero)
maxSurfaces (S n) = nsucc (add (maxSurfaces n) n)

{- Aufgabe 3 -}

subZ :: ZInt -> ZInt -> ZInt
subZ (Z n0 n1) (Z n2 n3) = Z (add n0 n3) (add n1 n2) 

eqZ :: ZInt -> ZInt -> B
eqZ (Z n0 n1) (Z n2 n3) = eqN (add n0 n3) (add n1 n2)

neqZ :: ZInt -> ZInt -> B
neqZ z0 z1 = notB (eqZ z0 z1)

-- Helper for (>>>)
gtZ' :: ZInt -> ZInt -> B
gtZ' (Z n0 Zero) (Z n2 Zero) = gt n0 n2
gtZ' (Z Zero n1) (Z Zero n3) = lt n1 n3

(>>>) :: ZInt -> ZInt -> B
(>>>) z0 z1 = gtZ' (zsimplify z0) (zsimplify z1)

negZ :: ZInt -> ZInt
negZ (Z n0 n1) = Z n1 n0

multZ :: ZInt -> ZInt -> ZInt
multZ (Z n0 n1) (Z n2 n3) = Z (add (mult n0 n2) (mult n1 n3))
                              (add (mult n0 n3) (mult n1 n2))

absZ' :: ZInt -> ZInt
absZ' (Z Zero n0) = Z n0 Zero
absZ' z0          = z0

absZ :: ZInt -> ZInt
absZ z0 = absZ' (zsimplify z0)

-- Helper for powZ
powZ' :: ZInt -> ZInt -> ZInt
powZ' z0 z1 = ifZ (eqZ z1 zZero) zOne (multZ z0 (powZ' z0 (subZ z1 zOne)))

-- Power function for integers.
-- Gives fixed value (Z (S Zero) Zero) (1) if second argument is ==0
-- Error if second argument is <0
powZ :: ZInt -> ZInt -> ZInt
powZ z0 z1 = ifZ (eqZ z1 zZero)
               zOne
               (ifZ (isNegZ z1)
                 (error "second argument must not be negative!")
                 (powZ' z0 z1))

-- TODO!!!
isDivisorZ :: ZInt -> ZInt -> B
isDivisorZ z0 z1 = T


{- Tests     -}





