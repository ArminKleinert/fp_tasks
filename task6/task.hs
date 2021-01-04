{- Abgabe von Anna Sophie Pipperr und Armin Kleinertt -}

{- Code von Fr. Prof. Esponda -}


{-- Algebraische Datentypen Beispiel: --}

module V14_examples where

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

eqN :: Nat -> Nat -> B
eqN a b = andB (notB (lt a b)) (notB (gt a b))

factorial :: Nat -> Nat
factorial Zero = S Zero
factorial (S b) = mult (S b) (factorial b)

data ZInt = Z Nat Nat deriving Show

zadd :: ZInt -> ZInt -> ZInt
zadd (Z a b) (Z c d) = Z (add a c) (add b d)

zsimplify :: ZInt -> ZInt
zsimplify (Z Zero b) = Z Zero b
zsimplify (Z a Zero) = Z a Zero
zsimplify (Z (S a) (S b)) = zsimplify (Z a b)

{- Aufgabe 1 -}

eqB :: B -> B -> B
eqB F F = T
eqB T T = T
eqB _ _ = F

xorB :: B -> B -> B
xorB F T = T
xorB T F = T
xorB _ _ = F

-- eqN MUSS REKURSIV SEIN!!!

{- Aufgabe 2 -}

{- Aufgabe 3 -}

{- Tests     -}





