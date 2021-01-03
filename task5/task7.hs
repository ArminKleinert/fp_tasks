{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A7

data RootNum = RootNum Int Int
  deriving (Show, Eq, Ord, Num)

instance Num a => Num (RootNum Int Int) where
   (RootNum a b) + (RootNum c d) = RootNum (a+c) (b+d)
   (RootNum a b) * (RootNum c d) = RootNum (a*c) (b*d)
   (RootNum a b) - (RootNum c d) = RootNum (a-c) (b-d)
   abs    (RootNum a b) = RootNum (abs a)    (abs b)
   signum (RootNum a b) = RootNum (signum a) (signum b)
   fromInteger i = RootNum (fromInteger i) (fromInteger i)


denominator :: RootNum -> Int
denominator (RootNum n d) = d

numerator :: RootNum -> Int
numerator (RootNum n d) = n

simplify :: RootNum -> RootNum
simplify (RootNum _p _q) = RootNum p2 q2
  where (p1,q1) = if (_q < 0) then (_p * (-1), _q * (-1)) else (_p, _q)
        denom = gcd _p _q
        p2 = div p1 denom
        q2 = div q1 denom

-- (+) :: RootNum -> RootNum -> RootNum
-- (+) (RootNum n1 d1) (RootNum n2 d2) = simplify (RootNum n3 d3)
--   where n3 = ((n1 * d2) + (d1 * n2))
--         d3 = d2










