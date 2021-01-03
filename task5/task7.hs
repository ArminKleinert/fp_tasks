{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A7

data RootNum = RootNum Int Int

instance Show RootNum where
          show (RootNum a b) = show a ++ "+" ++ show b ++ "sqrt(2)"

instance Num RootNum where
          (RootNum a1 b1) + (RootNum a2 b2) = (RootNum (a1+a2) (b1+b2))
          (RootNum a1 b1) - (RootNum a2 b2) = (RootNum (a1-a2) (b1-b2))
          (RootNum a1 b1) * (RootNum a2 b2) = (RootNum ((a1*a2)+(b1*b2*2)) (a1*b2+b1*a2))


getValue :: RootNum -> Float
getValue (RootNum a b) = (fromIntegral a)+(fromIntegral b)*(sqrt (fromIntegral 2))

-- Test 

test :: IO ()
test = putStrLn ("(3+2*(sqrt(2))) * (2+1*(sqrt(2))): " ++ (show ((3+2*(sqrt(2))) * (2+1*(sqrt(2))))) ++
                 "\n(RootNum 3 2) * (RootNum 2 1): " ++ (show ((RootNum 3 2) * (RootNum 2 1))) ++
                 "\ngetValue ((RootNum 3 2) * (RootNum 2 1))" ++ (show (getValue ((RootNum 3 2) * (RootNum 2 1)))))


                 

