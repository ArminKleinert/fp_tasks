data Variable = U | V | W | X | Y | Z 
  deriving Eq, Ord, Show

data Exp = Var Variable | Add Exp Exp | Mult Exp Exp

instance Show Exp where
  show (Var a) = show a
  show (Add a b) = ((show a) ++ " + " ++ (show b))
  show (Mult a b) = ((show a) ++ " * " ++ (show b))
