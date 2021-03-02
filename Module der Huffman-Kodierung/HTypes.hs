module HTypes ( 
                 HTree (Leaf, Node), 
                 Bit (L, R), HCode,
                 HTable
                       )
where
  
data Bit = L | R  deriving (Eq, Show)
type HCode = [Bit]
type HTable = [ (Char, HCode) ]
data HTree = Leaf Char Int | Node Int HTree HTree deriving (Show)