module MakeTree ( makeTree, toTreeList )

where

import HTypes ( HTree ( Leaf, Node ), Bit ( L, R ), HCode, HTable )

makeTree :: [(Char, Int)] -> HTree
makeTree = makeCodes . toTreeList

toTreeList ::  [(Char, Int)] -> [HTree]
toTreeList = map (uncurry Leaf)

amalgamate :: [HTree] -> [HTree]
amalgamate (t1:t2:ts) = insertTree (join t1 t2) ts

makeCodes :: [HTree] -> HTree
makeCodes [t] = t
makeCodes ts = makeCodes (amalgamate ts)

join :: HTree -> HTree -> HTree
join t1 t2 = Node (freq1+freq2) t1 t2
                 where
                   freq1 = value t1
                   freq2 = value t2

value :: HTree -> Int
value (Leaf   _  n ) = n
value (Node n _ _) = n

insertTree :: HTree -> [HTree] -> [HTree]
insertTree t [] = [t]
insertTree t (t1:ts)  | value t < value t1 = t:t1:ts
                            | otherwise = t1 : insertTree t ts
                            
