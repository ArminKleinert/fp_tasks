{- Abgabe von Anna Sophie Pipperr und Armin Kleinert -}

data SimpleBT = L | N SimpleBT SimpleBT deriving Show

{- Aufgabe 1 -}

-- TODO
isFull :: SimpleBT -> Bool
isFull tree = False

-- TODO
removeLeaves :: Integer -> SimpleBT -> SimpleBT
removeLeaves n tree = tree

-- TODO
printSimpleBT :: SimpleBT -> String
printSimpleBT tree = show tree

{- Aufgabe 2 -}

data BSearchTree a = Nil | Node a (BSearchTree a) (BSearchTree a) deriving Show

-- TODO
postOrder :: (Ord a) => BSearchTree a -> [a]
postOrder tree = []

-- TODO
oneChild :: (Ord a) => BSearchTree a -> Bool
oneChild (Node _ Nil t1) = True
oneChild (Node _ t0 Nil) = True
oneChild (Node _ _ _) = False

-- TODO
isFull2 :: (Ord a) => BSearchTree a -> Bool
isFull2 tree = True

-- TODO
successor :: (Ord a) => a -> BSearchTree a -> Maybe a
successor e tree = Nothing

-- TODO
mapTree :: (a -> b) -> BSearchTree a -> BSearchTree b
mapTree f tree = Nil

-- TODO
foldTree :: b -> (a -> b -> b -> b) -> BSearchTree a -> b
foldTree r f tree = r

{- Aufgabe 3 -}

-- ._.

{- Aufgabe 4 -}

-- ._.

{- Aufgabe 5 -}

--type Queue = ([a], [a])

class Queue q where
  enqueue :: q -> e -> q
  dequeue :: q -> q
  isEmpty :: q -> Bool
  makeQueue :: q
  showQueue :: q -> [Char]
  




















