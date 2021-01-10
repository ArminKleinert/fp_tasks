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

{-
Damit die dequeue Operation auf die Verwendung der (++) Funktion verzichten kann, modellieren Sie Ihre Warteschlange mit Hilfe von zwei Listen. Elemente werden immer aus der ersten Liste entfernt und neue Elemente werden am Anfang der zweiten Liste eingefügt. Wenn die erste Liste leer ist und ein weiteres Element entfernt werden soll, wird die zweite Liste umgedreht und als erste Liste gesetzt. 
-}

type Queue a = ([a], [a])

-- TODO
enqueue :: Queue -> e -> Queue
enqueue q0 e = q0

-- TODO
dequeue :: Queue -> Queue
dequeue q0 = q0

-- TODO
isEmpty :: Queue -> Bool
isEmpty q0 = True

-- TODO
makeQueue :: Queue
makeQueue = ([], [])

-- TODO
showQueue :: Queue -> [Char]
showQueue q0 = ""


-- TODO
instance (Show q) => Queue q where
    show q0 = showQueue q0 -- TODO

-- TODO
instance (Eq q) => Queue q where
    q0 == q1 = True -- TODO

-- TODO
instance (Ord q) => Queue q where
    q0 < q1 = True -- TODO
    q0 <= q1 = True -- TODO
    q0 > q1 = True -- TODO
    q0 >= q1 = True -- TODO

















