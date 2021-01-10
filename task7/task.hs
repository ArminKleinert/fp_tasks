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

data Queue a = Queue [a] [a]

enqueue :: Queue e -> e -> Queue e
enqueue (Queue l0 l1) e = Queue l0 (e : l1)

dequeue :: Queue e -> Queue e
dequeue (Queue l0 l1)
  | null l0 = Queue l0 l1 -- Queue empty
  | null (tail l0) = Queue (reverse l1) [] -- Only 1 element in head
  | otherwise = Queue (tail l0) l1

isEmpty :: Queue e -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

makeQueue :: Queue e
makeQueue = Queue [] []

showQueue :: (Show e) => Queue e -> [Char]
showQueue (Queue l0 l1) = "Queue[" ++ (sub l0 "") ++ " " ++ (sub (reverse l1) "") ++ "]"
  where sub (x:xs) acc | null xs = show x
                       | otherwise = (show x) ++ (' ' : sub xs "")
        sub []     acc = acc

queueLength :: Queue a -> Int
queueLength (Queue l0 l1) = (length l0) + length l1

listEql :: Eq a => [a] -> [a] -> Bool
listEql [] []         = True
listEql (x:xs) (y:ys) = x == y && listEql xs ys
listEql _      _      = False

instance (Eq a) => Eq (Queue a) where
  Queue xs0 xs1 == Queue ys0 ys1 = (listEql xs0 ys0) && listEql xs1 ys1

instance (Show a) => Show (Queue a) where
    show q = showQueue q

-- TODO
instance (Ord q) => Ord (Queue q) where
    q0 < q1 = True -- ???
    q0 <= q1 = True -- ???
    q0 > q1 = True -- ???
    q0 >= q1 = True -- ???

















