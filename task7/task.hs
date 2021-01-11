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

-- Enqueues a new element.
-- The element is prepended to the second list.
-- If the first list in the queue is empty, the second list is reversed and put in front.
enqueue :: Queue e -> e -> Queue e
enqueue (Queue [] l1) e = Queue (reverse (e:l1)) []
enqueue (Queue l0 l1) e = Queue l0 (e : l1)

-- Remove the first element from a queue.
-- If the Queue was empty, no change is made
-- If the first list is now empty, the second list is reversed and put in front.
-- Otherwise the first element is removed and the new queue returned.
dequeue :: Queue e -> Queue e
dequeue (Queue l0 l1)
  | null l0 = Queue l0 l1 -- Queue empty
  | null (tail l0) = Queue (reverse l1) [] -- Only 1 element in head
  | otherwise = Queue (tail l0) l1

-- Checks if the queue is empty.
-- A queue is empty if the first list is empty. All accessing functions 
-- (enqueue and dequeue) make it so that the second list can never be
-- non-empty when the first is empty.
isEmpty :: Queue e -> Bool
isEmpty (Queue [] _) = True
isEmpty _             = False

-- Creates an empty queue.
makeQueue :: Queue e
makeQueue = Queue [] []

-- Returns a string with the following format: "Queue[... ...]"
-- representing a queue.
showQueue :: (Show e) => Queue e -> [Char]
showQueue (Queue l0 l1) = "Queue[" ++ (sub l0 "") ++ " " ++ (sub (reverse l1) "") ++ "]"
  where sub (x:xs) acc | null acc = sub xs (show x)
                       | null xs = acc ++ (' ' : show x)
                       | otherwise = sub xs (acc ++ (' ':show x))
        sub []     acc = acc

-- Calculate the length of a queue.
queueLength :: Queue a -> Int
queueLength (Queue l0 l1) = (length l0) + length l1

-- Checks whether or not all elements in both lists as well as their 
-- lengths are equal.
listEql :: Eq a => [a] -> [a] -> Bool
listEql [] []         = True
listEql (x:xs) (y:ys) = x == y && listEql xs ys
listEql _      _      = False

-- Compares two lists via. two comparators:
--   One for the individual elements (cp) and one for the lengths (lc).
-- - If both lists have at least one element, call cp and continue with the 
--   next elements.
-- - If one of the lists is empty, call lc with 0 and the length of the other list.
-- - If both lists are empty, call lc with 0 and 0
lstCmp ::  (Ord a) => (a -> a -> Bool) -> (Int -> Int -> Bool) -> [a] -> [a] -> Bool
lstCmp cp lc (x:xs) (y:ys) = (cp x y) && lstCmp cp lc xs ys -- Both non-empty
lstCmp _  lc []     (_:ys) = lc 0 (1 + length ys) -- xs is empty
lstCmp _  lc (_:xs) []     = lc (1 + length xs) 0 -- ys is empty
lstCmp _  lc _      _      = lc 0 0 -- Both empty

-- Use lstCmp to compare two queues.
qCmp :: (Ord a) => (a -> a -> Bool) -> (Int -> Int -> Bool) -> Queue a -> Queue a -> Bool
qCmp cp lc (Queue l0 l1) (Queue l2 l3) = (lstCmp cp lc l0 l2) && lstCmp cp lc l1 l3

-- The == operator on queues uses the listEql function above.
instance (Eq a) => Eq (Queue a) where
  Queue xs0 xs1 == Queue ys0 ys1 = (listEql xs0 ys0) && listEql xs1 ys1

-- The show function for queues uses the showQueue function.
instance (Show a) => Show (Queue a) where
    show q = showQueue q

-- The functions <, <=, > and >= all use the qCmp function with their respective operators:
-- < : Every element in the first queue is less than its counterpart and the first queue is shorter.
-- <= : Same as < but equal elements and equal lengths are fine too.
-- > : Every element in the first queue is greateer than its counterpart and the first queue is longer.
-- >= : Same as > but equal elements and equal lengths are fine too.
instance (Ord q) => Ord (Queue q) where
    q0 < q1  = qCmp (<) (<) q0 q1
    q0 <= q1 = qCmp (<=) (<=) q0 q1
    q0 > q1  = qCmp (>) (<) q0 q1
    q0 >= q1 = qCmp (>=) (<=) q0 q1

newQueue xs = Queue xs []















