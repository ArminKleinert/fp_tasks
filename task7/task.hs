{- Abgabe von Anna Sophie Pipperr und Armin Kleinert -}

data SimpleBT = L | N SimpleBT SimpleBT deriving Show

{- Aufgabe 1 -}

type Height = Integer

-- Helper
genSimpleBT :: Height -> SimpleBT
genSimpleBT   0  =  L
genSimpleBT (n+1) = N (genSimpleBT n) (genSimpleBT n)

-- Helper
height :: SimpleBT -> Integer
height L = 0
height (N lt rt) = (max (height lt) (height rt)) + 1

-- Helper
makeTree :: SimpleBT
makeTree = N L L

-- Helper v. Esponda
nodes :: SimpleBT -> Integer
nodes L = 0
nodes (N leftT rightT) = 1 + nodes leftT + nodes rightT

-- A tree is full if is has ((2^height)-1) nodes and 2^height leaves 
-- (which is implied by having 2^h-1 nodes because we have no NIL element)
isFull :: SimpleBT -> Bool
isFull tree = (2 ^ height tree) - 1 == nnodes 
  where nnodes = nodes tree

-- Insert exactly 2 leaves, starting at the smaller sub-tree.
-- - If the tree is just a leaf, create a Node with two leaves
-- - If the tree at least one sub-tree which is not a leaf,
--   check the sizes of the sub-trees. If the right one is smaller,
--   recursively call on the right tree. Otherwise, recursively
--   call with the left sub-tree.
insertLeaves' :: SimpleBT -> SimpleBT
insertLeaves' L       = N L L
insertLeaves' (N t0 t1)
  | (height t0) > (height t1) = N t0 (insertLeaves' t1)
  | otherwise = N (insertLeaves' t0) t1

-- Inserts n leaves into a tree.
-- - If n is 0, return the tree
-- - If n is odd, show an error
-- - Otherwise, call insertLeaves' n-1 times, each with 
--   the result of the previous call.
insertLeaves :: Integer -> SimpleBT -> SimpleBT
insertLeaves 0 tree = tree
insertLeaves n tree = insertLeaves (n-1) (insertLeaves' tree)

-- Remove exactly 2 leaves, starting at the bigger sub-tree.
-- - If the tree is a node with two leaves, turn it into a leaf
-- - If the tree is just a leaf, return it
-- - If the tree at least one sub-tree which is not a leaf,
--   check the sizes of the sub-trees. If the right one is smaller,
--   recursively call on the left tree. Otherwise, recursively
--   call with the right sub-tree.
removeLeaves' :: SimpleBT -> SimpleBT
removeLeaves' (N L L) = L
removeLeaves' L       = L
removeLeaves' (N t0 t1)
  | (height t0) > (height t1) = N (removeLeaves' t0) t1
  | otherwise = N t0 (removeLeaves' t1)


-- Removes n leaves from a tree.
-- - If n is 0, return the tree
-- - If n is odd, show an error
-- - Otherwise, call removeLeaves' (n/2) times, each with 
--   the result of the previous call.
removeLeaves :: Integer -> SimpleBT -> SimpleBT
removeLeaves 0 tree = tree
removeLeaves n tree = removeLeaves (n-1) (removeLeaves' tree)

-- Aus den Unterlagen
gen :: Int -> [a] -> [a]
gen n str = take n (foldr (++) [] (repeat str))

-- Aus den Unterlagen
joinLines :: [String] -> String
joinLines [] = ""
joinLines (s:ls) = s ++ "\n" ++ joinLines ls

-- Aus den Unterlagen
paintTree :: SimpleBT -> ([String], Int)
paintTree L = ([" L  "], 1)
paintTree (N lTree rTree) = ([nodeLine, nodeHLine, horLine] ++ subTrees, newNodePos)
            where
                (lNodePicture, leftNodePos)  = paintTree lTree
                (rNodePicture, rigthNodePos) = paintTree rTree

                ltNewPicture = moveTreePos lNodePicture rNodePicture
                rtNewPicture = moveTreePos rNodePicture lNodePicture

                {- write spaces in between if necessary -}
                moveTreePos :: [String] -> [String] -> [String]
                moveTreePos str1 str2 | length str1 >= length str2 = str1
                                      | otherwise = str1 ++ (take rowsToFill (repeat spaces))
                                           where
                                              spaces = gen (length (head str1))  " "
                                              rowsToFill = (length str2) - (length str1)

                leftWidth = length (head lNodePicture)
                rightWidth = length (head rNodePicture)
                width = leftWidth + rightWidth

                hLineLength = (leftWidth - leftNodePos) + rigthNodePos
                newNodePos = leftNodePos + (div hLineLength 2)

                horLine  = (gen leftNodePos " ") ++ "*" ++ gen (hLineLength - 1) "-" ++ "*"
                                                 ++ gen (width - hLineLength - leftNodePos - 1) " "
                nodeLine  = (gen newNodePos " ") ++ "N" ++ gen (width - newNodePos - 1) " "
                nodeHLine = (gen newNodePos " ") ++ "|" ++ gen (width - newNodePos - 1) " "



                subTrees = zipWith (++) ltNewPicture rtNewPicture

-- Aus den Unterlagen
printCharList list = putStr (foldr (++) [] (map (++"\n") list))

-- Aus den Unterlagen
printSimpleBT tree = printCharList (fst (paintTree tree))

testPrintSimpleBT :: IO [Char]
testPrintSimpleBT = do {
                        x <- printSimpleBT (genSimpleBT 1);
                        y <- printSimpleBT (genSimpleBT 2);
                        z <- printSimpleBT (genSimpleBT 3);
                        return ""
                        }
    

{- Aufgabe 2 -}

data BSearchTree a = Nil | Node a (BSearchTree a) (BSearchTree a) deriving Show

-- Helper
-- Creates a balanced SearchTree from a list (if the list is sorted)
listToBST :: (Ord a) => [a] -> BSearchTree a
listToBST []   = Nil
listToBST elts = Node (elts !! half)
                      (listToBST (take half elts))
                      (listToBST (drop (half+1) elts))
    where half = length elts `quot` 2

-- Helper (Vorlesungsfolien)
insert :: (Ord a) => a -> BSearchTree a -> BSearchTree a
insert k Nil = Node k Nil Nil
insert k (Node x ltree rtree) | k<x = Node x (insert k ltree) rtree
                              | otherwise = Node x ltree (insert k rtree)


-- postOrder iterate a tree
-- 
-- tree = Node 'd' (Node 'b' (Node 'a' Nil Nil) (Node 'c' Nil Nil)) (Node 'f' (Node 'e' Nil Nil) Nil)
--      d
--   b     f
-- a   c e   Nil
-- postOrder tree => "acbefd"
postOrder :: BSearchTree a -> [a]
postOrder Nil = []
postOrder (Node x ltree rtree) = postOrder ltree ++ postOrder rtree ++ [x]

-- Checks whether or not a tree has exactly one child-node. Nil is not a child.
-- Nil => False
-- Node 1 Nil Nil => False
-- Node 1 (Node ...) Nil => True
-- Node 1 Nil (Node ...) => True
-- Node 1 (Node ...) (Node ...) => Recurse left and right
oneChild :: (Ord a) => BSearchTree a -> Bool
oneChild (Node _ Nil Nil) = False
oneChild (Node _ Nil t1) = True
oneChild (Node _ t0 Nil) = True
oneChild Nil = False
oneChild (Node _ l r) = oneChild l || oneChild r

{-
height2 :: BSearchTree a -> Integer
height2 Nil = 0
height2 (Node _ lt rt) = (max (height2 lt) (height2 rt)) + 1

balanced2 :: BSearchTree a -> Bool
balanced2 (Node _ Nil Nil) = True
balanced2 (Node _ _ Nil) = False
balanced2 (Node _ Nil _) = False
balanced2 (Node _ t0 t1) = (balanced2 t0) && (balanced2 t1) && (height2 t0) == height2 t1
-}



height2 :: BSearchTree a -> Integer
height2 Nil = 0
height2 (Node _ lt rt) = (max (height2 lt) (height2 rt)) + 1
{-
-- Checks whether or not a tree is full (wtf am I supposed to do here?!)
isFull2 :: (Ord a) => BSearchTree a -> Bool
isFull2 Nil = True
isFull2 (Node _ Nil Nil) = True
isFull2 (Node _ Nil _) = False
isFull2 (Node _ _ Nil) = False
isFull2 (Node _ t0 t1) = (isFull2 t0) == (isFull2 t1)
  -}

-- Helper
nodes2 :: BSearchTree a -> Integer
nodes2 Nil = 0
nodes2 (Node _ leftT rightT) = 1 + nodes2 leftT + nodes2 rightT

-- Helper
nils2 :: BSearchTree a -> Integer
nils2 Nil = 1
nils2 (Node _ leftT rightT) = nils2 leftT + nils2 rightT

-- A tree is full if is has ((2^height)-1) nodes and 2^height leaves
isFull2 :: BSearchTree a -> Bool
isFull2 tree = (2 ^ height2 tree) - 1 == nnodes && (2 ^ height2 tree) == nnils
  where nnodes = nodes2 tree
        nnils = nils2 tree

-- Find successor of an element in a tree. If no successor is found, returns Nothing.
successor :: (Ord a) => a -> BSearchTree a -> Maybe a
successor e tree = let l = (filter (\x -> x>e) (inOrderLst tree))
                   in if (null l) then Nothing else Just (head l)

-- In-order traversal of tree
inOrderLst :: BSearchTree a -> [a]
inOrderLst Nil = []
inOrderLst (Node x ltree rtree) = inOrderLst ltree ++ x : inOrderLst rtree

-- Applies a given function to each element in a tree:
-- mapTree (+1) (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil))
--           =>  Node 3 (Node 2 Nil Nil) (Node 4 Nil Nil)
mapTree :: (a -> b) -> BSearchTree a -> BSearchTree b
mapTree _ Nil = Nil
mapTree f (Node x ltree rtree) = Node (f x) (mapTree f ltree) (mapTree f rtree)

-- Yay
foldTree :: b -> (a -> b -> b -> b) -> BSearchTree a -> b
foldTree r _ Nil  = r -- Rekursionsanker
foldTree r f (Node x lt rt) = f x (foldTree r f lt) (foldTree r f rt)

{- Aufgabe 3 -}

{-
Aus der Aufgabe:
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
listEql _      _      = False -- Different lengths

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
    q0 > q1  = qCmp (>) (>) q0 q1
    q0 >= q1 = qCmp (>=) (>=) q0 q1

--newQueue xs = Queue xs []

listToQueue :: [a] -> Queue a
listToQueue lst = foldl (\q x -> enqueue q x) makeQueue lst

{- Tests -}

-- Test Aufgabe 1

testIsFull :: String
testIsFull = "isFull (N L L) => " ++ (show (isFull (N L L))) ++
             "\nisFull L       => " ++ (show (isFull L)) ++
             "\nisFull (N L (N L L) => " ++ show (isFull (N L (N L L)))

-- Tests Aufgabe 2

testPostOrder :: String
testPostOrder = "postOrder (Node 5 (Node 3 Nil Nil) (Node 8 Nil (Node 4 Nil Nil))) => " ++
                show (postOrder (Node 5 (Node 3 Nil Nil) (Node 8 Nil (Node 4 Nil Nil))))

testOneChild :: String
testOneChild = "oneChild (Node 5 Nil Nil) => " ++
               (show (oneChild (Node 5 Nil Nil))) ++ 
               "\noneChild (Node 5 (Node 1 Nil) Nil) => " ++
               (show (oneChild (Node 5 Nil Nil))) ++ 
               "\noneChild (Node 5 Nil (Node 1 Nil)) => " ++
               (show (oneChild (Node 5 Nil Nil))) ++ 
               "\noneChild (Node 5 (Node 1 Nil) (Node 6 Nil)) => " ++
               (show (oneChild (Node 5 Nil Nil)))

testSuccessor :: String
testSuccessor = "successor 3 (Node 5 (Node 3 Nil Nil) (Node 8 Nil (Node 4 Nil Nil))) => " ++
                show (successor 3 (Node 5 (Node 3 Nil Nil) (Node 8 Nil (Node 4 Nil Nil))))

testMapTree :: String
testMapTree = "mapTree (+1) (Node 5 (Node 3 Nil Nil) (Node 8 Nil (Node 4 Nil Nil)) => " ++
              show (mapTree (+1) (Node 5 (Node 3 Nil Nil) (Node 8 Nil (Node 4 Nil Nil))))

testFoldTree :: String
testFoldTree = "foldTree 0 addMult (Node 5 (Node 3 Nil Nil) (Node 8 Nil (Node 4 Nil Nil))) => " ++
               show (foldTree 0 addMult (Node 5 (Node 3 Nil Nil) (Node 8 Nil (Node 4 Nil Nil))))
  where addMult a b c = a + b*c

-- Tests Aufgabe 3

testMakeQueue :: String
testMakeQueue = "makeQueue => " ++ show q
  where q :: Queue Integer -- Must specify type because haskell cannot infer it otherwise
        q = makeQueue

testlistToQueue :: String
testlistToQueue = "listToQueue [1 .. 15] => " ++ show (listToQueue [1 .. 15])

testEnqueue :: String
testEnqueue = "enqueue (Queue [1 .. 15] [16, 17]) 16 => " ++ show (enqueue (Queue [1 .. 15] [16, 17]) 16)

testDequeue :: String
testDequeue = "dequeue (Queue [1 .. 15] [16, 17]) => " ++ show (dequeue (Queue [1 .. 15] [16, 17]))

testShowQueue :: String
testShowQueue = "showQueue (Queue [1 .. 15] [16, 17]) => " ++ showQueue (Queue [1 .. 15] [16, 17])

testEq :: String
testEq = "(==) (Queue [1, 2] [3, 4]) (Queue [1, 2] [3, 4]) => " ++
         show ((Queue [1, 2] [3, 4]) == (Queue [1, 2] [3, 4])) ++
         "\n(==) (Queue [5, 2] []) (Queue [1, 2] [3, 4])     => " ++
         show ((Queue [5, 2] []) == (Queue [1, 2] [3, 4])) ++
         "\n(==) (Queue [] []) (Queue [1, 2] [3, 4])         => " ++
         show ((Queue [] []) == (Queue [1, 2] [3, 4]))


testLt :: String
testLt = "(<) (Queue [1, 2] [3, 4]) (Queue [1, 2] [3, 4]) => " ++
         show ((Queue [1, 2] [3, 4]) < (Queue [1, 2] [3, 4])) ++
         "\n(<) (Queue [5, 2] []) (Queue [1, 2] [3, 4])     => " ++
         show ((Queue [5, 2] []) < (Queue [1, 2] [3, 4])) ++
         "\n(<) (Queue [] []) (Queue [1, 2] [3, 4])         => " ++
         show ((Queue [] []) < (Queue [1, 2] [3, 4]))


testLe :: String
testLe = "(<=) (Queue [1, 2] [3, 4]) (Queue [1, 2] [3, 4]) => " ++
         show ((Queue [1, 2] [3, 4]) <= (Queue [1, 2] [3, 4])) ++
         "\n(<=) (Queue [5, 2] []) (Queue [1, 2] [3, 4])     => " ++
         show ((Queue [5, 2] []) <= (Queue [1, 2] [3, 4])) ++
         "\n(<=) (Queue [] []) (Queue [1, 2] [3, 4])         => " ++
         show ((Queue [] []) <= (Queue [1, 2] [3, 4]))


testGt :: String
testGt = "(>) (Queue [1, 2] [3, 4]) (Queue [1, 2] [3, 4]) => " ++
         show ((Queue [1, 2] [3, 4]) > (Queue [1, 2] [3, 4])) ++
         "\n(>) (Queue [5, 2] []) (Queue [1, 2] [3, 4])     => " ++
         show ((Queue [5, 2] []) > (Queue [1, 2] [3, 4])) ++
         "\n(>) (Queue [] []) (Queue [1, 2] [3, 4])         => " ++
         show ((Queue [] []) > (Queue [1, 2] [3, 4]))

testGe :: String
testGe = "(>=) (Queue [1, 2] [3, 4]) (Queue [1, 2] [3, 4]) => " ++
         show ((Queue [1, 2] [3, 4]) >= (Queue [1, 2] [3, 4])) ++
         "\n(>=) (Queue [5, 2] []) (Queue [1, 2] [3, 4])     => " ++
         show ((Queue [5, 2] []) >= (Queue [1, 2] [3, 4])) ++
         "\n(>=) (Queue [] []) (Queue [1, 2] [3, 4])         => " ++
         show ((Queue [] []) >= (Queue [1, 2] [3, 4]))

testText :: String
testText = "-----------------\n--- Aufgabe 1 ---" ++
       "\n\n" ++ testIsFull ++
       --"\n\n" ++ testInsertLeaves ++
       --"\n\n" ++ testRemoveLeaves ++

       "\n\n-----------------\n--- Aufgabe 2 ---" ++
       "\n\n" ++ testPostOrder ++
       "\n\n" ++ testOneChild ++
       "\n\n" ++ testSuccessor ++
       "\n\n" ++ testMapTree ++
       "\n\n" ++ testFoldTree ++

       "\n\n-----------------\n--- Aufgabe 3 ---" ++
       "\n\n" ++ testMakeQueue ++
       "\n\n" ++ testlistToQueue++
       "\n\n" ++ testEnqueue ++
       "\n\n" ++ testDequeue ++
       "\n\n" ++ testShowQueue ++ 
       "\n\n" ++ testEq ++
       "\n\n" ++ testLt ++
       "\n\n" ++ testLe ++
       "\n\n" ++ testGt ++
       "\n\n" ++ testGe

test :: IO ()
test = putStrLn testText








