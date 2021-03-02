{-# LANGUAGE NPlusKPatterns #-}

-- HTypes

data Bit = L | R  deriving (Eq, Show)
type HCode = [Bit]
type HTable = [ (Char, HCode) ]
data HTree = Leaf Char Int | Node Int HTree HTree deriving (Show)

-- Coding

-- Encode a string using a table.
-- T(concat) + T(map) * T(clookup) * n
-- O(n^2)
code :: HTable -> [Char] -> HCode
code htable = concat . map (clookup htable)

-- O(n)
-- Get code for a char 'c'
clookup :: HTable -> Char -> HCode
clookup []  c   =  error "lookup..." -- 1
clookup ((char, hc):rtable) c | char == c =  hc -- 2
                              | otherwise = clookup rtable c -- n

-- O(n)
decode' :: HTree -> HCode -> [Char]
decode' htree = decodeByte htree
               where
                 decodeByte (Node n t1 t2) (L:rest) = decodeByte t1 rest
                 decodeByte (Node n t1 t2) (R:rest) = decodeByte t2 rest
                 decodeByte (Leaf c n) rest = c:decodeByte htree rest
                 decodeByte t [] = []

-- Frequency

-- O(n*log(n))
mergeSort :: ([a]->[a]->[a])-> [a] -> [a]
mergeSort merge xs | (length xs < 2) = xs
                   | otherwise = merge (mergeSort merge first) (mergeSort merge second)
                                   where
                                     first = take half xs
                                     second = drop half xs
                                     half = (length xs) `div`2

-- O(n)
alphaMerge :: [(Char, Int)] -> [(Char,Int)] -> [(Char,Int)]
alphaMerge xs [] = xs
alphaMerge [] ys = ys
alphaMerge ((p,n):xs) ((q,m):ys)
                              | (p==q)   = (p,n+m) : alphaMerge xs ys
                              | (p<q)    = (p,n) : alphaMerge xs ((q,m):ys)
                              | otherwise = (q,m) : alphaMerge ((p,n):xs) ys

-- O(n)
freqMerge :: [(Char, Int)] -> [(Char,Int)] -> [(Char,Int)]
freqMerge xs [] = xs
freqMerge [] ys = ys
freqMerge ((p,n):xs) ((q,m):ys)
           | (n<m || (n==m && p<q)) = (p,n) : freqMerge xs ((q,m):ys)
           | otherwise = (q,m) : freqMerge ((p,n):xs) ys

-- O(n*log(n)) + O(n*log(n)) + O(n)
-- O(n*log(n))
frequency :: [Char] -> [(Char, Int)]
frequency = mergeSort freqMerge . mergeSort alphaMerge . map start
                  where
                  start ch = (ch,1)

-- MakeTree

-- O(n^2)
makeTree :: [(Char, Int)] -> HTree
makeTree = makeCodes . toTreeList

-- O(n)
toTreeList ::  [(Char, Int)] -> [HTree]
toTreeList = map (uncurry Leaf)

-- O(n)
amalgamate :: [HTree] -> [HTree]
amalgamate (t1:t2:ts) = insertTree (join t1 t2) ts

-- O(n^2)
makeCodes :: [HTree] -> HTree
makeCodes [t] = t
makeCodes ts = makeCodes (amalgamate ts)

-- O(1)
join :: HTree -> HTree -> HTree
join t1 t2 = Node (freq1+freq2) t1 t2
                 where
                   freq1 = value t1
                   freq2 = value t2

-- O(1)
value :: HTree -> Int
value (Leaf   _  n ) = n
value (Node n _ _) = n

-- O(n)
insertTree :: HTree -> [HTree] -> [HTree]
insertTree t [] = [t]
insertTree t (t1:ts)  | value t < value t1 = t:t1:ts
                      | otherwise = t1 : insertTree t ts
                            
-- CodeTable

-- O(n)
codeTable :: HTree -> HTable
codeTable ht = convert [] ht
                  where
                  convert :: HCode -> HTree -> HTable
                  convert hc (Leaf c n) = [(c, hc)]
                  convert hc (Node n tl tr) = (convert (hc++[L]) tl) ++ 
                                                          (convert (hc++[R]) tr)

-- MakeCode

-- O(n^2)
codes :: [Char] -> HTree
codes = makeTree . frequency

--
-- Encode
--

-- O(n^2)
encode :: [Char] -> (HCode, HTable)
encode source = let table = codeTable (codes source)
                in ((code table source), table)

---
--- Decode
---

-- O(n)
decode :: HCode -> HTree -> [Char]
decode zipped tree = decode' tree zipped


























