{-# LANGUAGE NPlusKPatterns #-}

--
-- Begriffe
--

{- 
Funktion höherer Ordnung (Auch "Funktional" genannt)
Eine Funktion, die eine oder mehrere Funktionen als Argument entgegen nimmt oder eine Funktion zurückgibt.

Lineare Rekursion
Rekursive Funktionen, die in jedem Zweig ihrer Definition maximal einen rekursiven Aufruf beinhalten, werden als linear rekursiv bezeichnet.

Endrekursion (tail recursion)
Linear rekursive Funktionen werden als endrekursive Funktionen klassifiziert, wenn der rekursive Aufruf in jedem Zweig der Definition die letzte Aktion zur Berechnung der Funktion ist.
- Verbraucht weniger Arbeitsspeicher
- Kein Stack Overflow
- Schneller als andere Rekursionsarten
"Eine Funktion f, bei der der rekursive Aufruf bereits den Wert der aufrufenden Funktion bestimmt."

Lokale, injektive FUnktion
Eine Funktion, die auf jedem Element des Argumentbereichs definiert ist und wenn gilt, dass der selbe Input den selben Output gibt.
f(a) = f(b) => a=b (Eindeutigkeit der Urbilder von Funktionswerten)

Halteproblem
Die Frage, ob es ein Programm gibt, das für jedes Programm und jede Eingabe entscheiden kann, ob das Programm terminiert.

Variable (Funktionale Sprachen)
Ein Bezeichner für einen Wert

Offshoring
Auslagern von Aufgaben in Billiglohnländer.

Outsourcing
Auslagern von Aufgaben in spezialisierte Unternehmen.

Assoziativität:
Multiplikation: links
  2*2*2 = (2*2)*2 = 4*2
Subtraktion: links
  2-2-2 = (2-2)-2 = 0-2
Division: links
  a/b/c = (a/b)/c
Pow: rechts
  x^y^z = x^(y^z)

Komplexität:
Quadratisch: O(n^2)
Konstant: O(1), O(2), ... => O(1)
Logarithmisch: O(log(n))
Linear: O(n) oder O(n*log(n))
Kubisch: O(n^3)
Exponentiell: O(2^n), O(n!), O(n^k), ...

Pattern (n+1) aktivieren: ghci <file?> -XNPlusKPatterns
  sub1 (n+1) = n

Lazy-evaluation (lazyness ; lazy ; eval ; lazyevaluation)
Ist eine optimierte Auswertungsvariante von call-by-name und wird in Haskell und anderen funktionalen Sprachen verwendet.
  Vorteil:
    - Wenn eine Normalform existiert, wird diese erreicht.
  Beispiel:
    g x = 2*x
    f x = x*x
    f (g 5) = (g 5) * (g 5)
            = 10 * 10 <= Durch lazy-evaluation wird der Schritt (10 * (g 5)) übersprungen.
            = 100

Statisches Typsystem und Vorteile?
Der Datentyp von Funktionen wird statisch während der Übersetzungszeit des Programms abgeleitet. Der Datentyp kann sich während der Laufzeit nicht ändern.
- Datentyp-Fehler werden früher erkannt.
- Durch die Reduzierung der Typ-Überprüfung wird das Programm schneller
- Typ-Inferenz ist möglich

Striktheit ; Pureness ; Purheit ; Reinheit ; Functional purity
Eine Funktion kann lazy ausgeführt werden und terminiert nicht. Sie gibt für den selben Input den selben Output und nimmt immer die selbe Anzahl von Argumenten.
  Definition: f ist strikt wenn (f ⊥ = ⊥)
  "Eine Funktion f ist nach einem ihrer Argumente a strikt, wenn für die Auswertung der Funktion die Auswertung von a notwendig ist."

foldl vs foldr
foldl (:) [] [1,2,3,4,5] => Error (Versucht ((((([]:1):2):3):4):5) auszuführen)
foldr (:) [] [1,2,3,4,5] => [1,2,3,4,5] (Wird zu (1:(2:(3:(4:(5:[]))))))
-}

f :: (Num t) => [[t]] -> t
f = sum . map sum

g :: Foldable t => [t a] -> [String]
g = map (show . length)

--
-- FP Algorithmen
--

-- Un-curryfiziertes map
-- Verwende Tupel für Argumente.
map4 :: ((a -> b), [a]) -> [b]
map4 (_, [])     = []
map4 (f, (x:xs)) = (f x) : map4 (f, xs)

-- First of list
-- first ; fst ; head
head1 :: [a] -> a
head1 (x:xs) = x

-- Rest of list
-- rest ; tail
tail1 :: [a] -> [a]
tail1 []     = []
tail1 (_:xs) = xs

take1 :: (Eq t, Num t) => t -> [a] -> [a]
take1 0 xs = []
take1 _ [] = []
take1 n (x:xs) = x : take1 (n-1) xs

repeat1 :: a -> [a]
repeat1 x  =  x : repeat1 x 

replicate1 :: Int -> a -> [a]
replicate1 n a = take n (repeat1 a)

iterate1 :: (a -> a) -> a -> [a]
iterate1 f a = a : iterate1 f (f a) 

-- Infinite sequence [0, 1, 0, 1, 0, 1, ...]
blink :: [Int]
blink = 0:1:blink

-- List of natural numbers without list generator
nats :: Int -> [Int]
nats n = n: map (+1) (nats n)

--
-- Searching algorithms
--

-- Linear search
-- O(n)
linearSearch :: Eq a  => a -> [a] -> Bool 
linearSearch b []     = False
linearSearch b (a:x)  = (a==b) || linearSearch b x 

-- Binary Search
-- O(log(n))
binarySearch :: Ord a => a -> [a] -> Bool
binarySearch b []     = False
binarySearch b (a:xs) | a < b     = binarySearch b xs 
                      | a == b    = True 
                      | otherwise = False

-- Maximum by comparator with complexity O(n^2)
maxByComparator :: (a -> a -> Bool) -> [a] -> a
maxByComparator _ [x] = x
maxByComparator c (x:xs) | all (\y -> c x y) xs  = x
                         | otherwise = maxByComparator c xs
--
-- help_maxi x1 (x2:[x3, x4, ..., xn])
-- help_maxi x2 (x3:[x4, x5, ..., xn])
-- ...
-- help_maxi x1 []
-- T(n) = c * n + 1
-- T(n) = O(n)
maxByComparator1 :: (a -> a -> Bool) -> [a] -> a
maxByComparator1 _ [x] = x 
maxByComparator1 c (a:b:xs) = help_maxi c a (b:xs) 
  where                                  
    help_maxi _ a []  = a                                   
    help_maxi c a (b:xs)                                              
      | c a b       = help_maxi c a xs
      | otherwise   = help_maxi c b xs

--
-- Insertion sort
--

--   T(n) = (n+1) +(1+2+3+...+n)
-- = ((n+1)*(+2)) / 2
-- = n^2 + 3n + 2
-- = O(n^2)
insertSort [] = [] 
insertSort (a:x) = insert a (insertSort x) 
  where                        
    insert a []    = [a]
    insert a (b:y) |  a<= b    = a:(b:y)                                            
                   | otherwise = b:(insert a y)

--   T(n) = (n+1) +(1+2+3+...+n)
-- = ((n+1)*(+2)) / 2
-- = n^2 + 3n + 2
-- = O(n^2)
insertSortByComparator _ [] = [] 
insertSortByComparator c (a:x) = insert c a (insertSortByComparator c x) 
  where                        
    insert _ a []    = [a]
    insert c a (b:y) | c a b     = a:(b:y)                                            
                     | otherwise = b:(insert c a y)

-- Reverse list (linear recursive)
-- O(n^2)
reverse0 :: [a] -> [a]
reverse0 [] = []
reverse0 (x:xs) = reverse0 xs ++ [x]

-- Reverse list (tail-recursive)
-- O(n)
reverse1 :: [a] -> [a]
reverse1 xs = rev_helper xs []
  where                       
    rev_helper [] ys = ys                   
    rev_helper (x:xs) ys = rev_helper xs (x:ys)

-- Flip elements of function and apply function
-- O(3) = O(1)
flip1 :: (a -> b -> c) -> b -> a -> c
flip1 f x y = f y x

-- Reverse mit foldl
-- O(2n) = O(n)
reverse2 :: [a] -> [a]
reverse2 xs = foldl (flip1 (:)) [] xs

-- Quicksort
-- Works best if all sublists are same size.
-- n times log2(n) => O(n*log(n))
--quicksort  ::  Ord a =>  [a] -> [a]
quicksort [x] = [x]
quicksort (x:xs) = (quicksort  smaller)  ++  x : (quicksort  bigger)
  where  
    smaller = [ y | y <- xs, y<x ] 
    bigger  = [ y | y <- xs, y>=x ]
    
--quicksort  ::  Ord a => [a] -> [a]
quicksort1 _ [x] = [x] 
quicksort1 c (x:xs) = (quicksort1 c smaller)  ++  x : (quicksort1 c bigger)
  where  
    smaller = [ y | y <- xs, c y x ] 
    bigger  = [ y | y <- xs, c x y ]

-- Mergesort

-- O(n)
split :: [a] -> [[a]]
split [] = [] 
split [x] = [[x]] 
split (x:xs) = [x]: (split xs) 

-- O(log(n))
merge :: (Ord a) => [a] -> [a] -> [a] 
merge [] ys = ys
merge xs [] = xs 
merge (x:xs) (y:ys)  | x <= y     = x: (merge xs (y:ys))    
                     | otherwise = y: (merge (x:xs) ys)

-- O(n)
mergeLists :: (Ord a) => [[a]] -> [[a]] 
mergeLists [] = [] 
mergeLists [xs] = [xs] 
mergeLists (xs:ys:xss) = (merge xs ys): mergeLists xss 

-- O(n*log(n))
mergeSort :: (Ord a) => [[a]] -> [[a]] 
mergeSort [x] = [x] 
mergeSort (x:y:xs) = mergeSort (mergeLists (x:y:xs)) 

-- O(n*log(n)) because of MergeSort
startMergeSort :: (Ord a) => [a] -> [a]
startMergeSort xs  =  sortedList 
  where                   
    [sortedList] = mergeSort (split xs)

-- Find longest substring without repeating characters
{-
longestNoRepeat :: String -> String
longestNoRepeat s = reverse (fst (foldl step ("","") s))
  where
    step ("","") c = ([c],[c])
    step (maxSubstr,current) c
      | c `elem` current = step (maxSubstr,init current) c
      | otherwise = let candidate = (c:current)
                        longerThan = (>) `on` length
                        newMaxSubstr = if maxSubstr `longerThan` candidate
                                       then maxSubstr
                                       else candidate
                    in (newMaxSubstr, candidate)
-}

-- Common prefix
cpfx :: [[Char]] -> [Char]
cpfx []     = []
cpfx [x]    = x
cpfx (x:xs) = cpfx' (x:xs) 0
 
cpfx' :: [[Char]] -> Int -> [Char]
cpfx' [x]    _ = []
cpfx' (x:xs) n
  | ifMatch (x:xs) n = x!!n : cpfx' (x:xs) (n+1)
  | otherwise = []

ifMatch :: [[Char]] -> Int -> Bool
ifMatch [x]      _ = True
ifMatch [x,y]    n = x!!n == y!!n
ifMatch (x:y:xs) n
      | x!!n == y!!n = ifMatch xs n
      | otherwise = False

-- Common prefix 2
commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []

-- Apply function twice
twoTimes  ::  (a -> a) -> a -> a
twoTimes  f  x  =  f ( f  x )

-- Map
map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = (f x) : (map2 f xs)

-- Map with list generator
map3 :: (a -> b) -> [a] -> [b]
map3 f xs = [f x | x<-xs]

-- Map until condition is met
mapUntil :: (t -> a) -> (t -> Bool) -> [t] -> [a]
mapUntil f p [] = []
mapUntil f p (x:xs) | (p x) = []
                    | otherwise = (f x) : mapUntil f p xs

-- Filter
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p [] = [] 
filter2 p (x:xs) | p  x  =  x : filter2 p xs
                 | otherwise = filter2 p xs

-- takeWhile

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 p [] = []
takeWhile2 p (x:xs)
  | p x = x : takeWhile2 p xs
  | otherwise = []

-- takeUntil

takeUntil2 :: (a -> Bool) -> [a] -> [a]
takeUntil2 p [] = []
takeUntil2 p (x:xs)
  | p x = []
  | otherwise = x : takeUntil2 p xs

-- dropWhile

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 p [] = []
dropWhile2 p (x:xs) | p x = dropWhile2 p xs
                    | otherwise = x:xs

-- dropUntil

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p [] = []
dropUntil p (x:xs) | p x = x:xs
                   | otherwise = dropUntil p xs

-- zip

-- zip [1..] "abcd" => [(1,'a'), (2,'b'), (3,'c'), (4,'d')]
zip2 :: [a] -> [b] -> [(a,b)]
zip2 (x:xs) (y:ys) = (x,y) : zip2 xs ys
zip2 _  _          = []

-- zipWith

-- zipWith (^) [1, 2, 3] [0, 3, 2] => 1, 8, 9]
zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 f (x:xs) (y:ys) = (f x y):(zipWith2 f xs ys)
zipWith2 f _ _ = []

-- Un-currifiziertes zipWith
zipWith4 :: ((a -> b -> c), [a], [b]) -> [c]
zipWith4 (f, (x:xs), (y:ys)) = (f x y):(zipWith4 (f, xs, ys))
zipWith4 (_, _, _) = []

-- zipWith mit Listengenerator
zipWith6 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith6 f xs ys = [(f (xs !! n) (ys !! n)) | n <- [0 .. ((min (length xs) (length ys))-1)]]

-- zipWith mit Listengenerator (V2)
zipWith7 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith7 f xs ys = [(f x y) | (x,y) <- zip xs ys] 

--
-- contains ; include ; includes ; issublist ; is sublist ; subseq ; subsequence ; substring ; infix ; infixof ; isinfixof
--

subseq :: Eq a => [a] -> [a] -> Bool
subseq (_:_) [] = False
subseq xs ys
    | seqprefix xs ys = True
    | subseq xs (tail ys) = True
    | otherwise = False

seqprefix :: Eq a => [a] -> [a] -> Bool
seqprefix [] _ = True
seqprefix (_:_) [] = False
seqprefix (x:xs) (y:ys) = (x == y) && seqprefix xs ys

substring :: String -> String -> Bool
substring (_:_) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

prefix :: String -> String -> Bool
prefix [] _ = True
prefix (_:_) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

-- 
--  contains ; include ; includes ; elem ; has element ; indexof
--

elem1 :: Eq a => a -> [a] -> Bool
elem1 _ []     = False
elem1 x (y:ys) | x == y = True
               | otherwise = elem1 x ys

index1 :: Eq a => a -> [a] -> Int
index1 _ []     = 0
index1 x (y:ys) | x == y = 0
                | otherwise = 1 + index1 x ys

-- All

all2 :: (a -> Bool) -> [a] -> Bool
all2 p xs = and [p x | x <- xs]

all3 :: (a -> Bool) -> [a] -> Bool
all3 p [] = True
all3 p (x:xs) | p x = all3 p xs
              | otherwise = False

-- Any

any2 :: (a -> Bool) -> [a] -> Bool
any2  p  xs = or [p x | x <- xs]

any3 :: (a -> Bool) -> [a] -> Bool
any3 p []     = False
any3 p (x:xs) | p x = True
              | otherwise = any3 p xs

-- None

none2 :: (a -> Bool) -> [a] -> Bool
none2  p  xs = or [p x | x <- xs]

none3 :: (a -> Bool) -> [a] -> Bool
none3 p []     = True
none3 p (x:xs) | p x = False
               | otherwise = none3 p xs

-- Sum (add all ; addall)

sum2 :: (Num a) => [a] -> a
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs

sum3 :: (Num a) => [a] -> a
sum3 [] = 0
sum3 (x:xs) = x + sum3 xs
  where sum3' [] acc = acc
        sum3' (x:xs) acc = sum3' xs (acc + x)

sum4 = betweenAll (+) 0

-- multiply all (multall ; mulall)

mulAll :: (Num a) => [a] -> a
mulAll [] = 0
mulAll (x:xs) = x * mulAll xs

mulAll3 = betweenAll (*) 1

-- All true

allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs) = x && allTrue xs

allTrue2 = betweenAll (&&) True

-- betweenAll

betweenAll :: (a -> a -> a) -> a -> [a] -> a
betweenAll f k [] = k
betweenAll f k (x:xs) = f x (betweenAll f k xs)

-- length of list (size)

length2 :: [a] -> Int
length2 xs = foldl addOne 0 xs
  where addOne a b = a + 1

length3 :: [a] -> Int
length3 []     = 0
length3 (x:xs) = 1 + length3 xs

-- foldl

foldl2 :: (a -> b -> a) -> a -> [b] -> a
foldl2 f z []     = z
foldl2 f z (x:xs) = foldl2 f (f z x) xs

-- pow

-- pow using foldl and list generator
pow :: Integer -> Int -> Integer
pow b n = foldl (*) 1 (take n [b,b .. b])

-- pow without list generator
pow1 :: Integer -> Integer -> Integer
pow1 n m = natFold (*n) 1 m
  where
    natFold :: (a -> a) -> a -> Integer -> a
    natFold h c 0 = c
    natFold h c n = h (natFold h c (n-1))

-- (++)
-- concat ; append ; appendall ; prepend ; concatenate
{-
-- O(n) wobei n die Lnge der ersten Liste ist.
(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)
-}

--
-- bubbleSort
--

-- Check sorted with comparator
-- O(n)
isSorted :: (Ord a) => (a -> a -> Bool) -> [a] -> Bool
isSorted c xs = and (zipWith c xs (tail xs))

-- O(n^2)
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs  | isSorted (<=) xs = xs
               | otherwise = bubbleSort (moveBubble xs) 
  where                       
    moveBubble [] = []  
    moveBubble [x] = [x]      
    moveBubble (x:y:rest) | (<=) x y   = x: moveBubble (y:rest)                                                           
                          | otherwise  = y: moveBubble (x:rest)

--
-- ( composition ; functioncomposition .-operator ; dotoperator ; dot-operator )
-- Funktions-Komposition:
--
{-
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) g f x = (g (f x))
-}

--
-- Pseudo-Sets
--

-- remove ; Opposite of filter
-- O(n)
remove :: (a -> Bool) -> [a] -> [a]
remove f = filter (not . f)

-- Remove element from List
-- O(n)
delete1 :: Eq t => t -> [t] -> [t]
delete1 _ []                 = []
delete1 x (y:ys) | x == y    = ys
                 | otherwise = y : delete1 x ys

-- Remove element from List (using remove)
-- O(n)
delete2 :: Eq t => t -> [t] -> [t]
delete2 x ys = remove (==x) ys

-- Remove element from List (using filter
-- O(n)
delete3 :: Eq t => t -> [t] -> [t]
delete3 x ys = filter (/=x) ys

-- make unique ; distinct
-- O(n^2)
unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : unique (filter (/=x) xs)

-- Make list into set (sort and make unique)
-- O(n^2)
makeSet :: Ord a => [a] -> [a]
makeSet xs = unique (insertSort xs)

{-
binarySearch :: Ord a => a -> [a] -> Bool
binarySearch b []  = False
binarySearch b (a:xs) | a < b     = binarySearch b xs 
                      | a == b    = True 
                      | otherwise = False
-}

-- Check if element is in set
-- O(log(n))
inSet :: Ord a => a -> [a] -> Bool
inSet = binarySearch

-- Make union of two sets
-- O(makeSet)
union :: Ord a => [a] -> [a] -> [a]
union xs ys = makeSet (xs ++ ys)

-- Check if xs is in ys
-- O(n*log(n)
subset :: Ord a => [a] -> [a] -> Bool
subset []     _  = True
subset (x:xs) ys = (inSet x ys) && subset xs ys

-- intersection of sets
-- O(n*log(n))
intersection :: Ord a => [a] -> [a] -> [a]
intersection xs ys = filter (\x -> inSet x ys) xs

-- Difference of sets
-- O(n^2 + (2 * (n*log(n)))) = O(n^2)
(\\) :: Ord a => [a] -> [a] -> [a]
(\\) xs ys = union (remove (\x -> inSet x ys) xs) (remove (\y -> inSet y xs) ys)

{-
-- O(n^2)
(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) [] ys = []
(\\) (a:xs) ys | elem a ys   = (\\) xs ys
               | otherwise   = a : (\\) xs ys
-}

-- Count occurences of element in list 
-- frequency ; freq
-- O(n)
countOf :: (Num p, Eq t) => t -> [t] -> p
countOf _ []     = 0
countOf x (y:ys) | x == y = 1 + countOf x ys
                 | otherwise = countOf x ys

-- Count occurences of element in list (tail recursive)
-- frequency ; freq
-- O(n)
countOf1 :: (Num p, Eq t) => t -> [t] -> p
countOf1 _ []     = 0
countOf1 x (y:ys) | x == y = 1 + countOf1 x ys
                  | otherwise = countOf1 x ys
  where 
    countOf1' _ []     acc = acc
    countOf1' x (y:ys) acc | x == y = countOf1' x ys (acc+1)
                           | otherwise = countOf1' x ys acc

--
-- Selectionsort 
--

{-
-- O(n)
maxByComparator1 :: (a -> a -> Bool) -> [a] -> a
maxByComparator1 _ [x] = x 
maxByComparator1 c (a:b:xs) = help_maxi c a (b:xs) 
  where                                  
    help_maxi _ a []  = a                                   
    help_maxi c a (b:xs)                                              
      | c a b       = help_maxi c a xs
      | otherwise   = help_maxi c b xs

-- Remove element from List (using filter
-- O(n)
delete3 :: Eq t => t -> [t] -> [t]
delete3 x ys = filter (/=x) ys
-}

-- O(n^2)
selectionSort' :: Ord t => (t -> t -> Bool) -> [t] -> [t] -> [t]
selectionSort' _ sorted [] = sorted
selectionSort' c sorted unsorted = selectionSort' c (mx : sorted) (delete3 mx unsorted) -- O(n^2)
  where mx = maxByComparator1 (\x y -> c y x) unsorted -- O(n)

-- O(n^2)
selectSort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
selectSort c xs = selectionSort' c [] xs

-- First natural number which is not in list

-- O(n^2)
firstNatNotIn :: [Integer] -> Integer
firstNatNotIn xs = head ([0..] \\ xs)

--
-- First element to satisfy predicate
--

first_by_pred :: (a -> Bool) -> [a] -> Maybe a
first_by_pred p xs =
    let ys = filter p xs
    in if (null ys) then Just (head ys) else Nothing

first_by_pred2 :: (a -> Bool) -> [a] -> Maybe a
first_by_pred2 _ [] = Nothing
first_by_pred2 p (x:xs) | p x = Just x
                        | otherwise = first_by_pred2 xs

--
-- Stack
--

type Stack a = [a]

leer :: Stack a
leer = []

push :: a -> Stack a -> Stack a
push a s = a:s

top :: Stack a -> a
top s = head s

pop :: Stack a -> Stack a
pop (a:s) = s

istLeer :: Stack a -> Bool
istLeer [] = True
istLeer _  = False

-- 
-- data und type
-- 

data Bit = One | Zero0 deriving Eq
type Bits = [Bit]

pack' :: Bits -> Integer -> [Integer] -> [Integer]
pack' []         _ _   = [0]
pack' [b]        n acc = acc ++ [n + 1]
pack' (b0:b1:bs) n acc | b0 == b1 = pack' (b1:bs) (n+1) acc
                       | otherwise = pack' (b1:bs) 0 (acc ++ [n + 1])

pack1 :: Bits -> [Integer]
pack1 bs = pack' bs 0 []

-- Hanoi

type Turm = (Char, Char, Char)
type Move = (Int, Char, Char)

hanoi  ::  Int -> Turm -> [Move]
hanoi  0          _  =  []
hanoi  n (a, b, c) = hanoi (n-1) (a, c, b) ++ [(n,a,c)] ++ hanoi (n-1) (b, a, c)

-- More data

data Nat = Zero | S Nat deriving Show
data ZInt = Z Nat Nat deriving Show
data B = T | F deriving Show

add :: Nat-> Nat-> Nat
add a Zero = a
add a (S b) = add (S a) b

mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult a (S b) = add a (mult a b)

foldn :: (Nat -> Nat) -> Nat -> Nat -> Nat
foldn h c Zero  = c
foldn h c (S n) = h (foldn  h  c  n)

-- Check numbers for equality
equal :: Nat -> Nat -> B
equal Zero Zero = T
equal Zero _    = F
equal _    Zero = F
equal (S n) (S m) = equal n m

-- Check B for equality
bitEql :: B -> B -> B
bitEql T T = T
bitEql F F = T
bitEql _ _ = F

natXor :: Nat -> Nat -> B
natXor Zero Zero = F
natXor Zero _    = T
natXor _    Zero = T
natXor _    _    = F

xor ::  B -> B -> B
xor T F = T
xor F T = T
xor _ _ = F

smaller :: Nat -> Nat -> B
smaller Zero Zero = F
smaller _    Zero = F
smaller Zero _    = T
smaller (S n) (S m) = smaller n m

power :: Nat -> Nat -> Nat
power Zero Zero = error "undefined"
power _ Zero  = S Zero
power n (S m) = mult n (power n m)

-- Power using foldn
powerf :: Nat -> Nat -> Nat
powerf Zero Zero = error "undefined"
powerf m n = foldn (mult m) (S Zero) n

-- Stuff with Maybe

data Maybe1 a = Nothing1 | Just1 a 
  deriving (Eq, Ord, Show)

-- Tmajority(n)
-- = Tlocal_maj(n) + Tfreq(n) + Thalf(n)
-- = c1 * n + c2 * n + c3
-- = (c1 + c2) * n + c3
-- = O(n)
majority :: (Eq a) => [a] -> Maybe1 a
majority []   = Nothing1
majority [x]  = Just1 x
majority xs | (freq l_maj xs) > half = (Just1 l_maj)
            | otherwise = Nothing1
  where
    (c, l_maj) = local_maj (1, head xs) (tail xs)
    local_maj (n, m) [] = (n, m)
    local_maj (n, m) (e:es)
      | e==m = local_maj (n+1,m) es
      | (e/=m && n==0) = local_maj (1, e) es
      | otherwise = local_maj (n-1,m) es
    half = div (length xs) 2

freq :: Eq a => a -> [a] -> Int
freq e xs = sum [ 1 | x<-xs, x == e ]

--
-- Tree stuff
--
data SimpleBT  =  L | N SimpleBT SimpleBT

nodes :: SimpleBT -> Integer
nodes L = 1
nodes (N leftT rightT) = 1 + nodes leftT + nodes rightT

pfad :: SimpleBT -> Integer
pfad L = 0
pfad (N lt rt) = (pfad rt) + (pfad lt) + (nodes (N lt rt)) - 1             

height :: SimpleBT -> Integer
height L = 0
height (N lt rt) = (max (height lt) (height rt)) + 1

balanced :: SimpleBT -> Bool
balanced  L = True
balanced  (N lt rt) = (balanced lt) && (balanced rt) && height lt == height rt

balanced :: SimpleBT -> Bool
balance1 tree = (size tree) == (2^((height tree)+1)-1)
  where
    size :: SimpleBT -> Integer
    size L = 1
    size (N lt rt) = size lt + size rt + 1

{-
Zur Erinnerung:
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) g f x = (g (f x))

Reduzierung zur Normalform:

Ausdruck 1:

com :: (a -> a) -> (a -> a) -> a -> (a, a) 
com f g x = ((f . g) x, (g . f) x)

com (*10) (mod 10) 7
= ((((*10).(mod 10)) 7), (((mod 10).(*10)) 7))
= (((*) (mod 10 7) 10), (((mod 10).(10 * 7)) 7))
= (((*) 3 10), (mod 10 70))
= (30, 10)

Ausdruck 2:

  foldl (\ys x -> x:ys) [] (take 4 [1..])
= foldl (\ys x -> x:ys) [] [1,2,3,4]
= foldl (\ys x -> x:ys) [1] [2,3,4]
= foldl (\ys x -> x:ys) [2,1] [3,4]
= foldl (\ys x -> x:ys) [3,2,1] [4]
= foldl (\ys x -> x:ys) [4,3,2,1] []
= [4,3,2,1]

Ausdruck 3:

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs

  foldl1 ((+).(*2)) [1,0,1,0]
= foldl ((+).(*2)) 1  [0,1,0]
= foldl ((+).(*2)) (((+).(*2)) 1 0) [1,0]
= foldl ((+).(*2))  2  [1,0]
= foldl ((+).(*2)) (((+).(*2)) 2 1)  [0]
= foldl ((+).(*2)) 5 [0]
= foldl ((+).(*2)) (((+).(*2)) 5 0) [] 
= foldl ((+).(*2)) 10 []
= 10

Ausdruck 4

  [x | xs<-["zwei", "drei", "vier"], x<-xs, (x/='e'), (x/=‘i')]
= ['z', 'w', 'e', 'i', 'd', 'r', 'e', 'i', 'v', 'i', 'e', 'r']
= ['z', 'w', 'd', 'r', 'v', 'r']
= "zwdrvr"

Ausdruck 5
  ((foldr (+) 1) . (map (div 4))) [1..5]
= foldr (+) 1 (map (div 4) [1,2,3,4,5])
= foldr (+) 1 ((div 4 1):(map (div 4) [2,3,4,5]))
= foldr (+) 1 ((div 4 1):(div 4 2):(div 4 3):(map (div 4) [4,5]))
= foldr (+) 1 ((div 4 1):(div 4 2):(div 4 3):(div 4 4):(map (div 4) [5]))
= foldr (+) 1 ((div 4 1):(div 4 2):(div 4 3):(div 4 4):(div 4 5):(map (div 4) []))
= foldr (+) 1 ((div 4 1):(div 4 2):(div 4 3):(div 4 4):(div 4 5):[])
= foldr (+) 1 ((div 4 1):(div 4 2):(div 4 3):(div 4 4):[0])
= foldr (+) 1 ((div 4 1):(div 4 2):(div 4 3):[1,0])
= foldr (+) 1 ((div 4 1):(div 4 2):[1,1,0])
= foldr (+) 1 ((div 4 1):[2,1,1,0])
= foldr (+) 1 [4,2,1,1,0]
= foldr (+) ((+) 1 4) [2,1,1,0]
= foldr (+) ((+) ((+) 1 4) 2) [1,1,0]
= foldr (+) ((+) ((+) ((+) 1 4) 2) 1) [1,0]
= foldr (+) ((+) ((+) ((+) ((+) 1 4) 2) 1) 1) [0]
= foldr (+) ((+) ((+) ((+) ((+) ((+) 1 4) 2) 1) 1) 0) []
= foldr (+) ((+) ((+) ((+) ((+) 5 2) 1) 1) 0) []
= foldr (+) ((+) ((+) ((+) 7 1) 1) 0) []
= foldr (+) ((+) ((+) 8 1) 0) []
= foldr (+) ((+) 9 0) []
= foldr (+) 9 []
= 0
-}
















































