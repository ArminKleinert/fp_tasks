{-# LANGUAGE NPlusKPatterns #-}

-- 1.

gcd2 a b = if a==0 then b else gcd2 (mod b a) a

-- kleines gemeinsames Vielfaches ; lcm
kgV :: Integer -> Integer -> Integer
kgV n m = (div n  (gcd2 n m)) * m

-- 2.

--subsets 0 _ = [[]]
--subsets _ [] = []
--subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

-- pot ; all combinations ; combinations ; allcombinations
--pot :: [t] -> [[t]]
--pot xs = foldl (++) [] (map (\n -> subsets n xs) [0 .. (length xs)])

{-
pot [1,2]
= foldl (++) [] (map (\n -> subsets n [1,2]) [0 .. (length [1,2])])
= foldl (++) [] (map (\n -> subsets n [1,2]) [0 .. 2])
= foldl (++) [] (map (\n -> subsets n [1,2]) [0, 1, 2])
= foldl (++) [] (map (\n -> subsets n [1,2]) [0, 1, 2])
= foldl (++) [] (((\n -> subsets n [1,2]) 0) : map (\n -> subsets n [1,2])) [1, 2])
= foldl (++) [] ((((\n -> subsets n [1,2]) 0) : ((\n -> subsets n [1,2]) 1) : map (\n -> subsets n [1,2])) [2])
= foldl (++) [] ((((\n -> subsets n [1,2]) 0) : ((\n -> subsets n [1,2]) 1) : ((\n -> subsets n [1,2]) 2) : map (\n -> subsets n [1,2])) [])
= foldl (++) [] (((\n -> subsets n [1,2]) 0) : ((\n -> subsets n [1,2]) 1) : ((\n -> subsets n [1,2]) 2) : [])
= foldl (++) [] ((subsets 0 [1,2]) : ((\n -> subsets n [1,2]) 1) : ((\n -> subsets n [1,2]) 2) : [])
= foldl (++) [] ([] : ((\n -> subsets n [1,2]) 1) : ((\n -> subsets n [1,2]) 2) : [])
= foldl (++) [] ([] : (subsets 1 [1,2]) : ((\n -> subsets n [1,2]) 2) : [])
= foldl (++) [] ([] : [[1],[2]] : (subsets 2 [1,2]) : [])
= foldl (++) [] ([] : [[1],[2]] : [[1,2]] : [])
= foldl (++) [] ([[1],[2],[1,2]])
-}

-- pot ; all combinations ; combinations ; allcombinations
pot :: [t] -> [[t]]
pot [] = [[]]
pot (t:ts) = pot ts ++ map (t:) (pot ts)

-- 3.

firstMatch :: (a -> Bool) -> [a] -> Maybe a
firstMatch p [] = Nothing
firstMatch p (x:xs) = if (p x) then Just x else firstMatch p xs

filter2 _ [] = []
filter2 p (x:xs) | p x = x : filter2 p xs
                 | otherwise = filter2 p xs

matchIndices p ls = miSub p ls 0
  where miSub _ [] _ = []
        miSub p (x:xs) i | p x = i : miSub p xs (i+1)
                         | otherwise = miSub p xs (i+1)

-- 4.

sp :: [String] -> IO()
sp xs = putStr (sptransponiere (spnormalisiere xs))

spnormalisiere s = map (indentation blanks) s
  where indentation blanks str = str ++ [' ' | x <- [1..blanks-length str]]
        blanks = foldl max 0 (map length s)

sptransponiere :: [String] -> String
sptransponiere s = foldr (++) [] (foldr op [] s)
  where op str [] = [[ch] ++ "\n" | ch <- str]
        op (ch:rest) (cha:rest') = (ch:cha) : op rest rest'

-- 5.

-- 6.
{-
run :: [t] -> u -> u
run [] s = s                        -- run.1
run (i:il) s = run il (execute i s) -- run.2

execute :: t -> u -> u
execute i s = ...

(++) [] ys = ys                 -- (++).1
(++) (x:xs) ys = x:((++) xs ys) -- (++).2

Beweis über die Struktur von il1
Zu zeigen: run ((++) il1 il2) s = run il2 (run il1 s)

I.A. il1 = []
run ((++) [] il2) s =? run il2 (run [] s)
run il2 s           =? run il2 (run [] s) -- (++).1
run il2 s           =? run il2 s -- run.1
     
I.V. il1 = il1'
run ((++) il1' il2) s = run il2 (run il1' s)

I.S. il1 = a : il1'

run ((++) (a:il1') il2) s         = run il2 (run (a:il1') s)
run (a:((++) il1' il2)) s         = run il2 (run (a:il1') s) -- (++).2
run ((++) il1' il2) (execute a s) = run il2 (run (a:il1') s) -- run.2
run ((++) il1' il2) (execute a s) = run il2 (run il1' (execute a s)) -- run.2
run il2 (run il1' (execute a s))  = run il2 (run il1' (execute a s)) -- I.V.
-}


Lustig, dass Spotify von "Enemy of the night" auf "The day is my enemy" umschaltet.








