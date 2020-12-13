{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A5

-- Flattens a list of lists using foldr
flatten_r :: [[a]] -> [a]
flatten_r list = foldr (++) [] list

-- Flattens a list of lists using foldl
flatten_l :: [[a]] -> [a]
flatten_l list = foldl (++) [] list

-- Was sind die Vor- und Nachteile der Implementationen?

-- Der Vorteil der Implementation mit foldl ist, dass sie sehr einfach und offensichtlich ist.
-- Der Nachteil ist, dass der Code mit foldl vergleichsweise langsam ist.
-- Zum Beispiel: Der Befehl (flatten_l (map (\x -> [x]) [0..100000])) benötigt bei mir mehrere Minuten. Die Implementation mit foldr ist in wenigen Sekunden fertig.

-- Der Vorteil der Implementation mit foldr ist, dass sie sehr schnell läuft.
-- Der Nachteil ist, dass sie bei sehr großen Inputs augrund zu tiefer Rekursion einen StackOverflow verursachen kann (Dieses Limit wurde im meinen Tests aber noch nicht erreicht).

test :: IO ()
test = putStrLn ("flatten_l" ++
                 "\n[[1,2,3,4],[5],[0,9,8],[7],[],[6,5,4,3,2,1],[]] " ++
                 (show (flatten_l [[1,2,3,4],[5],[0,9,8],[7],[],[6,5,4,3,2,1],[]])) ++
                 "\n(map (\\x -> [x]) [0..1000]) == [0..1000] " ++ 
                 (show ((flatten_l (map (\x -> [x]) [0..1000])) == [0..1000])) ++
                 "\n" ++ -- Switch to foldr-tests
                 "flatten_r" ++ 
                 "\n[[1,2,3,4],[5],[0,9,8],[7],[],[6,5,4,3,2,1],[]] " ++ 
                 (show (flatten_r [[1,2,3,4],[5],[0,9,8],[7],[],[6,5,4,3,2,1],[]])) ++
                 "\n(map (\\x -> [x]) [0..100000]) == [0..100000] "++
                 (show ((flatten_r (map (\x -> [x]) [0..100000])) == [0..100000])))

