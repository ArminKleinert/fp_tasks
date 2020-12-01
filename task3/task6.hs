{-
Funktionale Programmierung Übung 2 
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A6

isDivider :: Char -> Bool
isDivider c = elem c ",.? "

tokenizerSub :: [Char] -> [Char] -> [[Char]] -> [[Char]]
tokenizerSub ""    curr acc = reverse (acc ++ [curr])
tokenizerSub (c:s) curr acc = if isDivider c
                              then tokenizerSub s "" (acc ++ [curr])
                              else tokenizerSub s (c:curr) acc

tokenizer :: [Char] -> [[Char]]
tokenizer s = tokenizerSub (reverse s) "" []
