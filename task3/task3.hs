{-
Funktionale Programmierung Übung 2 
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A3

-- insert '3' 3 "Hello" => "Hel3lo"
insert :: t2 -> Integer -> [t2] -> [t2]
insert e 0 l      = e : l
insert e i []     = error "Index too big."
insert e i (s:ls) = s : insert e (i-1) ls
