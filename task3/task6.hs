{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert, Anna Sophie Pipperr und Alexandra Wolfers
-}

-- A6

tokenizer :: String -> [String]
tokenizer str | nextWord == filter withoutTrenn str = nextWord:[] --Rekursionsanker: wenn es nur noch ein Wort gibt
              | otherwise = nextWord:(tokenizer (drop (index+1) string)) --hier muss der String mit einem Buchstabe vorne weitergegeben werden
                where (string, nextWord, index) = word 0 0 str str

withoutTrenn :: Char -> Bool
withoutTrenn c = not (elem c ".?, ")

inList :: Eq a => a -> [a] -> Bool
inList _ [] = False
inList z (x:xs) | z == x = True
                | otherwise = inList z xs

--word gibt den gesamten (String mit Buchstaben vorne!), das erste Wort des Strings und den Index im ganzen String als Tuple zurück
--a: 0 -> Noch kein Buchstabe, 1 -> Bereits ein Buchstabe in String betrachtet
word :: Int -> Int -> String -> String -> (String, String, Int)                
word a i str [] = (str,str,i)
word a i str (x:xs) | inList x trenn && a == 0 = word 0 i xs xs
                    | inList x trenn = (str, take i str, i)
                    | otherwise = word 1 (i+1) str xs
                     where trenn = ".?, "



--

test :: IO ()
test = putStrLn ("tokenizer..."        ++
                 "\n\"tion tokenizer, die aus ei\": " ++ (show (tokenizer "tion tokenizer, die aus ei")) ++
                 "\n\"Hey there.\":                 " ++ (show (tokenizer "Hey there.")))
