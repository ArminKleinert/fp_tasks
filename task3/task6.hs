{-
Funktionale Programmierung Übung 3
Abgabe von Armin Kleinert, Anna Sophie Pipperr und Alexandra Wolfers
-}

-- A6

-- Checks whether or not a given Char is a divider (',', '.', '?' or ' ')
isDivider :: Char -> Bool
isDivider c = elem c ",.? "

-- Helper for tokenizer. Uses the input string (the full text), another
-- string (for the current token) and a 2-dimensional list of strings 
-- (for the output).
-- If a divider is encountered, the current token is appended to the output.
-- The input is processed in reverse order, if this function jjjj. 
-- Example:
--   tokenizerSub ".ereht yeH" "" [] -- The divider '.' is encountered, so add the
--                                   -- current, empty accumulator to the output.
--   tokenizerSub "ereht yeH" "" [""] -- Start processing the word "ereht" ("there")
--   tokenizerSub "reht yeH" "e" [""] 
--   tokenizerSub "eht yeH" "re" [""]
--   tokenizerSub "ht yeH" "ere" [""]
--   tokenizerSub "t yeH" "here" [""]
--   tokenizerSub " yeH" "there" [""] -- ' ' found, put word into result and continue
--   tokenizerSub "yeH" "" ["there", ""]
--   tokenizerSub "eH" "y" ["there", ""]
--   tokenizerSub "H" "ey" ["there", ""]
--   tokenizerSub "" "Hey" ["there", ""] -- String empty, put current into result:
--   tokenizerSub "" "" ["Hey", "there", ""] -- return result
tokenizerSub :: [Char] -> [Char] -> [[Char]] -> [[Char]]
tokenizerSub ""    curr acc = curr : acc
tokenizerSub (c:s) curr acc = if isDivider c
                              then tokenizerSub s "" (curr:acc)
                              else tokenizerSub s (c:curr) acc

-- Takes a string and splits it at the characters ',', '.', '?' and ' '.
--   tokenizer "tokenizer :: [Char] -> " => ["tokenizer","::","[Char]","->",""]
-- Basically just calls tokenizerSub with reversed input.
tokenizer :: [Char] -> [[Char]]
tokenizer s = tokenizerSub (reverse s) "" []

--

test :: IO ()
test = putStrLn ("tokenizer..."        ++
                 "\n\"tion tokenizer, die aus ei\": " ++ (show (tokenizer "tion tokenizer, die aus ei")) ++
                 "\n\"Hey there.\":                 " ++ (show (tokenizer "Hey there.")))
