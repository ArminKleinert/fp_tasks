{-
Funktionale Programmierung Übung 2 
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- 3.

-- Recursively iterates a string and returns a list of chars containing only the chars
-- '(', '{', '[', ']', '}', ')' in the order in which they appear in the string.
-- For the result, a second string is used as a buffer to achieve tail-recursion.
onlyParenthesisSub :: [Char] -> [Char] -> [Char]
onlyParenthesisSub [] res = res
onlyParenthesisSub (fc:rest) res  =
  let allParens = "({[]})"
  in onlyParenthesisSub rest (if elem fc allParens then (res ++ [fc]) else res)

-- Basically just calls onlyParenthesisSub
onlyParenthesis :: [Char] -> [Char]
onlyParenthesis s = onlyParenthesisSub s ""

--

test :: IO ()
test = putStrLn ("onlyParenthesis \"\": " ++ (show (onlyParenthesis "")) ++
                 "\nonlyParenthesis [(2+7.0)*a-(xyz), {word}]: " ++ 
                 (show (onlyParenthesis "[(2+7.0)*a-(xyz), e{word}]")) ++
                 "\nonlyParenthesis (replicate 50 '('): " ++
                 (show (onlyParenthesis (replicate 50 '('))) ++
                 "\nonlyParenthesis (replicate 50 'a'): " ++
                 (show (onlyParenthesis (replicate 50 'a'))))
