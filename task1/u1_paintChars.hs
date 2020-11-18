
{-- Funktionale Programmierung, U1, 2020/2021 Author: M. Esponda --}

paintChars f size = putStrLn (genChars f size)

genChars :: ((Int, Int, Int) -> Char) -> Int -> [Char]
genChars f size = paint size (map f [(x,y,size) | y <- [1..size], x <- [1..size]])
                  where
                  paint 0  []     = []
                  paint 0 (c:cs)  = '\n' : (paint size (c:cs))
                  paint n (c:cs)  = c: (paint (n-1) cs)

{-- Funktionsbeispiele für die 3.Aufgabe des 1.Übungsblattes   --}

diag (x,y,size) = if (x==y) then 'O' else '.'

quad (x,y,size) = if (x>s && x<3*s && y>s && y<3*s) then ' ' else '+'
                  where
                  s = div size 4

gitter (x,y,size) = if k || p  then ' ' else '0'
                    where
                    k = (mod x space)==0
                    p = (mod y space)==0
                    space = div size 4

{- Testfunktionen -}

test_diag = paintChars diag 40
test_quad = paintChars quad 40
test_gitter = paintChars gitter 40
