{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A7

-- Vorgegebene Funktion
unfold p f g x | p x = []
               | otherwise = f x : unfold p f g (g x)

-- Implementation of map using unfold
--    predicate p is "null", which checks if a list is empty
--    f is supposed to only be applied to the first element in the list, not the full list. So it is composed with the "head" function which gets the first element of the list.
--    g from unfold gets the rest of the list, so the fitting choice is "tail", which returns the rest of a list.
map2 :: (t -> a) -> [t] -> [a]
map2 f xs = unfold null (f . head) tail xs

-- Implementation of iterate using unfold
--    predicate p is always false, because iterate returns an infinite sequence
--    g just returns its argument and does nothing with it
--    f stays as it is.
iterate2 :: (a -> a) -> a -> [a]
iterate2 f x = unfold (\_ -> False) (\x -> x) f x

-- TODO Implement toDecFrom also.

-- TODO Tests
test :: IO ()
test = putStrLn ""
