{-# LANGUAGE NPlusKPatterns #-}

-- 1)

type Sortiment = [Eintrag]
type Eintrag = (Artikel,Preis)
type Artikel = String
type Preis = Integer

{-
preis :: Sortiment -> String -> Preis
preis waren name = if (e == Nothing) then e else (snd e)
  where
    es = (filter (\x -> (fst x) == name) waren)
    e = if (null es) then Nothing else Just (head es)
-}

artikelAnzahl :: Sortiment -> String
artikelAnzahl waren = length waren

maxByComparator :: (a -> a -> Bool) -> [a] -> a
maxByComparator _ [x] = x
maxByComparator c (x:xs) | all (\y -> c x y) xs  = x
                         | otherwise = maxByComparator c xs

teuersterArtikel :: Sortiment -> String
teuersterArtikel waren = fst (maxByComparator (\x y -> (snd x) >= (snd y)) waren)

-- 2)



-- 3)




