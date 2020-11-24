-- Helper for num2GermanWord (n < 10 && n /= 0)
-- Achtung! (1 => "ein", nicht "eins")
singleDigits :: Int -> [Char]
singleDigits i = words !! i
  where
    words = ["",  "ein", "zwei", "drei", "vier", "fünf", "sechs", "sieben", "acht", "neun"]

-- Helper for num2GermanWord (n >= 10 && n < 20)
twoDigits :: Int -> [Char]
twoDigits i = words !! i
  where
    words = ["zehn", "elf", "zwölf", "dreizehn", "vierzehn", "fünfzehn", "sechszehn", "siebzehn", "achtzehn", "neunzehn"]

-- Helper for num2GermanWord (multiples of 10)
tensMultiple :: Int -> [Char]
tensMultiple i = words !! i
  where
    words = ["", "zehn", "zwanzig", "dreizig", "vierzig", "fünfzig", "sechszig", "siebzig", "achtzig", "neunzig"]

-- Capitalize first letter of string.
capitalized :: [Char] -> [Char]
capitalized (head:tail) = toUpper head : map toLower tail
capitalized []          = []

-- Helper for num2GermanWordSub
-- Called if n>=20
-- if n%10/=0 <singledigit>und<tensmultiple>
-- if n%10/=0 <tensMultiple>
num2GermanWordElseCase :: Int -> [Char]
num2GermanWordElseCase n = (if (n `mod` 10) == 0
                            then ""
                            else ((num2GermanWordSub (n `mod` 10)) ++ "und"))
                           ++ (tensMultiple (n `div` 10))

-- Helper for num2GermanWord
-- Called if n>1
-- n is an int. Overflows are not a concern, since numbers need to have max 2 digits
num2GermanWordSub :: Int -> [Char]
num2GermanWordSub n | n < 10 = singleDigits n
                    | n < 20 = twoDigits (n `mod` 10)
                    | otherwise = num2GermanWordElseCase n

-- Turn integer (2 digits + potential sign) into german words
-- If n == 0 => "Null"
-- If n == 1 => "Eins"
-- If n <  0 => "minus <word>"
-- Otherwise call num2GermanWordSub
num2GermanWord :: Integer -> [Char]
num2GermanWord n | n == 0 = "Null"
                 | n < 0  = "minus " ++ num2GermanWord (abs n)
                 | n == 1 = "Eins" -- Special case because of the 's'
                 | otherwise = capitalized (num2GermanWordSub (fromIntegral n))
