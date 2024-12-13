module Roman (numerals) where



numerals :: Integer -> Maybe String
numerals n
  | n <= 0 || n >= 4000 = Nothing
  | otherwise = Just (numerals' n romanValues)
  where
    romanValues =
      [ (1000, "M"), (900, "CM"), (500, "D"), (400, "CD")
      , (100, "C"), (90, "XC"), (50, "L"), (40, "XL")
      , (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")
      ]
numerals' :: Integer -> [(Integer, String)] -> String
numerals' _ [] = ""
numerals' n (x:xs)
    | n >=  fst x = snd x ++ numerals' (n - fst x) (x:xs)
    | otherwise = numerals' n xs
