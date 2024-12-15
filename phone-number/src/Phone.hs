module Phone (number) where

import           Data.Char

number :: String -> Maybe String
number xs
    | length digits < 10 || length digits > 12 = Nothing
    | areaCode == '0' || areaCode == '1' = Nothing
    | exchangeCode == '0' || exchangeCode == '1' = Nothing
    | length digits == 10 =  Just (digits)
    | head digits /= '1' = Nothing
    | otherwise = Just ( tail digits)
    where digits = filter (isDigit) xs
          areaCode = head (drop 9 (reverse digits))
          exchangeCode = head (drop 6 (reverse digits))
