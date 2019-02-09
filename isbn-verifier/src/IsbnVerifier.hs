module IsbnVerifier (isbn) where

import           Data.Char

digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

isbnCheck :: [Int] -> Bool
isbnCheck digits = (sum (zipWith (*) digits [start..10]) + 10 * (start - 1)) `mod` 11 == 0
    where start =  11 - length digits

isbn :: String -> Bool
isbn number
    | length number < 10 = False
    | length digits < 9 || length digits > 10 = False
    | length digits == 10 = isbnCheck digits
    | character /= "X" = False
    | otherwise = isbnCheck digits
    where digits = digs (read (filter isDigit number) :: Int)
          character = filter isAlpha number
