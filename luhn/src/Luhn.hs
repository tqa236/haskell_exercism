module Luhn (isValid) where

import           Data.Char

evens :: [a] -> [a]
evens (x:xs) = x:odds xs
evens _      = []

odds :: [a] -> [a]
odds (_:xs) = evens xs
odds _      = []

isValid :: String -> Bool
isValid n
    | number == "0" = False
    | n == "1" = False
    | (sum(oddList) + sum(evenList) - substraction) `mod` 10 == 0 = True
    | otherwise = False
    where number = reverse (filter isDigit n)
          digits = map (read . (:"")) (number) :: [Int]
          evenList = evens digits
          oddList = map (2 *) (odds digits)
          substraction = 9 * length( filter (> 9) oddList)
