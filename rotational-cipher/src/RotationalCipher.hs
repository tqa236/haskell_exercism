module RotationalCipher (rotate) where

import           Data.Char

addAscii :: Int -> Char -> Char
addAscii n c | isUpper c = chr $ ((ord c - ord 'A' + n) `mod` 26) + ord 'A'
             | isLower c = chr $ ((ord c - ord 'a' + n) `mod` 26) + ord 'a'
             | otherwise = c

rotate :: Int -> String -> String
rotate number = map (addAscii number)
