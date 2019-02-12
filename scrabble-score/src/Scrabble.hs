module Scrabble (scoreLetter, scoreWord) where

import           Data.Char
import           Data.Map  (fromList, (!))

scoreLetter :: Char -> Integer
scoreLetter letter
    | isAlpha letter = fromList letterValue ! toLower letter
    | otherwise = 0
    where letterValue = [('a', 1), ('e', 1), ('i', 1), ('o', 1), ('u', 1),
                         ('l', 1), ('n', 1), ('r', 1), ('s', 1), ('t', 1),
                         ('d', 2), ('g', 2),
                         ('b', 3), ('c', 3), ('m', 3), ('p', 3),
                         ('f', 4), ('h', 4), ('v', 4), ('w', 4), ('y', 4),
                         ('k', 5),
                         ('j', 8), ('x', 8),
                         ('q', 10), ('z', 10)]

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
