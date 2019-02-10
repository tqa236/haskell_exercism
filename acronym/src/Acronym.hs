module Acronym (abbreviate) where

import           Data.Char
import           Data.List

abbreviate :: String -> String
abbreviate xs = takeFirst $ notUpperToSpace $ upperFirst $ filter (/= '\'') xs
    where upperFirst = concatMap (\(c:cs) -> toUpper c : cs)
           . groupBy (\a b -> (not . isAlpha) a == (not . isAlpha) b)
          notUpperToSpace = map (\c -> if (not . isUpper) c then ' ' else c)
          takeFirst phrase = concatMap (take 1) $ words phrase
