module Acronym (abbreviate) where

import           Data.Char
import           Data.List

abbreviate :: String -> String
abbreviate xs = filter isUpper (upperFirst xs)
    where upperFirst = concatMap (\(c:cs) -> toUpper c : lowerString cs)
           . groupBy (\a b -> isSpace a == isSpace b)
          lowerString = map toLower
