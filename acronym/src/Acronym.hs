module Acronym (abbreviate) where

import           Data.Char
import           Data.List

abbreviate :: String -> String
-- abbreviate xs = filter (isUpper) upperFirst
abbreviate xs = filter (isUpper) (upperFirst xs)
    where upperFirst = concat
           . map (\(c:cs) -> toUpper c : (lowerString cs))
           . groupBy (\a b -> isSpace a == isSpace b)
          lowerString = \x -> map toLower x
