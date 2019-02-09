module Anagram (anagramsFor) where

import           Data.Char
import           Data.List

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (\x -> (sort (lowerString x) == sort(lowerString xs)) && (lowerString x /= lowerString xs))
    where lowerString = map toLower
