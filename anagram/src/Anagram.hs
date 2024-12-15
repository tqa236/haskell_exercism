module Anagram (anagramsFor) where

import           Data.Char
import           Data.List

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (\x -> (sort (lowerString x) == sort(lowerString xs)) && (lowerString x /= lowerString xs)) xss
    where lowerString = map toLower
