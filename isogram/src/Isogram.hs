module Isogram (isIsogram) where

import           Data.Char
-- import           Data.Set

isIsogram :: String -> Bool
isIsogram phrase = length (alpha) == length (unique alpha)
    where alpha = filter (isAlpha) phrase
