module Isogram (isIsogram) where

import           Data.Char
import qualified Data.Set  as Set

isIsogram :: String -> Bool
isIsogram phrase = length lowerString == Set.size (Set.fromList lowerString)
    where lowerString = map toLower (filter isAlpha phrase)
