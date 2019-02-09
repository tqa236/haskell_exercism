module Pangram (isPangram) where

import           Data.Char
import qualified Data.Set  as Set

isPangram :: String -> Bool
isPangram text = Set.size (Set.fromList (map toLower (filter (isAlpha) text))) >= 26
