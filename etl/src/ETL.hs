module ETL (transform) where

import           Data.Char
import           Data.Map  (Map, fromList, toList)

transform :: Map a String -> Map Char a
transform legacyData = fromList
                        $ concatMap
                        (\(k, vs) -> [(v, k) | v <- map toLower vs])
                        (toList legacyData)
