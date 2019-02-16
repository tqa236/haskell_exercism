module Raindrops (convert) where

import           Data.Map (fromList, (!))

convert :: Int -> String
convert n | null string = show n
          | otherwise   = string
    where string                  = concatMap (rain n) [3, 5, 7]
          rain x y | mod x y == 0 = raindrops ! y
                   | otherwise    = ""
          raindrops = fromList [(3, "Pling"), (5, "Plang"), (7, "Plong")]
