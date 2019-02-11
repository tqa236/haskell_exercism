module WordCount (wordCount) where

import           Data.Char
import           Data.List

wordCount :: String -> [(String, Int)]
wordCount xs = map (\w -> (head w, length w)) $ group $ sort $ words xs
