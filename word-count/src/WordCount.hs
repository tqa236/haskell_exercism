module WordCount (wordCount) where

import           Data.Char
import           Data.List
import           Data.List.Utils (replace)

wordCount :: String -> [(String, Int)]
wordCount xs = map (\w -> (head w, length w)) $ group $ sort $ words noQuotation
    where sentence = map (\c -> if not (isAlphaNum c || (c == '\'')) then ' ' else toLower c) xs
          noQuotation = replace " \'" " " (replace "\' " " " sentence)
