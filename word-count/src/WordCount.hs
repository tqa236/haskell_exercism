module WordCount (wordCount) where

import           Data.Char
import           Data.List
import           Data.Text (pack, replace, unpack)

wordCount :: String -> [(String, Int)]
wordCount xs = map (\w -> (head w, length w)) $ group $ sort $ words noQuotation
    where noQuotation =  unpack . replace "\' " " " . pack
          sentence = map (\c -> if not (isAlphaNum c || (c == '\'')) then ' ' else toLower c) xs
