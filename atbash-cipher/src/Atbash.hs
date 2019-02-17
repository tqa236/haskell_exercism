module Atbash (decode, encode) where

import           Data.Char
import           Data.List.Split (chunksOf)
import           Data.Map        (findWithDefault, fromList)

decode :: String -> String
decode cipherText = map (\x -> findWithDefault x (toLower x) values) alnum
    where values = fromList (zip ['a'..'z'] ['z','y'..'a'])
          alnum = filter isAlphaNum cipherText

encode :: String -> String
encode plainText = unwords $ chunksOf 5 $ decode plainText
