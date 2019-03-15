module PigLatin (translate) where

import           Data.List
import           Data.Maybe (fromJust)

translate :: String -> String
translate xs = unwords $ map translateOneWord (words xs)

translateOneWord :: String -> String
translateOneWord xs = findPrefix xs ++ "ay"

findPrefix :: String -> String
findPrefix xs
    | consonantLength == 0 && checkQu = xs
    | checkQu = drop 3 xs ++ take 3 xs
    | any (`isPrefixOf` xs) ["yt", "xr"] = xs
    | "qu" `isPrefixOf` xs = drop 2 xs ++ take 2 xs
    | "y" `isPrefixOf` xs = tail xs ++ [head xs]
    | otherwise = drop consonantLength xs ++ take consonantLength xs
    where checkQu = "qu" `isPrefixOf` drop 1 xs
          consonantLength = fromJust $ findIndex (`elem` "aeiouy") xs
