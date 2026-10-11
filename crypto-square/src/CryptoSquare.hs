module CryptoSquare (encode) where

import           Data.Char (isAlphaNum, toLower)
import           Data.List (transpose)
import           Data.List.Split (chunksOf)

encode :: String -> String
encode xs
    | null plainText = ""
    | otherwise = unwords $ transpose $ chunksOf columns paddedText
  where
    plainText = map toLower $ filter isAlphaNum xs
    columns = ceiling $ sqrt (fromIntegral (length plainText) :: Double)
    rows = (length plainText + columns - 1) `div` columns
    paddedText = plainText ++ replicate (rows * columns - length plainText) ' '
