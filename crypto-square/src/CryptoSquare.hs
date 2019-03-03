module CryptoSquare (encode) where

import           Data.Char
import           Data.List
import           Data.List.Split (chunksOf)

encode :: String -> String
encode xs = unwords $ transpose $ chunksOf numChunks plainText
    where numChunks = isqrt $ length xs
          plainText = map toLower $ filter isAlphaNum xs


isqrt :: Int -> Int
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral
