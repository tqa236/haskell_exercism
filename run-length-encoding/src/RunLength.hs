module RunLength (decode, encode) where

import           Data.Char


decode :: String -> String
decode encodedText = decodeNumber encodedText 0

decodeNumber :: String -> Int -> String
decodeNumber "" _ = ""
decodeNumber (x:xs) n
    | n == 0 && not (isDigit x) = x : decodeNumber xs 0
    | not (isDigit x) = concat (replicate n [x]) ++ decodeNumber xs 0
    | otherwise = decodeNumber xs (10 * n + digit)
    where digit = read [x] :: Int

encode :: String -> String
encode ""   = ""
encode text = encodeNumber (tail text) (head text) 1

encodeNumber :: String -> Char -> Int -> String
encodeNumber "" c n
    | n > 1 = show n ++ [c]
    | otherwise = [c]
encodeNumber (x:xs) c n
    | x == c = encodeNumber xs c (n+1)
    | n > 1 = show n ++ [c] ++ encodeNumber xs x 1
    | otherwise = c : encodeNumber xs x 1
