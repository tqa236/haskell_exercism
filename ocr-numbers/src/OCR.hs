module OCR (convert) where

import Data.List (intercalate, transpose)
import Data.Maybe (fromMaybe)

digitMap :: [([String], Char)]
digitMap =
  [ ([" _ ", "| |", "|_|", "   "], '0')
  , (["   ", "  |", "  |", "   "], '1')
  , ([" _ ", " _|", "|_ ", "   "], '2')
  , ([" _ ", " _|", " _|", "   "], '3')
  , (["   ", "|_|", "  |", "   "], '4')
  , ([" _ ", "|_ ", " _|", "   "], '5')
  , ([" _ ", "|_ ", "|_|", "   "], '6')
  , ([" _ ", "  |", "  |", "   "], '7')
  , ([" _ ", "|_|", "|_|", "   "], '8')
  , ([" _ ", "|_|", " _|", "   "], '9')
  ]

lookupDigit :: [String] -> Char
lookupDigit chunk = fromMaybe '?' $ lookup chunk digitMap

splitChunks :: [String] -> [[String]]
splitChunks [] = []
splitChunks xs = take 4 xs : splitChunks (drop 4 xs)

splitColumns :: [String] -> [[String]]
splitColumns = transpose . map (chunksOf 3)
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

convertGroup :: [String] -> String
convertGroup group = map lookupDigit $ splitColumns group

validateInput :: [String] -> Either String [String]
validateInput input
  | length input `mod` 4 /= 0 = Left "Invalid number of rows."
  | any (\row -> length row `mod` 3 /= 0) input = Left "Invalid number of columns."
  | otherwise = Right input

convert :: String -> String
convert input = case validateInput linesInput of
  Left err      -> error err
  Right valid   -> intercalate "," . map (convertGroup . padRows) . splitChunks $ valid
  where
    linesInput = lines input
    padRows xs = take 4 $ xs ++ repeat ""
