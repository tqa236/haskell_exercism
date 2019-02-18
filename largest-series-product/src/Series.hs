module Series (Error(..), largestProduct) where

import           Data.Char (digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
    | size > length digits || size < 0 = Left InvalidSpan
    | all (`elem` ['0'..'9']) digits = Right $ toInteger $ maximum $ map product series
    | otherwise = Left (InvalidDigit x)
    where series = slices size digits
          x = head $ filter (not . (`elem` ['0'..'9'])) digits

slices :: Int -> String -> [[Int]]
slices n xs = fmap (\i -> (fmap digitToInt. take n. drop i) xs) [0..length xs - n]
