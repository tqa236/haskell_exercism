module Diamond (diamond) where

import           Data.Char

diamond :: Char -> Maybe [String]
diamond c = Just (normalDiamond c)

normalDiamond :: Char -> [String]
normalDiamond c
    | distance == 0 = ["A"]
    | distance == 1 = [" " ++ head (normalDiamond 'A') ++ " ", "B B"," " ++ head (normalDiamond 'A') ++ " "]
    -- | c == 'C' = map (\x -> " " ++ x ++ " ")( take 2 (normalDiamond 'B')) ++ ["C   C"] ++ map (\x -> " " ++ x ++ " ")(reverse (take 2 (normalDiamond 'B')))
    where distance = ord c - ord 'A'
          -- pad =
