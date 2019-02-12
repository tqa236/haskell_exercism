module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond c = Just (normalDiamond c)

normalDiamond :: Char -> [String]
normalDiamond c
    | c == 'A' = ["A"]
    | c == 'B' = [" " ++ head (normalDiamond 'A') ++ " ", "B B"," " ++ head (normalDiamond 'A') ++ " "]
    | c == 'C' = map (\x -> " " ++ x ++ " ")( take 2 (normalDiamond 'B')) ++ ["C   C"] ++ map (\x -> " " ++ x ++ " ")(reverse (take 2 (normalDiamond 'B')))
