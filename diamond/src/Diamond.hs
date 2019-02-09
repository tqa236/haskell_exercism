module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond c = Just (normalDiamond c)
    -- | c == 'A' = Just ["A"]
    -- | c == 'B' = Just [" " ++ diamond 'A' ++ " ", "B B"," " ++ diamond 'A' ++ " "]

normalDiamond :: Char -> [String]
normalDiamond c
    | c == 'A' = ["A"]
    | c == 'B' = [" " ++ (head normalDiamond 'A') ++ " ", "B B"," " ++ (head normalDiamond 'A') ++ " "]
