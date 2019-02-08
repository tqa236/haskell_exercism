module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond 'A' = Just ["A"]
diamond 'B' = Just [" " ++ diamond 'A' ++ " ", "B B"," " ++ diamond 'A' ++ " "]
