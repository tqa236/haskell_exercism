module ProteinTranslation(proteins) where

proteins :: String -> Maybe [String]
proteins rna
    | take 3 rna == "AUG" = Just $ "Methionine" : proteins (drop 3 rna)
    | otherwise = Nothing
