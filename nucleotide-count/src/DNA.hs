module DNA (nucleotideCounts, Nucleotide(..)) where


import           Data.Map (Map, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
    | all (`elem` ['A', 'C', 'G', 'T']) xs = Right $ fromList [(A, numA), (C, numC), (G, numG), (T, numT)]
    | otherwise = Left xs
    where numA = length $ filter (== 'A') xs
          numC = length $ filter (== 'C') xs
          numG = length $ filter (== 'G') xs
          numT = length $ filter (== 'T') xs
