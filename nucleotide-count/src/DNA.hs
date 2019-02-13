module DNA (nucleotideCounts, Nucleotide(..)) where

import           Data.List
import           Data.Map  (Map, findWithDefault, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Bounded)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = countAll
    where countAll = fromList
                      . map (\x -> (head x, length x))
                      . group
                      . sort
                      <$> traverse convertStrand xs
          -- final = fromList . map(\x -> (x, findWithDefault 0 x countAll)) ([Right A, Right C, Right G, Right T])

convertStrand :: Char -> Either String Nucleotide
convertStrand c = case c of 'A' -> Right A
                            'C' -> Right C
                            'G' -> Right G
                            'T' -> Right T
                            _   -> Left "Invalid strand"
