module DNA (nucleotideCounts, Nucleotide(..)) where

import           Data.Either.Combinators (mapBoth)
import           Data.List
import           Data.Map                (Map, fromListWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Enum, Bounded)

nucs :: [Nucleotide]
nucs = [minBound..maxBound] -- [minBound..] works too, so does [A..] or [A..T]

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = countAll
    where countAll = fromListWith (+)
                      -- . mapBoth (\x -> x) (\x -> (head x, length x))
                      . map (\x -> (head x, length x))
                      . group
                      . sort
                      <$> traverse convertStrand xs

convertStrand :: Char -> Either String Nucleotide
convertStrand c = case c of 'A' -> Right A
                            'C' -> Right C
                            'G' -> Right G
                            'T' -> Right T
                            _   -> Left "Invalid strand"
