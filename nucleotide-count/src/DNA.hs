{-# LANGUAGE TupleSections #-}
module DNA (nucleotideCounts, Nucleotide(..)) where

import           Data.Either.Combinators (mapBoth)
import           Data.Map                (Map, fromListWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Enum, Bounded)

nucs :: [Nucleotide]
nucs = [minBound..maxBound] -- [minBound..] works too, so does [A..] or [A..T]

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = fromListWith (+) <$> countAll
    where countAll = (map (, 0) nucs ++) <$> mapM convertStrand xs

convertStrand :: Char -> Either String (Nucleotide, Int)
convertStrand c = mapBoth (const "Invalid strand") (, 1) (readEither c)

readEither :: Char -> Either String Nucleotide
readEither c = case c of 'A' -> Right A
                         'C' -> Right C
                         'G' -> Right G
                         'T' -> Right T
                         _   -> Left "Invalid strand"
