{-# LANGUAGE TupleSections #-}
module DNA (nucleotideCounts, Nucleotide(..)) where

import           Data.Either.Combinators (mapBoth)
import           Data.Map                (Map, fromListWith)
import           Text.Read               (readEither)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Enum, Bounded, Read)

nucs :: [Nucleotide]
nucs = [minBound..maxBound] -- [minBound..] works too, so does [A..] or [A..T]

nucleotideCounts :: String -> Either String (Map Nucleotide Integer)
nucleotideCounts xs = fromListWith (+) . (map (, 0) nucs ++)
                        <$> mapM convertStrand xs

convertStrand :: Char -> Either String (Nucleotide, Integer)
convertStrand c = mapBoth (const "Invalid strand") (, 1) (readEither [c])
