module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, insertWith, fromList)
import Text.Read (readEither)
import Control.Monad (foldM)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

type NucleotideCounts = Map Nucleotide Int

countDNA :: NucleotideCounts -> Char -> Either String NucleotideCounts

countDNA m x =
  readEither [x] >>=
    \n -> return $ insertWith (+) n 1 m

nucleotideCounts :: String -> Either String NucleotideCounts
nucleotideCounts =
  foldM countDNA (fromList [(A, 0), (C, 0), (G, 0), (T, 0)])