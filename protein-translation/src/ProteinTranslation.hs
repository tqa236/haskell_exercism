module ProteinTranslation(proteins) where

import           Data.List.Split

proteins :: String -> Maybe [String]
proteins rna = traverse toProtein codons
    where toProtein codon
            | codon == "AUG" = Just "Methionine"
            | codon `elem` ["UUU", "UUC"] = Just "Phenylalanine"
            | codon `elem` ["UUA", "UUG"] = Just "Leucine"
            | codon `elem` ["UCU", "UCC", "UCA", "UCG"] = Just "Serine"
            | codon `elem` ["UAU", "UAC"] = Just "Tyrosine"
            | codon `elem` ["UGU", "UGC"] = Just "Cysteine"
            | codon == "UGG" = Just "Tryptophan"
            | otherwise = Nothing
          codons = head
                    $ splitWhen (`elem` ["UAA", "UAG", "UGA"])
                    $ chunksOf 3 rna
