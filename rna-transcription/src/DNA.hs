module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse convertStrand
    where convertStrand c = case c of 'C' -> Right 'G'
                                      'G' -> Right 'C'
                                      'T' -> Right 'A'
                                      'A' -> Right 'U'
                                      _   -> Left c
