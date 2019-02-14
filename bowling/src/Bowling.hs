module Bowling (score, BowlingError(..)) where

import           Data.List.Split

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = Right $ sum $ turnPoints
    where turns = chunksOf 2 rolls
          turnPoints = map (\x -> sum x) turns
