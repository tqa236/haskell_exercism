module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import           System.Random   (randomRIO)
import           Test.QuickCheck (Gen)

data Character = Character
  { name         :: String
  , strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

genRandomList :: Int -> IO [Int]
genRandomList n = sequence $ replicate n $ randomRIO (1,6::Int)

modifier :: Int -> Int
modifier point = (point - 10) `div` 2

ability :: Gen Int
ability = sum randomList - minimum randomList
    where randomList = genRandomList 4

character :: Gen Character
character =
  error "You need to implement this generator."
