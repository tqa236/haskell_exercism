module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import           Data.List       (sort)
import           Test.QuickCheck (Gen, arbitrary, choose)

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

modifier :: Int -> Int
modifier point = (point - 10) `div` 2


ability :: Gen Int
ability = do
    first <- choose (1, 6)
    second <- choose (1, 6)
    third <- choose (1, 6)
    forth <- choose (1, 6)
    return $ sum $ tail $ sort [first, second, third, forth]

character :: Gen Character
character = do
    nm <- arbitrary
    st <- ability
    dt <- ability
    co <- ability
    it <- ability
    wm <- ability
    ch <- ability
    return (Character nm st dt co it wm ch (modifier co + 10))
