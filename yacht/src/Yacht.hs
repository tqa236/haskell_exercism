module Yacht (yacht, Category(..)) where

import Data.List (group, nub, sort)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht category dice =
  case category of
    Ones    -> countOf 1
    Twos    -> countOf 2
    Threes  -> countOf 3
    Fours   -> countOf 4
    Fives   -> countOf 5
    Sixes   -> countOf 6
    FullHouse -> if isFullHouse then sum dice else 0
    FourOfAKind -> if isFourOfAKind then 4 * mostFrequent else 0
    LittleStraight -> if isLittleStraight then 30 else 0
    BigStraight -> if isBigStraight then 30 else 0
    Choice -> sum dice
    Yacht -> if isYacht then 50 else 0
  where
    countOf n = n * length (filter (== n) dice)
    isFullHouse = (length unique == 2) && (any (== 3) counts) && (any (== 2) counts)
    isFourOfAKind = any (== 4) counts || any (== 5) counts
    isLittleStraight = sort dice == [1, 2, 3, 4, 5]
    isBigStraight = sort dice == [2, 3, 4, 5, 6]
    isYacht = all (== head dice) dice
    counts = map length . group . sort $ dice
    unique = nub dice
    mostFrequent = head . head . filter ((== maximum counts) . length) . group . sort $ dice
