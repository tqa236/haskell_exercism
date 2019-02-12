module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter divided numberList
    where numberList = [1..limit-1]
          nonZeroFactors = filter (> 0) factors
          divided number = any (\x -> number `rem` x == 0) nonZeroFactors
