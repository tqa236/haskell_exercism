module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter divided numberList
    where numberList = take (fromIntegral limit - 1) [1,2..]
          nonZeroFactors = filter (> 0) factors
          divided number = any (\x -> number `mod` x == 0) nonZeroFactors
