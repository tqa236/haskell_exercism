module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

classify :: Int -> Maybe Classification
classify number
    | number <= 0 = Nothing
    | aliquotSum == 2 * number = Just Perfect
    | aliquotSum > 2 * number = Just Abundant
    | otherwise = Just Deficient
    where squareRoot = isqrt number
          divisor = filter (\x -> number `mod` x == 0) [1..squareRoot]
          allDivisor = map (\x -> number `div` x) divisor
          aliquotSum
            | isqrt number ^2 == number = sumDivisors - isqrt number
            | otherwise = sumDivisors
            where sumDivisors = sum divisor + sum allDivisor
