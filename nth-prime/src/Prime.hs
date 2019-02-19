module Prime (nth) where

isqrt :: Int -> Int
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

isPrime :: Int -> Bool
isPrime n = notElem True $  map (\x -> n `mod` x == 0) [2..isqrt n]

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just $ fromIntegral $ last $ take n $ filter isPrime [2..]
