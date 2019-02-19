module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors n = checkPrimeFactors n 2

checkPrimeFactors :: Integer -> Integer -> [Integer]
checkPrimeFactors n x
    | n < 2 = []
    | n `rem` x == 0 = x : checkPrimeFactors (n `quot` x) x
    | x == 2 = checkPrimeFactors n 3
    | otherwise = checkPrimeFactors n (x + 2)
