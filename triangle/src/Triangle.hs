module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Ord a, Eq a, Num a) => a -> a -> a -> TriangleType
triangleType a b c
    | minimum [a, b, c] <= 0 = Illegal
    | a + b + c < 2 * maximum [a, b, c] = Illegal
    | all ( == True) checkEqual = Equilateral
    | all ( == False) checkEqual = Scalene
    | otherwise = Isosceles
    where checkEqual = [ a == b, a == c, b == c]
