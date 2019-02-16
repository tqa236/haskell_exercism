module School (School, add, empty, grade, sorted) where

import           Data.List
import           Data.Map  (Map)
import qualified Data.Map  as Map

type School = Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student = Map.fromList [gradeNum, [student]]

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
grade gradeNum school = error "You need to implement this function."

sorted :: School -> [(Int, [String])]
sorted = Map.toList
