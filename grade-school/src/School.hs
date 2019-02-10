module School (School, add, empty, grade, sorted) where

import           Data.Map (Map)
import qualified Data.Map as Map

type School = [(String, Int)]

add :: Int -> String -> School -> School
add gradeNum student = Map.insert gradeNum [student]

empty :: School
empty = empty

grade :: Int -> School -> [String]
grade gradeNum school = error "You need to implement this function."

sorted :: School -> [(Int, [String])]
sorted school = error "You need to implement this function."
