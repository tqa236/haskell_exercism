module School (School, add, empty, grade, sorted) where

import           Data.Bifunctor
import           Data.List       (sort)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)

type School = Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student = Map.insertWith (++) gradeNum [student]

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
grade gradeNum school = sort $ fromMaybe [] $ Map.lookup gradeNum school

sorted :: School -> [(Int, [String])]
sorted = map (Data.Bifunctor.second sort) . Map.toAscList
