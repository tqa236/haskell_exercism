module School (School, add, empty, grade, sorted) where

data School = Dummy

add :: Int -> String -> School -> School
add gradeNum student school = [(gradeNum, [student])]

empty :: School
empty = []

grade :: Int -> School -> [String]
grade gradeNum school = error "You need to implement this function."

sorted :: School -> [(Int, [String])]
sorted school = error "You need to implement this function."
