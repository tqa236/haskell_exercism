module ResistorColors (Color(..), value) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Read, Enum, Bounded)

value :: (Color, Color) -> Int
value (color1, color2) = 10 * fromEnum color1 + fromEnum color2
