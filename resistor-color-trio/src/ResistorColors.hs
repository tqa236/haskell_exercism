module ResistorColors (Color(..), Resistor(..), label, ohms) where

data Color =
    Black | Brown | Red | Orange | Yellow | Green | Blue | Violet | Grey | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

colorToDigit :: Color -> Int
colorToDigit color = fromEnum color

ohms :: Resistor -> Int
ohms (Resistor (color1, color2, color3)) = 
  let base = colorToDigit color1 * 10 + colorToDigit color2
      multiplier = colorToDigit color3
  in base * (10 ^ multiplier)

label :: Resistor -> String
label resistor@(Resistor (_, _, _)) = 
  let resistance = ohms resistor
      (prefix, divisor) = 
        case resistance of
          r | r >= 10^9 -> ("gigaohms", 10^9)
          r | r >= 10^6 -> ("megaohms", 10^6)
          r | r >= 10^3 -> ("kiloohms", 10^3)
          _             -> ("ohms", 1)
      formattedResistance = fromIntegral (resistance `div` divisor)
  in show formattedResistance ++ " " ++ prefix
