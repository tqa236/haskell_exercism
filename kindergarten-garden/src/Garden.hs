module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import qualified Data.Map as Map

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Garden = Map.Map String [Plant]

toPlant :: Char -> Plant
toPlant 'C' = Clover
toPlant 'G' = Grass
toPlant 'R' = Radishes
toPlant _   = Violets

garden :: [String] -> String -> Garden
garden students plants =
  let ss = concatMap (\x -> [x, x]) students
      ps = map (map toPlant) $ lines plants
      xs = concatMap (zip ss) ps
  in foldr (\(k,v) -> Map.insertWith (++) k [v]) Map.empty xs

lookupPlants :: String -> Garden -> [Plant]
lookupPlants = Map.findWithDefault []
