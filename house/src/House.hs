module House ( rhyme ) where

import           Data.List

list :: [(String, String)]
list =
  [ ("lay in"     , " the house that Jack built.\n")
  , ("ate"        , " the malt\n")
  , ("killed"     , " the rat\n")
  , ("worried"    , " the cat\n")
  , ("tossed"     , " the dog\n")
  , ("milked"     , " the cow with the crumpled horn\n")
  , ("kissed"     , " the maiden all forlorn\n")
  , ("married"    , " the man all tattered and torn\n")
  , ("woke"       , " the priest all shaven and shorn\n")
  , ("kept"       , " the rooster that crowed in the morn\n")
  , ("belonged to", " the farmer sowing his corn\n")
  , (""           , " the horse and the hound and the horn\n")
  ]

verse :: Int -> String
verse n = intercalate "that " . map (uncurry (++)) $ list'
 where (first : rest) = reverse . take n $ list
       list'          = ("This is", snd first) : rest

rhyme :: String
rhyme = intercalate "\n" . map verse $ [1 .. 12]
