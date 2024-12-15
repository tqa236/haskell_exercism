module FoodChain (song) where

song :: String
song = unlines $ concatMap fullVerse [1..8]

fullVerse :: Int -> [String]
fullVerse n
  | n == 8    = verse n
  | otherwise = verse n ++ [""]

verse :: Int -> [String]
verse n = case n of
  1 -> ["I know an old lady who swallowed a fly.",
        "I don't know why she swallowed the fly. Perhaps she'll die."]
  2 -> ["I know an old lady who swallowed a spider.",
        "It wriggled and jiggled and tickled inside her."]
        ++ chain n
  3 -> ["I know an old lady who swallowed a bird.",
        "How absurd to swallow a bird!"]
        ++ chain n
  4 -> ["I know an old lady who swallowed a cat.",
        "Imagine that, to swallow a cat!"]
        ++ chain n
  5 -> ["I know an old lady who swallowed a dog.",
        "What a hog, to swallow a dog!"]
        ++ chain n
  6 -> ["I know an old lady who swallowed a goat.",
        "Just opened her throat and swallowed a goat!"]
        ++ chain n
  7 -> ["I know an old lady who swallowed a cow.",
        "I don't know how she swallowed a cow!"]
        ++ chain n
  8 -> ["I know an old lady who swallowed a horse.",
        "She's dead, of course!"]
  _ -> []

chain :: Int -> [String]
chain n = concatMap swallow (reverse [2..n]) ++ ["I don't know why she swallowed the fly. Perhaps she'll die."]
  where
    swallow 2 = ["She swallowed the spider to catch the fly."]
    swallow 3 = ["She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."]
    swallow 4 = ["She swallowed the cat to catch the bird."]
    swallow 5 = ["She swallowed the dog to catch the cat."]
    swallow 6 = ["She swallowed the goat to catch the dog."]
    swallow 7 = ["She swallowed the cow to catch the goat."]
    swallow _ = []
