{-# LANGUAGE TupleSections #-}
module Logic (
  LetterMap,
  initializeLetterMap,
  letterMapToString,
  isValidGuess,
  getLetterMapFromWords
) where

import Common.LetterState (LetterState (..), letterStateColor)
import Common.Styling (styleString)
import Common.Constants (wordleWordLength)
import Words (isValidWord)

-- | A letter map will preserve the state of each letter in the alphabet.
type LetterMap = [(Char, LetterState)]

formatCharWithState :: (Char, LetterState) -> String
formatCharWithState (ch, state) = styleString [ch] [letterStateColor state]

letterMapToString :: LetterMap -> String
letterMapToString = unwords . map formatCharWithState

-- | Initializes a letter map with all 26 letters of the alphabet and "unknown"
--   letter state for each letter.
initializeLetterMap :: LetterMap
initializeLetterMap = map (, LetterStateUnknown) ['a'..'z']

-- | Checks whether the provided word is a valid guess. A guess is valid if it
--   has the correct length and is considered by the game to be a valid word.
isValidGuess :: String -> Bool
isValidGuess guess = length guess == wordleWordLength && isValidWord guess

countLetterInLetterMap :: Char -> LetterMap -> Int
countLetterInLetterMap ch letterMap = length $ filter (\(ch', _) -> ch == ch') letterMap

countLetterInWord :: Char -> String -> Int
countLetterInWord ch word = length $ filter (== ch) word

mapInitialLetterMap :: Char -> Char -> String -> LetterState
mapInitialLetterMap a b word
  | a == b = LetterStateExactMatch
  | countLetterInWord a word > 0 = LetterStateContainsMatch
  | otherwise = LetterStateNoMatch

getLetterMapFromWords :: String -> String -> LetterMap
getLetterMapFromWords attempt actual = finalLetterMap
  where
    zippedWords = zip attempt actual
    initialLetterMap = map (\x -> (fst x, uncurry mapInitialLetterMap x actual)) zippedWords
    finalLetterMap = initialLetterMap
