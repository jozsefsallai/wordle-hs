{-# LANGUAGE TupleSections #-}
module Logic (
  LetterMap,
  initializeLetterMap,
  letterMapToString,
  isValidGuess,
  getLetterMapFromWords,
  generateNewAlphabet,
  convertAttemptsToShareString
) where

import Common.LetterState (LetterState (..), letterStateColor, letterStateEmoji)
import Common.Styling (styleString)
import Common.Constants (wordleWordLength)
import Words (isValidWord)
import Data.List (find, intercalate)

-- | A letter map will preserve the state of each letter in the alphabet.
type LetterMap = [(Char, LetterState)]

-- | Styles a character based on its letter state.
formatCharWithState :: (Char, LetterState) -> String
formatCharWithState (ch, state) = styleString [ch] [letterStateColor state]

-- | Converts a letter map to a styled string.
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

-- | Counts how many times a letter can be found in the current letter map.
countLetterInLetterMap :: Char -> LetterMap -> Int
countLetterInLetterMap ch letterMap = length $ filter (\(ch', _) -> ch == ch') letterMap

-- | Counts how many times a letter can be found in a word.
countLetterInWord :: Char -> String -> Int
countLetterInWord ch word = length $ filter (== ch) word

-- | Creates the initial version of the letter map before post-processing.
mapInitialLetterMap :: Char -> Char -> String -> LetterState
mapInitialLetterMap a b word
  | a == b = LetterStateExactMatch
  | countLetterInWord a word > 0 = LetterStateContainsMatch
  | otherwise = LetterStateNoMatch

-- | Cleans up the initial letter map so that there are no duplicate letter
--   matches.
cleanupLetterMap :: String -> LetterMap -> LetterMap
cleanupLetterMap word = foldl cleanup []
  where
    cleanup :: LetterMap -> (Char, LetterState) -> LetterMap
    cleanup letterMap (ch, state) =
      if countLetterInLetterMap ch letterMap < countLetterInWord ch word
        then (ch, state) : letterMap
        else (ch, LetterStateNoMatch) : letterMap

-- | Checks the relation of the letters in two words and creates a letter map
--   out of them.
getLetterMapFromWords :: String -> String -> LetterMap
getLetterMapFromWords attempt actual = finalLetterMap
  where
    zippedWords = zip attempt actual
    initialLetterMap = map (\x -> (fst x, uncurry mapInitialLetterMap x actual)) zippedWords
    finalLetterMap = reverse $ cleanupLetterMap actual initialLetterMap

-- | Generates the new alphabet based on the results of the last guess.
generateNewAlphabet :: LetterMap -> LetterMap -> LetterMap
generateNewAlphabet oldAlphabet lastGuessMap = map (\x -> (fst x, findElementInGuessMap x)) oldAlphabet
  where
    findElementInGuessMap :: (Char, LetterState) -> LetterState
    findElementInGuessMap (ch, oldState) = newLetterState
      where
        lastState = find (\x -> fst x == ch) lastGuessMap
        newLetterState = case lastState of
          Just (_, state) -> state
          Nothing -> oldState

-- | Converts a single attempt to a share string that consists of emojis.
convertAttemptToShareString :: LetterMap -> [String]
convertAttemptToShareString = map (letterStateEmoji . snd)

-- | Converts all atttempts to a share string that consists of emojis.
convertAttemptsToShareString :: [LetterMap] -> String
convertAttemptsToShareString attempts = unlines $ map (intercalate "" . convertAttemptToShareString) attempts
