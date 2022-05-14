{-# LANGUAGE TupleSections #-}
module Logic (
  LetterMap,
  initializeLetterMap
) where

import Common.LetterState (LetterState (..))

-- | A letter map will preserve the state of each letter in the alphabet.
type LetterMap = [(Char, LetterState)]

-- | Initializes a letter map with all 26 letters of the alphabet and "unknown"
--   letter state for each letter.
initializeLetterMap :: LetterMap
initializeLetterMap = map (, LetterStateUnknown) ['a'..'z']
