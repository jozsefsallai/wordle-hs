module Common.LetterState (
  LetterState (..),
  letterStateColor,
  letterStateEmoji
) where

import Common.Styling (StyleBlock, white, gray, yellow, green)

-- | The state of a letter specifies the letter's relationship with the given
--   word. A letter can either be unchecked, in the word but on the wrong,
--   position, in the word and in the correct position, or not in the word at
--   all.
data LetterState =
  LetterStateUnknown |
  LetterStateExactMatch |
  LetterStateContainsMatch |
  LetterStateNoMatch
  deriving (Eq, Enum)

-- | Specifies what color should the letter be displayed as when printing it to
--   the terminal, based on its state.
letterStateColor :: LetterState -> StyleBlock
letterStateColor LetterStateUnknown = white
letterStateColor LetterStateExactMatch = green
letterStateColor LetterStateContainsMatch = yellow
letterStateColor LetterStateNoMatch = gray

-- | Specifies what emoji will be displayed in the share text in place of the
--   letter.
letterStateEmoji :: LetterState -> String
letterStateEmoji LetterStateUnknown = ""
letterStateEmoji LetterStateExactMatch = "🟩"
letterStateEmoji LetterStateContainsMatch = "🟨"
letterStateEmoji LetterStateNoMatch = "🔳"
