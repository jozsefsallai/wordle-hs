{-# LANGUAGE TemplateHaskell #-}
module Words (wordleWords, validWords, isValidWord, getOfficialWordOfDay, getWordWithIndex) where

import Data.Char (isLetter)
import Data.FileEmbed (embedFile)
import Data.List.Split (wordsBy)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import qualified Data.ByteString.Char8 as B

--- Internal members

-- | A raw, embedded bytestring which contains a list of Wordle words (that can
--   be generated by the app). It is separated by spaces and newlines.
wordleWordsRaw :: B.ByteString
wordleWordsRaw = $(embedFile "assets/words.txt")

-- | A raw, embedded bytestring which contains a list of all valid words that
--   can be entered. It is separated by spaces and newlines.
validWordsRaw :: B.ByteString
validWordsRaw = $(embedFile "assets/words_valid.txt")

-- | Creates an array of strings from a raw bytestring, which is separated by
--   spaces and newlines.
extractWords :: B.ByteString -> [String]
extractWords = wordsBy (not . isLetter) . B.unpack

-- | Will return the word of the day based on a given epoch, relative to the
--   current system time.
getWordOfTheDay :: [String] -> UTCTime -> UTCTime -> (String, Int)
getWordOfTheDay words now epoch = (words !! idx, idx)
  where
    diff = diffUTCTime now epoch
    days = floor diff `div` 3600 `div` 24
    idx = days `mod` length words

--- Public members

-- | A generated static list of official Wordle words.
wordleWords :: [String]
wordleWords = extractWords wordleWordsRaw

-- | A generated static list of valid words.
validWords :: [String]
validWords = extractWords validWordsRaw

-- | Checks whether the list of Wordle words or the list of valid words contains
--   the given word.
isValidWord :: String -> Bool
isValidWord word = word `elem` validWords || word `elem` wordleWords

-- | Returns the official Wordle (epoch is 2021-06-19).
getOfficialWordOfDay :: UTCTime -> (String, Int)
getOfficialWordOfDay now = getWordOfTheDay wordleWords now (read "2021-06-19 00:00:00 UTC")

-- | Returns a Wordle from a given index.
getWordWithIndex :: Int -> (String, Int)
getWordWithIndex idx = (wordleWords !! idx, idx)
