{-# LANGUAGE DeriveGeneric #-}

module Save (SaveData (..), saveGame, loadSave) where

import Data.Aeson (encodeFile, FromJSON, ToJSON, decodeFileStrict, eitherDecodeFileStrict)
import Common.GameState (GameState)
import Logic (LetterMap)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

profileFileName :: String
profileFileName = ".wordle-profile.json"

-- | Contains the latest game save data (only for daily games). The goal is to
--   update this data and save whenever a valid choice is made.
data SaveData = SaveData {
  gameId :: Int,
  gameState :: GameState,
  currentIndex :: Int,
  alphabet :: LetterMap,
  guesses :: [LetterMap]
} deriving (Eq, Generic)

instance FromJSON SaveData
instance ToJSON SaveData

-- | Saves the current state of the game to a JSON file.
saveGame :: Int -> GameState -> Int -> LetterMap -> [LetterMap] -> IO ()
saveGame gameId gameState currentIndex alphabet guesses = do
  let saveData = SaveData {
    gameId = gameId,
    gameState = gameState,
    currentIndex = currentIndex,
    alphabet = alphabet,
    guesses = guesses
  }

  encodeFile profileFileName saveData

-- | Loads the latest JSON save file if it exists.
loadSave :: IO (Maybe SaveData)
loadSave = do
  saveExists <- doesFileExist profileFileName
  if not saveExists then
    return Nothing
  else do
    contents <- eitherDecodeFileStrict profileFileName
    return $ case contents of
      Left _ -> Nothing
      Right saveData -> Just saveData
