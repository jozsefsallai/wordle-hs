module Save (SaveData) where
import Common.GameState (GameState)

-- | Contains the latest game save data (only for daily games). The goal is to
--   update this data and save whenever a valid choice is made.
data SaveData = SaveData {
  lastGameID :: Int,
  lastGameStatus :: GameState,
  lastGameGrid :: [[Int]]
} deriving (Show, Eq)
