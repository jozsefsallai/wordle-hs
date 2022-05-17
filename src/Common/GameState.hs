{-# LANGUAGE DeriveGeneric #-}

module Common.GameState (
  GameState (..),
  getFinalMessage
) where
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | Specifies the state of the game (running, won, lost).
data GameState =
  GameStateRunning |
  GameStateWon |
  GameStateLost
  deriving (Eq, Show, Generic)

instance FromJSON GameState
instance ToJSON GameState

-- | Change the world... The message that will be displayed at the end of a game
--   depending on the game's state and the number of attempts.
getFinalMessage :: GameState -> Int -> String -> String
getFinalMessage GameStateRunning _ _ = ""
getFinalMessage GameStateLost _ word = "You lost! 😔 The word was: " ++ word ++ "."
getFinalMessage GameStateWon 1 _ = "Genius! 😱"
getFinalMessage GameStateWon 2 _ = "Magnificent! 😲"
getFinalMessage GameStateWon 3 _ = "Impressive! 🤩"
getFinalMessage GameStateWon 4 _ = "Splendid! 👏"
getFinalMessage GameStateWon 5 _ = "Great! 😊"
getFinalMessage GameStateWon 6 _ = "Phew! 🎉"
getFinalMessage GameStateWon _ _ = "You won! 🎉"
