module Game (GameType (..), startGame) where

import Data.Time (getCurrentTime)
import System.Random (randomRIO)
import Common.GameState (GameState (..), getFinalMessage)
import Words (getOfficialWordOfDay, wordleWords, getWordWithIndex, isValidWord)
import Utils (prompt, lowercase)
import Common.Constants (wordleMaxGuesses, wordleWordLength)

-- Data types

-- | Specifies the type of the game. If `GameTypeDaily`, then a daily word will
--   be used, generated based on the Wordle epoch. This mode can only be played
--   once per day. If `GameTypeRandom`, then a random word will be used. This
--   mode can be played multiple times per day.
data GameType = GameTypeDaily | GameTypeRandom deriving (Eq, Enum)

-- | A data structure containing information about the active game.
data Game = Game {
  gameId :: Int,
  gameType :: GameType,
  gameState :: GameState,
  word :: String,
  currentIndex :: Int
}

-- Internal members

-- | Prints out the final message.
--   TODO: handle saving.
handleGameEnd :: Game -> IO ()
handleGameEnd (Game _ _ gameState word currentIndex) = putStrLn $ getFinalMessage gameState currentIndex word

-- | Checks whether the provided word is a valid guess. A guess is valid if it
--   has the correct length and is considered by the game to be a valid word.
isValidGuess :: String -> Bool
isValidGuess guess = length guess == wordleWordLength && isValidWord guess

-- | The main game loop which will be run until the game is over or the user
--   guesses the word.
gameLoop :: Game -> IO ()
gameLoop (Game gameId gameType GameStateRunning word currentIndex) = do
  if currentIndex == wordleMaxGuesses then do
    gameLoop $ Game gameId gameType GameStateLost word currentIndex
  else do
    let newIndex = currentIndex + 1
    rawGuess <- prompt ("Guess #" ++ show newIndex ++ ": ")
    let guess = lowercase rawGuess

    if isValidGuess guess then do
      if guess == word then gameLoop (Game gameId gameType GameStateWon word newIndex)
      else gameLoop (Game gameId gameType GameStateRunning word newIndex)
    else do
      putStrLn "Invalid word length or unknown word. Please try again."
      gameLoop (Game gameId gameType GameStateRunning word currentIndex)
gameLoop (Game gameId gameType gameState word 6) =
  handleGameEnd (Game gameId gameType gameState word 6)
gameLoop (Game gameId gameType gameState word currentIndex) =
  handleGameEnd (Game gameId gameType gameState word currentIndex)

-- | Starts a new game in "daily" mode.
startDailyGame :: IO ()
startDailyGame = do
  now <- getCurrentTime
  let (word, idx) = getOfficialWordOfDay now

  let game = Game {
    gameId = idx,
    gameType = GameTypeDaily,
    gameState = GameStateRunning,
    word = word,
    currentIndex = 0
  }

  gameLoop game

-- | Starts a new game in "random" mode.
startRandomGame :: IO ()
startRandomGame = do
  rng <- randomRIO (0, length wordleWords - 1)
  let (word, idx) = getWordWithIndex rng

  let game = Game {
    gameId = idx,
    gameType = GameTypeRandom,
    gameState = GameStateRunning,
    word = word,
    currentIndex = 0
  }

  gameLoop game

-- Public members

-- | Starts a new game based on a given type. We receive the game type from the
--   application's bootstrap function.
startGame :: GameType -> IO ()
startGame GameTypeDaily = startDailyGame
startGame GameTypeRandom = startRandomGame
