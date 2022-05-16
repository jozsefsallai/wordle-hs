module Game (GameType (..), startGame) where

import Data.Time (getCurrentTime)
import System.Random (randomRIO)
import Common.GameState (GameState (..), getFinalMessage)
import Words (getOfficialWordOfDay, wordleWords, getWordWithIndex)
import Utils (prompt, lowercase)
import Common.Constants (wordleMaxGuesses)
import Logic (isValidGuess, LetterMap, initializeLetterMap, getLetterMapFromWords, letterMapToString, convertAttemptsToShareString)
import Common.Styling (styleString, red, bold)

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
  currentIndex :: Int,
  alphabet :: LetterMap,
  guesses :: [LetterMap]
}

-- Internal members

printHelp :: LetterMap -> [LetterMap] -> IO ()
printHelp alphabet guesses = do
  putStrLn $ letterMapToString alphabet

  if not (null guesses) then do
    putStrLn ""
    mapM_ (putStrLn . letterMapToString) guesses
    putStrLn ""
  else
    putStrLn ""

printShareString :: [LetterMap] -> Int -> GameState -> IO ()
printShareString guesses attempts gameState = do
  let attemptsString = if gameState == GameStateLost then "X" else show attempts
  putStrLn $ "Wordle " ++ attemptsString ++ "/6"
  putStrLn ""
  putStrLn $ convertAttemptsToShareString guesses

-- | Prints out the final message.
--   TODO: handle saving.
handleGameEnd :: Game -> IO ()
handleGameEnd (Game _ _ gameState word currentIndex alphabet guesses) = do
  printHelp alphabet guesses
  putStrLn $ getFinalMessage gameState currentIndex word
  putStrLn ""
  printShareString guesses currentIndex gameState

-- | The main game loop which will be run until the game is over or the user
--   guesses the word.
gameLoop :: Game -> IO ()
gameLoop (Game gameId gameType GameStateRunning word currentIndex alphabet guesses) = do
  if currentIndex == wordleMaxGuesses then do
    gameLoop $ Game gameId gameType GameStateLost word currentIndex alphabet guesses
  else do
    printHelp alphabet guesses

    let newIndex = currentIndex + 1
    rawGuess <- prompt ("Guess #" ++ show newIndex ++ ": ")
    putStrLn ""
    let guess = lowercase rawGuess

    if isValidGuess guess then do
      let newGuesses = guesses ++ [getLetterMapFromWords guess word]
      if guess == word then gameLoop (Game gameId gameType GameStateWon word newIndex alphabet newGuesses)
      else gameLoop (Game gameId gameType GameStateRunning word newIndex alphabet newGuesses)
    else do
      putStrLn $ styleString "Invalid word length or unknown word. Please try again." [red, bold]
      putStrLn ""
      gameLoop (Game gameId gameType GameStateRunning word currentIndex alphabet guesses)
gameLoop (Game gameId gameType gameState word 6 alphabet guesses) =
  handleGameEnd (Game gameId gameType gameState word 6 alphabet guesses)
gameLoop (Game gameId gameType gameState word currentIndex alphabet guesses) =
  handleGameEnd (Game gameId gameType gameState word currentIndex alphabet guesses)

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
    currentIndex = 0,
    alphabet = initializeLetterMap,
    guesses = []
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
    currentIndex = 0,
    alphabet = initializeLetterMap,
    guesses = []
  }

  gameLoop game

-- Public members

-- | Starts a new game based on a given type. We receive the game type from the
--   application's bootstrap function.
startGame :: GameType -> IO ()
startGame GameTypeDaily = startDailyGame
startGame GameTypeRandom = startRandomGame
