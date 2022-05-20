module Lib ( runApp ) where

import Words (getOfficialWordOfDay)
import Data.Time (getCurrentTime)
import Game (startGame, GameType (..))
import Common.Styling (styleString, green, yellow, gray, bold, errorString)
import Control.Monad (forM_)
import Utils (prompt)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Logic (initializeLetterMap, letterMapToString)
import Common.LetterState (LetterState (..))

-- | Wrapper function to display the application's logo with some quirky
--   formatting.
displayLogo :: IO ()
displayLogo = do
  let w = styleString "W" [gray]
  let o = styleString "O" [green]
  let r = styleString "R" [yellow]
  let d = styleString "D" [gray]
  let l = styleString "L" [yellow]
  let e = styleString "E" [gray]

  let logo = styleString (w ++ " " ++ o ++ " " ++ r ++ " " ++ d ++ " " ++ l ++ " " ++ e) [bold]
  putStrLn logo

-- | A function that will prompt the player to choose a game type. They can
--   either play the daily word, a random word, or just close the app. If an
--   invalid choice is made, the function will loop until the player provides
--   a valid choice.
promptGameType :: Bool -> IO (Maybe GameType)
promptGameType isRetry = do
  if isRetry
    then putStrLn $ errorString "Invalid choice. Please try again.\n"
  else
    putStrLn ""

  putStrLn "What type of game would you like to play?"
  putStrLn "1. Daily Word"
  putStrLn "2. Random Word"
  putStrLn "3. Quit\n"

  input <- prompt "> "

  putStrLn ""

  case input of
    "1" -> return (Just GameTypeDaily)
    "2" -> return (Just GameTypeRandom)
    "3" -> return Nothing
    _   -> promptGameType True

-- | The application's bootstrap function. It sets the locale encoding to UTF-8
--   for proper text rendering, displays the logo, then retrieves the type of
--   the game the user wants to play, and starts the game.
runApp :: IO ()
runApp = do
  setLocaleEncoding utf8
  displayLogo
  gameType <- promptGameType False
  forM_ gameType startGame
