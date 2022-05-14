module Utils (prompt, lowercase) where
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Data.Char

-- | Will prompt the user for input while flushing the standard output's buffer
--   so that the prompt itself and the user input is synced correctly.
prompt :: String -> IO String
prompt s = putStr s *> hFlush stdout *> getLine

-- | Wrapper around a function that converts all characters in a string to
--   lowercase.
lowercase :: String -> String
lowercase = map toLower
