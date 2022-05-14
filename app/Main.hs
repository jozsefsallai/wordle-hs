module Main where

import System.IO.CodePage (withCodePageOptions, defaultOptions, chatty, cp65001)
import Lib

-- Emoji rendering is painful (specifically on Windows), so we have to wrap the
-- whole app around a monad that will switch the terminal's code page to UTF-8.

main :: IO ()
main = withCodePageOptions defaultOptions{chatty = True} cp65001 runApp
