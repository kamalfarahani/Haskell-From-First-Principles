module Main (main) where

import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Traversable (traverse)
import Morse (morseToChar, stringToMorse)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)

convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess
  line <- hGetLine stdin
  convertLine line
  where
    convertLine :: String -> IO ()
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just strList) -> do
          putStrLn (unwords strList)
        Nothing -> do
          putStrLn "ERROR: "
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess
  line <- hGetLine stdin
  convertLine line
  where
    convertLine :: String -> IO ()
    convertLine line = do
      let decoded :: Maybe String
          decoded = traverse morseToChar (words line)
      case decoded of
        (Just s) -> putStrLn s
        Nothing -> do
          putStrLn "ERROR: "
          exitFailure

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] ->
      case arg of
        "from" -> convertFromMorse
        "to" -> convertToMorse
        _ -> argError
    _ -> argError
  where
    argError = do
      putStrLn "ERROR: "
      putStrLn "Please specify the correct mode: 'from' or 'to'"
      exitFailure
