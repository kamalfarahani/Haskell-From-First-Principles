module Main (main) where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


type WordList = [String]

data Puzzle = 
  Puzzle String [Maybe Char] [Char]


instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (map renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed


minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9


freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = toLower c `elem` map toLower s


alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ gussed) c = toLower c `elem` gussed


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c


fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word guessed chars) c = 
  Puzzle word newGuessed (c':chars)
  where
    newGuessed = zipWith selector word guessed
    c' = toLower c
    selector :: Char -> Maybe Char -> Maybe Char
    selector wordChar guessedChar =
      if toLower wordChar == c'
        then Just wordChar
        else guessedChar


allWords :: IO WordList
allWords = do
  dict <- readFile "./data/dict"
  return (lines dict)


gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return $ filter isInRange aw
  where
    isInRange :: String -> Bool
    isInRange w = 
      length w >= minWordLength && length w <= maxWordLength


randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex


randomWord' :: IO String
randomWord' = gameWords >>= randomWord


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)


gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) = 
  if (length guessed) > 7
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
    else return ()


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
    then do
      putStrLn "You win!"
      exitSuccess
    else return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is:\n" ++ show puzzle
  guess <- getLine
  case guess of
    [c] -> do
      putStrLn "\n_________________\n"
      newPuzzle <- handleGuess puzzle c
      runGame newPuzzle
    _ -> putStrLn "Your guess must be a single character"


main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle word
  runGame puzzle