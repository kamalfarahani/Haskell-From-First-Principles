module Morse
  ( charToMorse,
    morseToChar,
    stringToMorse,
    letterToMorse,
    morseToLetter,
    MorseCode,
  )
where

import Data.Char (toUpper)
import qualified Data.Map as M

type MorseCode = String

letterToMorse :: M.Map Char MorseCode
letterToMorse =
  M.fromList
    [ ('A', ".-"),
      ('B', "-..."),
      ('C', "-.-."),
      ('D', "-.."),
      ('E', "."),
      ('F', "..-."),
      ('G', "--."),
      ('H', "...."),
      ('I', ".."),
      ('J', ".---"),
      ('K', "-.-"),
      ('L', ".-.."),
      ('M', "--"),
      ('N', "-."),
      ('O', "---"),
      ('P', ".--."),
      ('Q', "--.-"),
      ('R', ".-."),
      ('S', "..."),
      ('T', "-"),
      ('U', "..-"),
      ('V', "...-"),
      ('W', ".--"),
      ('X', "-..-"),
      ('Y', "-.--"),
      ('Z', "--.."),
      ('0', "-----"),
      ('1', ".----"),
      ('2', "..---"),
      ('3', "...--"),
      ('4', "....-"),
      ('5', "....."),
      ('6', "-...."),
      ('7', "--..."),
      ('8', "---.."),
      ('9', "----.")
    ]

morseToLetter :: M.Map MorseCode Char
morseToLetter = M.foldrWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe MorseCode
charToMorse c = M.lookup c letterToMorse

morseToChar :: MorseCode -> Maybe Char
morseToChar m = M.lookup m morseToLetter

stringToMorse :: String -> Maybe [MorseCode]
stringToMorse text = mapM charToMorse textLower
  where
    textLower = map toUpper text