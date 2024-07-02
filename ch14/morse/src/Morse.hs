module Morse
  ( charToMorse,
    morseToChar,
    stringToMorse,
    letterToMorse,
    morseToLetter,
  )
where

import qualified Data.Map as M

type Morse = String

letterToMorse :: M.Map Char Morse
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

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldrWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

stringToMorse :: String -> Maybe [Morse]
stringToMorse = mapM charToMorse