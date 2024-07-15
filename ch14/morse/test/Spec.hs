module Main where

import Data.Map qualified as M
import Morse
import Morse (letterToMorse)
import Test.QuickCheck

main :: IO ()
main = quickCheck prop_thereAndBackAgain

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [MorseCode]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen MorseCode
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll
    charGen
    (\c -> (charToMorse c >>= morseToChar) == Just c)