module Vingere where

import Data.Char (chr, isAlpha, ord, toUpper)

encrypt :: [(Char, Char)] -> String
encrypt =
  map (shift . codeToShift)
  where
    padding = ord 'A'
    codeToShift (x, code) = (x, ord code - padding)
    shift (char, shift) = chr ((((ord char - padding) + shift) `mod` 26) + padding)

vingere :: String -> String -> String
vingere text key =
  restoreNonAlpha $ encrypt charAndCode
  where
    cycledKey = cycle (toUpperString key)
    charAndCode = zip clearedText cycledKey
    clearedText = removeNonAlpha (toUpperString text)
    toUpperString = map toUpper
    removeNonAlpha = filter isAlpha
    restoreNonAlpha text' = foldl selector "" text
      where
        selector :: String -> Char -> String
        selector acc x =
          if not (isAlpha x)
            then acc ++ [x]
            else acc ++ [text' !! (length acc - getNonAlpha acc)]
          where
            getNonAlpha :: String -> Int
            getNonAlpha = length . filter (not . isAlpha)