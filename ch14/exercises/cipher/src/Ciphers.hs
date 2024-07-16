module Ciphers
  ( caesar,
    unCaesar,
    vingere,
    unVingere,
  )
where

import Data.Char

caesar :: Int -> String -> String
caesar n = map encode
  where
    encode c
      | not (isAlpha c) = c
      | isUpper c = toUpper coded
      | otherwise = coded
      where
        padding = ord 'a'
        coded = chr $ (ord c' - padding + n) `mod` 26 + padding
        c' = toLower c

unCaesar :: Int -> String -> String
unCaesar n = caesar (-n)

vingere :: String -> String -> String
vingere key text =
  restoreNonAlpha $ encrypt charAndCode
  where
    encrypt :: [(Char, Char)] -> String
    encrypt =
      map (shift . codeToShift)
      where
        padding = ord 'A'
        codeToShift (x, code) = (x, ord code - padding)
        shift (char, shift) = chr ((((ord char - padding) + shift) `mod` 26) + padding)
    cycledKey = cycle (toUpperString key)
    charAndCode = zip clearedText cycledKey
    clearedText = removeNonAlpha (toUpperString text)
    toUpperString = map toUpper
    removeNonAlpha = filter isAlpha
    restoreNonAlpha text' =
      foldl selector "" text
      where
        selector :: String -> Char -> String
        selector acc x =
          if not (isAlpha x)
            then acc ++ [x]
            else acc ++ [text' !! (length acc - getNonAlpha acc)]
          where
            getNonAlpha :: String -> Int
            getNonAlpha = length . filter (not . isAlpha)

unVingere :: String -> String -> String
unVingere key = vingere (antikey (map toUpper key))
  where
    antikey :: String -> String
    antikey = map antiChar
      where
        antiChar c = chr ((26 - alphaCode c) + ord 'A')

alphaCode :: Char -> Int
alphaCode x =
  if not (isAlpha x)
    then error "Not an alpha character"
    else ord x - ord 'A'