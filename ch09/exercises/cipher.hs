module Cipher where

import Data.Char


caesar :: Int -> String -> String
caesar n = map encode
    where 
        encode c
            | not (isAlpha c) = c
            | isUpper c = toUpper coded
            | otherwise = coded
            where padding = ord 'a'
                  coded   = chr $ (ord c' - padding + n) `mod` 26 + padding
                  c'      = toLower c


unCaesar :: Int -> String -> String
unCaesar n = caesar (-n)