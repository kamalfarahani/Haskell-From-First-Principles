module Vingere where

import Data.Char


encrypt :: [(Char, Char)] -> String
encrypt =
    map (shift . codeToShift)
    where
        padding                = ord 'A'
        codeToShift :: (Char, Char) -> (Char, Int)
        codeToShift (x, code)  = (x, ord code - padding)
        shift :: (Char, Int) -> Char
        shift (char, shift)    = chr ((((ord char - padding ) + shift ) `mod` 26) + padding)


vingere :: String -> String -> String
vingere text key =
    restoreNonAlpha $ encrypt charAndCode
    where
        charAndCode         = zip clearedText cycledKey
        clearedText         = removeNonAlpha (toUpperString text)
        cycledKey           = cycle (toUpperString key)
        toUpperString :: String -> String
        toUpperString = map toUpper
        removeNonAlpha :: String -> String
        removeNonAlpha = filter isAlpha
        restoreNonAlpha :: String -> String
        restoreNonAlpha text' = foldl selector "" text
            where
                selector :: String -> Char -> String
                selector acc x = 
                    if not (isAlpha x)
                    then acc ++ [x]
                    else acc ++ [text' !! (length acc - getNonAlpha acc)]
                    where
                        getNonAlpha :: String -> Int
                        getNonAlpha str = 
                            sum $ map (\char -> if isAlpha char then 0 else 1) str