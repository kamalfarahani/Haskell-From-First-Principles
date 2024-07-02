module Main (main) where

import Addition

main :: IO ()
main = do
    testAddition
    testDividedBy
    testMyMult
    propertyTestAddition