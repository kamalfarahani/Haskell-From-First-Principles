module Main where

import Hello ( sayHello )


main :: IO ()
main = do
  sayHello "Haskell"
