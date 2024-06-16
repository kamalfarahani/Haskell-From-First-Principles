module Main where

import Hello ( sayHello )
import DogsRule ( dogs )


main :: IO ()
main = do
  sayHello "Haskell"
  dogs
