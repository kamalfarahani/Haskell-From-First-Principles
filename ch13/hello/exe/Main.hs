module Main where

import Hello ( sayHello )
import DogsRule ( dogs )
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Who are you? "
  name <- getLine
  sayHello name
  dogs
