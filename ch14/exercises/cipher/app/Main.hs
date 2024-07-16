module Main (main) where

import Ciphers (vingere)

main :: IO ()
main = do
  putStrLn "Enter your text"
  text <- getLine

  putStrLn "Enter your key"
  key <- getLine

  putStrLn $ vingere key text