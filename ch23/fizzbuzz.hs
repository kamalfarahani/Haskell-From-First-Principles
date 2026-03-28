module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State


fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n


addResult :: Integer -> State [String] ()
addResult n = do
    st <- get
    let result = fizzBuzz n
    put (result : st)


fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = 
    execState (mapM_ addResult list) []


fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to =
    fizzBuzzList $ [to, to-1..from]
