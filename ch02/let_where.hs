module LetWhere where

f :: Num a => a -> a
f x = 
    let y = x + 1
    in let z = y * 2
       in z + 3

g :: Num a => a -> a
g x = 
    x * y + z
    where y = x + 1
          z = y * 2