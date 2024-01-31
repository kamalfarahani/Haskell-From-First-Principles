module TopOrLocal where


topLevelFunction :: Integer -> Integer
topLevelFunction x =
    x + y + 10
    where y :: Integer
          y = 2

topLevelValue :: Integer
topLevelValue = 10