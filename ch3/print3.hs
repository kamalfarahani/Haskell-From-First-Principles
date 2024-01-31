module Print3 where


myGreeting :: String
myGreeting = "Hello" ++ " World"

hello :: String
hello = "Hello"

world :: String
world = "World"

main :: IO ()
main = do
    putStrLn myGreeting
    putStrLn $ concat [hello, " ", world]