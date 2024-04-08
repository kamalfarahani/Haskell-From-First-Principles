module Reverse where
import Distribution.Compat.CharParsing (CharParsing(text))


rvrs :: String -> String
rvrs x =
    concat [word_2, is,  word_1]
    where
        word_1 = take (length "Curry") x
        word_2 = drop (length "Curry is ") x
        is = " is "


main :: IO ()
main =
    print $ rvrs text
    where text = "Curry is awesome"