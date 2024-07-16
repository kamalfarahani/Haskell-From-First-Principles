import Test.QuickCheck
import Ciphers (caesar, unCaesar, unVingere, vingere)
import Data.Char (toUpper)

alphabet :: String
alphabet = ['A'..'Z'] ++ ['a'..'z']

main :: IO ()
main = do
  quickCheck prop_Vingere
  quickCheck prop_Ceasar

toUpperStr :: String -> String
toUpperStr = map toUpper

allAlpha :: String -> Bool
allAlpha = all (`elem` alphabet)

prop_Vingere :: String -> String -> Property
prop_Vingere key text =
  (not (null key) && allAlpha key && allAlpha text) ==> thereAndBackAgain
  where
    thereAndBackAgain = 
      unVingere key (vingere key text) == toUpperStr text

prop_Ceasar :: Int -> String -> Property
prop_Ceasar key text =
  allAlpha text ==> thereAndBackAgain
  where
    thereAndBackAgain = 
      toUpperStr (unCaesar key (caesar key text)) == toUpperStr text