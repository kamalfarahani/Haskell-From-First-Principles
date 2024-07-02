module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: (Integral a) => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise =
          go (n - d) d (count + 1)

myMult :: (Eq a, Num a) => a -> a -> a
myMult x y = go x y 0
  where
    go x y count
      | x == 0 = count
      | otherwise = go (x - 1) y (count + y)

testAddition :: IO ()
testAddition = hspec $ do
  describe "Addition" $ do
    it "1 + 1 = 2" $ do
      1 + 1 `shouldBe` 2
    it "2 + 2 > 3" $ do
      2 + 2 > 3 `shouldBe` True

testDividedBy :: IO ()
testDividedBy = hspec $ do
  describe "DividedBy" $ do
    it "10 / 5 = 2 remainder 0" $ do
      dividedBy 10 5 `shouldBe` (2, 0)

testMyMult :: IO ()
testMyMult = hspec $ do
  describe "MyMult" $ do
    it "10 * 5 = 50" $ do
      myMult 10 5 `shouldBe` 50
    it "5 * 5 = 25" $ do
      myMult 5 5 `shouldBe` 25

propertyTestAddition :: IO ()
propertyTestAddition =
  hspec $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

prop_multGreater :: Int -> Bool
prop_multGreater x = (x > 0) && (x * 2 > x)

runQc :: IO ()
runQc = do
  quickCheck prop_additionGreater
  quickCheck prop_multGreater