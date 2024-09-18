module ZipListApplicative where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  fs <*> xs = flatMap (<$> xs) fs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap haskellListToList arbitrary

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = fmap (ZipList' . haskellListToList) arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where 
      xs' = 
        let (ZipList' l) = xs
        in take' 3000 l
      ys' = 
        let (ZipList' l) = ys
        in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ haskellListToList (repeat x)
  (<*>) (ZipList' fs) (ZipList' xs) =
    ZipList' $ fmap (\(f, x) -> f x) zipped
    where
      zipped = zip' fs xs

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons x xs) =
  if n <= 0
    then Nil
    else Cons x $ take' (n - 1) xs

zip' :: List a -> List b -> List (a, b)
zip' _ Nil = Nil
zip' Nil _ = Nil
zip' (Cons x xs) (Cons y ys) = Cons (x, y) $ zip' xs ys

haskellListToList :: [a] -> List a
haskellListToList = foldr Cons Nil

append :: List a -> List a -> List a
append Nil ys = ys
append xs Nil = xs
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

main :: IO ()
main = do
  let trigger :: ZipList' (String, Int, [String])
      trigger = undefined
  quickBatch $ applicative trigger