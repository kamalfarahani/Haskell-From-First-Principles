module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure :: a -> List a
  pure a = Cons a Nil
  (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  fs <*> xs = flatMap (<$> xs) fs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    hlist <- arbitrary
    let l = haskellListToList hlist
    return l

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

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
  let trigger :: List (String, Int, [String])
      trigger = undefined
  quickBatch $ applicative trigger