module MonadInstances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Nope a = NopeDotJpg 
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

data PhhhbbtttEither b a = 
    Lefty a 
  | Righty b 
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Lefty a) = Lefty $ f a
  fmap _ (Righty b) = Righty b

instance Applicative (PhhhbbtttEither b) where
  pure = Lefty
  (<*>) (Lefty f) (Lefty a) = Lefty $ f a
  (<*>) (Righty b) _ = Righty b
  (<*>) _ (Righty b) = Righty b

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Lefty a) f = f a
  (>>=) (Righty b) _ = Righty b

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Lefty x, Righty y]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

newtype Identity a = Identity a 
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

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

instance Monad List where
  return = pure
  (>>=) xs f = flatMap f xs

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
  putStrLn "Testing Identity"
  let trigger :: Identity (String, Int, [String])
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

  putStrLn "Testing List"
  let trigger' :: List (String, Int, [String])
      trigger' = undefined
  quickBatch $ functor trigger'
  quickBatch $ applicative trigger'
  quickBatch $ monad trigger'

  putStrLn "Testing Nope"
  let trigger'' :: Nope (String, Int, [String])
      trigger'' = undefined
  quickBatch $ functor trigger''
  quickBatch $ applicative trigger''
  quickBatch $ monad trigger''

  putStrLn "Testing PhhhbbtttEither"
  let trigger''' :: PhhhbbtttEither (String, Int, [String]) (String, Int, [String])
      trigger''' = undefined
  quickBatch $ functor trigger'''
  quickBatch $ applicative trigger'''
  quickBatch $ monad trigger'''