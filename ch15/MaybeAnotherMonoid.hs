module MaybeAnotherMonoid where

import Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

newtype First' a = First' {getFirst' :: Optional a}
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) :: First' a -> First' a -> First' a
  First' (Only x) <> _ = First' (Only x)
  First' Nada <> First' (Only x) = First' (Only x)
  _ <> _ = First' Nada

instance Monoid (First' a) where
  mempty :: First' a
  mempty = First' Nada

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    frequency
      [ (1, return First' {getFirst' = Nada}),
        (3, return $ First' {getFirst' = Only x})
      ]

type FirstMappend =
  First' String ->
  First' String ->
  First' String ->
  Bool

type FirstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FirstId)
  quickCheck (monoidRightIdentity :: FirstId)