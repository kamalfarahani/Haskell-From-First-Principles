{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data S n a = S (n a) a deriving (Eq, Show)


instance (Functor n, Arbitrary (n a) , Arbitrary a ) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n, Testable (n Property), EqProp a ) => EqProp (S n a) where
    (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance Functor n => Functor (S n) where
    fmap f (S xs x) = S (fmap f xs) (f x)

instance Foldable n => Foldable (S n) where
    foldMap f (S xs x) = foldMap f xs <> f x

instance Traversable n => Traversable (S n) where
    traverse f (S xs x) = S <$> traverse f xs <*> f x

main = 
    sample' (arbitrary :: Gen (S [] Int))