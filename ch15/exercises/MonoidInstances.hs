module MonoidInstances where

import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary
    return (BoolConj x)

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary
    return (BoolDisj x)

newtype Combine a b = Combine {unCombine :: a -> b}

instance (Semigroup b) => Semigroup (Combine a b) where
  f <> g = Combine (\x -> unCombine f x <> unCombine g x)

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (const mempty)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return (Combine f)

newtype Comp a = Comp {unComp :: a -> a}

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return (Comp f)

newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance Semigroup (Mem s a) where
  m1 <> m2 = Mem f
  where
    f s = (a2, s2)
      where
        (a1, s1) = runMem m1 s
        (a2, s2) = runMem m2 s1

instance Monoid (Mem s a) where
  mempty = \x -> ()