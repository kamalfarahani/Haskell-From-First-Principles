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

type TrivId = Trivial -> Bool

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

type IdAssoc = Identity String -> Identity String -> Identity String -> Bool

type IdId = Identity String -> Bool

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

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolConjId = BoolConj -> Bool

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

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type BoolDisjId = BoolDisj -> Bool

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

instance (Semigroup a) => Semigroup (Mem s a) where
  m1 <> m2 = Mem f
    where
      f s = (a1 <> a2, s2)
        where
          (a1, s1) = runMem m1 s
          (a2, s2) = runMem m2 s1

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ \x -> (mempty, x)

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = do
    f <- arbitrary
    return (Mem f)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

mli :: (Monoid a, Eq a) => a -> Bool
mli a = (mempty <> a) == a

mri :: (Monoid a, Eq a) => a -> Bool
mri a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)

  quickCheck (mli :: TrivId)
  quickCheck (mli :: IdId)
  quickCheck (mli :: BoolConjId)
  quickCheck (mli :: BoolDisjId)

  quickCheck (mri :: TrivId)
  quickCheck (mri :: IdId)
  quickCheck (mri :: BoolConjId)
  quickCheck (mri :: BoolDisjId)