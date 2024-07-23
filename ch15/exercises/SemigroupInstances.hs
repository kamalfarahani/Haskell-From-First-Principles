module SemigroupInstances where

import Data.Semigroup (Semigroup)
import Test.QuickCheck (Arbitrary, CoArbitrary, arbitrary, elements, quickCheck)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

type IdAssoc = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (Three x y z)

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four x y z t) <> (Four x' y' z' t') = Four (x <> x') (y <> y') (z <> z') (t <> t')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    t <- arbitrary
    return (Four x y z t)

type FourAssoc = Four String String String String -> Four String String String String -> Four String String String String -> Bool

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary
    return (BoolConj x)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary
    return (BoolDisj x)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd x) <> _ = Snd x
  (Fst _) <> (Snd x) = Snd x
  (Fst _) <> (Fst x) = Fst x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Fst x, Snd y]

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

newtype Combine a b = Combine {unCombine :: a -> b}

instance (Semigroup b) => Semigroup (Combine a b) where
  f <> g = Combine (\x -> unCombine f x <> unCombine g x)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return (Combine f)

type CombineAssoc = Combine String Int -> Combine String Int -> Combine String Int -> Bool

newtype Comp a = Comp {unComp :: a -> a}

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return (Comp f)

type CompAssoc = Comp String -> Comp String -> Comp String -> Bool

data Validation a b
  = Failure a
  | Success b
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b) where
  Failure x <> Failure y = Failure (x <> y)
  Failure x <> Success y = Success y
  Success x <> Failure y = Success x
  Success x <> Success y = Success x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Failure x, Success y]

type ValidationAssoc = Validation String Int -> Validation String Int -> Validation String Int -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  -- quickCheck (semigroupAssoc :: CompAssoc)
  -- quickCheck (semigroupAssoc :: CombineAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)