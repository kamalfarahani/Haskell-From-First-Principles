module ValidationApplicative where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Validation e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
    fmap _ (Failure' e) = Failure' e
    fmap f (Success' x) = Success' (f x)

-- This is different
instance Monoid e => Applicative (Validation e) where
    pure = Success'
    (<*>) (Failure' e1) (Failure' e2) = Failure' (e1 <> e2)
    (<*>) (Failure' e) _ = Failure' e
    (<*>) _ (Failure' e) = Failure' e
    (<*>) (Success' f) (Success' x) = Success' (f x)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [Failure' x, Success' y]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

main :: IO ()
main = do
  let trigger :: Validation [String] (String, Int, [Int])
      trigger = undefined
  quickBatch $ applicative trigger