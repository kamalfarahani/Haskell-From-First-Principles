module ListyInstance where

import Listy

instance Semigroup (Listy a) where
  (<>) (Listy l) (Listy l') = Listy (l ++ l')

instance Monoid (Listy a) where
  mempty = Listy []