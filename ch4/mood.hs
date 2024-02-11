module Mood where


data Mood = Blah | Woot deriving Show

instance Eq Mood where
    (==) :: Mood -> Mood -> Bool
    (==) Blah Blah = True
    (==) Woot Woot = True
    (==) _ _ = False

instance Ord Mood where
    compare :: Mood -> Mood -> Ordering
    compare Blah _ = GT
    compare _ Blah = LT
    compare _ _ = EQ

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah