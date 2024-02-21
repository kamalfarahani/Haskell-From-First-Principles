data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving Show

data Date = Date DayOfWeek Int deriving Show


instance Eq DayOfWeek where
    (==) :: DayOfWeek -> DayOfWeek -> Bool
    Mon  == Mon   = True
    Tue  == Tue   = True
    Weds == Weds  = True
    Thu  == Thu   = True
    Fri  == Fri   = True
    Sat  == Sat   = True
    Sun  == Sun   = True
    _    ==  _    = False


instance Eq Date where
    (==) :: Date -> Date -> Bool
    (Date dayOfWeek dayOfMonth) == (Date dayOfWeek' dayOfMonth') = 
        (dayOfWeek == dayOfWeek') && (dayOfMonth == dayOfMonth')

-- A partial function
f :: DayOfWeek -> Date
f Mon = Date Mon 10