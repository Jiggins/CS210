module User where

data User = User { uID     :: Int
                 , age     :: Int
                 , gender  :: Gender
                 , job     :: String
                 , zipCode :: Int
                } deriving (Eq, Show)

data Gender = M | F
    deriving (Eq, Read)

instance Show Gender where
    show M = "Male"
    show F = "Female"