module Movie where

import Data.Function
import Data.List
import Data.List.Split

-- | Movie data type.  Used to store, print and compare Movies.
data Movie = Movie { movieId :: Int
                   , title   :: String
                   , date    :: String
                   , url     :: String
                   , rating  :: Double
                   , genres  :: [Genre]
                   } deriving (Eq, Read)

-- | Data type holding each of the Genre types.
-- This is an instance of Enum so that I can convert between Int and Genre
-- pretty easily.
-- This is also an instance of Ord, so that I can sort by Genre if needed at
-- some point in the future
data Genre = Unknown | Action      | Adventure | Animation | Children | Comedy
           | Crime   | Documentary | Drama     | Fantasy   | FilmNoir | Horror
           | Musical | Mystery     | Romance   | SciFi     | Thriller | Western
           deriving (Eq, Enum, Ord, Read, Show)

-- | Changing the way Movies are printed so that they print in the form
-- `ID    Title     Rating`
instance Show Movie where
  show m = intercalate "\t" . map ($m) $ [show . movieId, title, show . rating]

-- | Instancing Ord to compare Movies by their rating.
instance Ord Movie where
  compare = compare `on` rating

genreID :: Genre -> Int
genreID = (+1) . fromEnum

toGenre :: Int -> Genre
toGenre = toEnum . (\x -> x - 1)

data Rating = Rating { userId   :: Int
                     , rMovieId :: Int
                     , rRating  :: Int
                     } deriving (Eq)

instance Show Rating where
  show r = intercalate "\t" [show $ userId r, show $ rMovieId r, show $ rRating r]

instance Ord Rating where
  compare = compare `on` rMovieId
