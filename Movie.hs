module Movie where

import Data.Function
import Data.List
import Data.List.Split

data Movie = Movie { movieId :: Int
                   , title   :: String
                   , date    :: String
                   , url     :: String
                   , rating  :: Double
                   , genres  :: [Genre]
                   } deriving (Eq, Read)

data Genre = Unknown | Action      | Adventure | Animation | Children | Comedy 
           | Crime   | Documentary | Drama     | Fantasy   | FilmNoir | Horror
           | Musical | Mystery     | Romance   | SciFi     | Thriller | Western
           deriving (Eq, Enum, Ord, Read, Show)

instance Show Movie where
  show m = intercalate "\t" . map ($m) $ [show . movieId, title, show . rating]

instance Ord Movie where
  compare = compare `on` rating

genreID :: Genre -> Int
genreID = (+1) . fromEnum

toGenre :: Int -> Genre
toGenre = toEnum . (\x -> x - 1)

data Rating = Rating { userId  :: Int
                     , movieId :: Int
                     , rating  :: Double
                     } deriving (Eq)

instance Show Rating where
  show r = intercalate "\t" [show $ userId r, show $ movieId r, show $ rating r]