module Parser where

import Movie

import Data.Function
import Data.List.Split
import Data.Maybe
import Text.Regex
import Text.Regex.Posix

import Prelude hiding (id)

parseMovie :: [String] -> Movie
parseMovie (id:title:date:url:genres) = Movie (read id)
                                               title
                                               date
										      (parseUrl url)
										       0
										      (parseGenres genres)


parseUrl :: String -> String
parseUrl = filter (/= '\"') . head . splitRegex (mkRegex "\\(.*\\)")

parseGenre :: (Num a, Eq a) => (Int, a) -> Maybe Genre
parseGenre (g, 1) = Just $ toGenre g
parseGenre _      = Nothing

parseGenres :: [String] -> [Genre]
parseGenres = catMaybes . map parseGenre . zip [1..18] . map read

parseRating :: String -> Rating
parseRating = parse . splitOn "\t"
	where parse (uId:mId:rating:_) = Rating (read uId) (read mId) (read rating)