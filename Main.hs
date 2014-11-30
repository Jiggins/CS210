module Main where

import Data.List
import Data.List.Split
import Data.Ord
import Data.Function

import System.Environment
import System.IO

import Movie
import Parser

open :: FilePath -> IO String
open file = openFile file ReadMode >>= hGetContents >>= return

getLines :: FilePath -> IO [String]
getLines file = open file >>= return . lines

ratingsPerUser = groupBy ((==) `on` userId)

main :: IO ()
main = do
    movies <- getLines "data/Movies.csv" >>= 
        return . map (parseMovie . splitOn "\t" . filter (/= '\r')) . take 10

    ratings <- getLines "data/Ratings.csv" >>=
        return . map (parseRating . filter (/= '\r')) . take 200

    print . ratingsPerUser $ ratings
