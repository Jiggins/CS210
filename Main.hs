module Main where

import Data.List.Split

import System.Environment
import System.IO

import Movie
import Parser

open :: FilePath -> IO String
open file = openFile file ReadMode >>= hGetContents >>= return

getLines :: FilePath -> IO [String]
getLines file = open file >>= return . lines

main :: IO ()
main = do
    movies <- getLines "data/Movies.csv" >>= 
        return . map (parseMovie . splitOn "\t" . filter (/= '\r')) . take 1
    ratings <- getLines "data/Ratings.csv" >>=
        return . map (parseRating . filter (/= '\r')) . take 1
    print ratings