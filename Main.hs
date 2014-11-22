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
main = getLines "data/Movies.csv" >>= 
    mapM_ print . map (parseMovie . splitOn "\t" . filter (/= '\r')) . take 1