module Main (
  main
) where

import GPXparser
--import System.Console.CmdArgs.Explicit
--import System.Exit
import Control.Monad(forM_)
import Data.List(isSuffixOf)
import System.Directory (getDirectoryContents)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  doLoop
 
doLoop :: IO () 
doLoop = do
  putStrLn "Enter a command. Enter ? for help:"
  command <- getLine
  case command of
    '?':_ -> do help; doLoop
    'q':_ -> do putStrLn ("Quitting!")
    'S':_-> do writeGPXFile; doLoop
    's':_-> do summarizeGPXFile ; doLoop
    _    -> do putStrLn ("Invalid  command"); doLoop

--summarize just the given file
summarizeGPXFile :: IO()
summarizeGPXFile = do
  putStrLn "Enter the filename:"
  filename <- getLine
  summarizeGPX filename

--summarize all the .gpx files it can find in the curently directory
writeGPXFile :: IO()
writeGPXFile = do
  gpxFiles <- filesWithExt "." ".gpx"
  putStrLn ("Summarizing gpx file(s) to summary.txt file")
  forM_ gpxFiles $ \file -> do summarizeGPXDataToFile file


filesWithExt :: FilePath -> String -> IO [String]
filesWithExt dir ext = do
  contents <- getDirectoryContents dir
  return $ filter (isSuffixOf ext) contents
  
help :: IO ()
help = do
  putStrLn "USAGE\n "
  putStrLn "S - Summarize all GPX file in the directory and write to file"
  putStrLn "s - Summarize given GPX file"
  putStrLn "? - Get this help message"
  putStrLn "q - Quit the program"

