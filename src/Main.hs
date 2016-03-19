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
    's':_-> do summarizeGPXFiles; doLoop
    _    -> do putStrLn ("Invalid  command"); doLoop


summarizeGPXFiles :: IO()
summarizeGPXFiles = do
  gpxFiles <- filesWithExt "." ".gpx"
  putStrLn ("Summarizing "++show (length gpxFiles)++" gpx file(s)...")
  forM_ gpxFiles $ \file -> do summarizeGPX file

filesWithExt :: FilePath -> String -> IO [String]
filesWithExt dir ext = do
  contents <- getDirectoryContents dir
  return $ filter (isSuffixOf ext) contents
  
help :: IO ()
help = do
  putStrLn "USAGE\n "
  putStrLn "s - Summarizing GPX file"
  putStrLn "? - Get this help message"
  putStrLn "q - Quit the program"

