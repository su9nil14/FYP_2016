module Main (
  main
) where

import GPXparser
import Control.Monad(forM_)
import Data.List(isSuffixOf)
import Text.XML.HXT.Core
import Text.Html
import System.FilePath
import System.Directory
import System.IO

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  doLoop
 
doLoop :: IO () 
doLoop = do
  putStrLn "\nEnter a command. Enter ? for help:"
  command <- getLine
  case command of
    '?':_ -> do help; doLoop
    'q':_ -> do putStrLn ("Quitting!")
    'r' :_ -> do generateHtmlReport; doLoop
    'S':_-> do summarizeAllGPXFile; doLoop
    's':_-> do summarizeGPXFile ; doLoop
    _    -> do putStrLn ("Invalid  command"); doLoop

--summarize just the given file
summarizeGPXFile :: IO()
summarizeGPXFile = do
  putStrLn "Enter the filename:"
  filename <- getLine
  summarizeGPX filename

--summarize all the .gpx files it can find in the curently directory to txt file
--summarizeAllGPXFile :: IO()
summarizeAllGPXFile = do
  curDir <- getCurrentDirectory
  allFiles <- getDirectoryContents curDir
  let allFilesSplit = map splitExtension allFiles
  let gpxFiles = filter (\(_,b) -> b==".gpx") allFilesSplit
  putStrLn ("Processing "++show (length gpxFiles)++" file(s)...")
  mapM (\(a,b) -> summarizeGPXDataToFile (a++b) ) gpxFiles



--summarize all the .gpx files it can find in the curently directory in html format
--generateHtmlReport :: IO()
generateHtmlReport = do
  curDir <- getCurrentDirectory
  allFiles <- getDirectoryContents curDir
  let allFilesSplit = map splitExtension allFiles
  let gpxFiles = filter (\(_,b) -> b==".gpx") allFilesSplit
  putStrLn ("Processing "++show (length gpxFiles)++" file(s)...")
  mapM (\(a,b) -> writeHtmlReport (a++b) ) gpxFiles


writeHtmlReport gpxFile = do
  points <- runX (parseGPX gpxFile >>> getTrkpt)
  [segs] <- runX (parseGPX gpxFile >>> getTrkseg)

  let path =  ("reportsHtml/"++gpxFile)
  putStrLn "Generating report..." 
  writeFile (path++".html") (renderHtml $ generateHtmlPage gpxFile points segs)
  putStrLn $ gpxFile ++" processed. Report saved in: "++path

  
help :: IO ()
help = do
  putStrLn "USAGE\n "
  putStrLn "S - Summarize all GPX file in the directory and write to text file"
  putStrLn "s - Summarize given GPX file"
  putStrLn "r - Summarize all GPX file in the directory and write to HTML file"
  putStrLn "? - Get this help message"
  putStrLn "q - Quit the program"

