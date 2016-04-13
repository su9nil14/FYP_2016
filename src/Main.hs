module Main (
  main
) where

import GPXparser
import Chart
import Summary
import Control.Monad(forM_)
import Data.List(isSuffixOf)
import Text.XML.HXT.Core
import Text.Html
import System.FilePath
import System.Directory
import System.IO
--import System(getArgs)


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
    'c' :_ -> do drawPNGChart; doLoop
    'S':_-> do summarizeAllGPXFile; doLoop
    's':_-> do summarizeGPXFile ; doLoop
    'f':_-> do fastestKmPace; doLoop
    _    -> do putStrLn ("Invalid  command"); doLoop

--summarize just the given file
fastestKmPace :: IO()
fastestKmPace = do
  putStrLn "Enter the filename:"
  filename <- getLine
  putStrLn "Enter distance parameter:"
  dist <- getLine
  let d = read dist :: Double
  getFastestKMPace filename d

--getIntArg :: IO Int
--getIntArg = fmap (read . head) getArgs


--summarize just the given file
summarizeGPXFile :: IO()
summarizeGPXFile = do
  putStrLn "Enter the filename:"
  filename <- getLine
  summarizeGPX filename

--summarize all the .gpx files it can find in the curently directory to txt file
summarizeAllGPXFile :: IO [()]
summarizeAllGPXFile = do
  let path = "reportsTxt"
  putStrLn "Enter filename to save report to"
  filename <- getLine
  createEmptyDirectory path
  curDir <- getCurrentDirectory
  allFiles <- getDirectoryContents curDir
  let allFilesSplit = map splitExtension allFiles
  let gpxFiles = filter (\(_,b) -> b==".gpx") allFilesSplit
  putStrLn ("Processing "++show (length gpxFiles)++" file(s)...")
  mapM (\(a,b) -> summarizeGPXDataToTxtFile (a++b) (path++"/"++filename)) gpxFiles



--summarize all the .gpx files it can find in the curently directory in html format
generateHtmlReport :: IO [()]
generateHtmlReport = do
  putStrLn "Enter the filename to save Html report:"
  filename <- getLine
  curDir <- getCurrentDirectory
  allFiles <- getDirectoryContents curDir
  let allFilesSplit = map splitExtension allFiles
  let gpxFiles = filter (\(_,b) -> b==".gpx") allFilesSplit
  putStrLn ("Processing "++show (length gpxFiles)++" file(s)...")
  mapM (\(a,b) -> writeHtmlReport filename (a++b) ) gpxFiles


writeHtmlReport :: String->String -> IO ()
writeHtmlReport filename gpxFile = do
  points <- runX (parseGPX gpxFile >>> getTrkpt)
  [segs] <- runX (parseGPX gpxFile >>> getTrkseg)

  createEmptyDirectory "reportsHtml"
  let path =  ("reportsHtml/"++filename)
  putStrLn "Generating report..." 
  appendFile ("reportsHtml/"++filename) (renderHtml $ generateHtmlPage gpxFile points segs)
  putStrLn $ "Report saved in: "++path



--drawPNGChart :: [Char]-> String -> IO (Graphics.Rendering.Chart.Renderable.PickFn ())
drawPNGChart  = do
  createEmptyDirectory "reportsPNG"
  putStrLn "Enter the GPX filename:"
  gpxFile <- getLine
  point <- runX (parseGPX gpxFile >>> getTrkpt)
  [points] <- runX (parseGPX gpxFile >>> getTrkseg)
  putStrLn "Enter the filename to save PNG chart to:"
  filename <- getLine
  let path =  ("reportsPNG/"++filename)
  putStrLn "Drawing chart..." 
  drawChart point path
  drawChart2 point points path


  
help :: IO ()
help = do
  putStrLn "USAGE  [COMMAND]\n "
  putStrLn "S - Summarize all GPX file in the directory to terminal and write to text file"
  putStrLn "s - Summarize given GPX file to terminal"
  putStrLn "r - Summarize all GPX file in the directory and write to HTML file"
  putStrLn "c - Summarize all GPX file in the directory and draw chart in PNG format"
  putStrLn "f - Get the fastest km pace in the long track"
  putStrLn "? - Get this help message"
  putStrLn "q - Quit the program"


createEmptyDirectory :: FilePath -> IO ()
createEmptyDirectory dir = do
       exists <- doesDirectoryExist dir
       createDirectoryIfMissing exists dir
