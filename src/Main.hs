module Main (
  main
) where

import GPXparser
--import System.Console.CmdArgs.Explicit
--import System.Exit
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
    's':_-> do 
           putStrLn "Enter filename:" 
           filename <- getLine
           if null filename
                then doLoop--putStrLn("Nothing entered")
                else do
                      summarizeGPX filename; doLoop;
    _    -> do putStrLn ("Invalid  command"); doLoop
  
help :: IO ()
help = do
  putStrLn "USAGE\n "
  putStrLn "s - Summarizing GPX file"
  putStrLn "? - Get this help message"
  putStrLn "q - Quit the program"
  