module Main (
    main
) where

import GPXparser
import System.Console.CmdArgs.Explicit
import System.Exit
import System.IO

main = do
  hSetBuffering stdin LineBuffering
  doLoop
  
doLoop = do
  putStrLn "Enter a command. Enter ? for help:"
  command <- getLine
  case command of
    '?':_ -> do help; doLoop
    'q':_ -> do putStrLn ("Quitting!")
    's':_ -> do summarize; doLoop

    _            -> do putStrLn ("Nothing entered"); doLoop

summarize = do 
  putStrLn "Enter a test file name with path:"
  filename <- getLine
  case filename of
    'q':_ -> do putStrLn ("Quitting!")
    's':input-> do summarizeGPX input; summarize

    _            -> do putStrLn ("Nothing entered. Try again"); summarize
  

help = do
  putStrLn "s - Summarizing GPX file"
  putStrLn "? - Get this help message"
  putStrLn "q - Quit the program"