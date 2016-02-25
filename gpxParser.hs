{-# LANGUAGE Arrows #-}

module Main (
    main
) where

import Text.XML.HXT.Core
import Data.Time(UTCTime, readTime, diffUTCTime)
import Data.Time.Format(defaultTimeLocale)
import Text.Printf (printf)
import Text.Printf
import System.Environment
import Data.Tree.NTree.TypeDefs
import Graphics.Gnuplot.Simple
import System.Console.Haskeline
import Control.Monad.Trans



type Repl a = InputT IO a


data Trkseg = Trkseg [Trkpt] deriving (Eq, Show)

--define data structure - the child elements
data Trkpt = Trkpt {
  time :: UTCTime,
  longitude :: Double,
  latitude :: Double,
  elevation :: Double
  } deriving (Eq, Show)

-- derive time and timezone stuffs -- getting error -----  
--In the use of `readTime' (imported from Data.Time, but defined in time-1.6:Data.Time.Format.Parse):
--Deprecated: "use "parseTimeOrError True" instead"
-- this error prevents using ghci
getTime :: String -> UTCTime
getTime = readTime defaultTimeLocale "%FT%T%Z"


--do deep search on the gpx file
atTag:: (ArrowXml a) => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)  -- search and if found the tag name, assign it to tag


text :: ArrowXml cat => cat (NTree XNode) String
text = getChildren >>> getText  --get the text value - used to get time and elevation values


--turn the elements into Haskell data, by using the features enabled by the Arrows extension
getTrkpt = atTag "trkpt" >>>
  proc x -> do
    time_ <- text <<< atTag "time" -< x   --the value of x is sent as an input to the arrow text, and matches its output against time_
    lon <- getAttrValue "lon" -< x
    lat <- getAttrValue "lat" -< x
    ele <- text <<< atTag "ele" -< x
    returnA -< Trkpt {
      time = getTime time_,
      longitude = read lon,
      latitude = read lat,
      elevation = read ele
      }

----get all the child elements(trkpt(lat,lon),ele,time) inside the root element(trkseg) 
getTrkseg = atTag "trkseg" >>>
  proc x -> do
    trksegments <- listA getTrkpt -< x
    returnA -< Trkseg trksegments

-- Haversine's formula for computing the length of a segment expressed in longitude/latitude.
-- From http://www.movable-type.co.uk/scripts/latlong.html
segmentLength :: Trkpt -> Trkpt -> Double
segmentLength point_a point_b = radius * c where
  radius = 6371 -- Earth's mean radius (km)
  deg2rad x = 2 * pi * x / 360
  lat1 = deg2rad $ latitude point_a
  lat2 = deg2rad $ latitude point_b
  dlat = lat2 - lat1
  dlon = deg2rad $ longitude point_b - longitude point_a
  a = (sin dlat/2)^2 + (sin dlon/2)^2 * cos lat1 * cos lat2
  c = 2 * atan2 (sqrt a) (sqrt (1-a))


timeDelta :: Trkpt -> Trkpt -> Double
timeDelta a b = realToFrac $ diffUTCTime (time b) (time a)  
  
--elevation
avgElev :: Trkpt -> Trkpt -> Double
avgElev elev = e where
  e = elevation

  
-- Length of track as (seconds, kms)
trackLength :: Trkseg -> (Double, Double, Double)
trackLength (Trkseg segs) = (timeLen, kmLen, elevlen) where
    kmLen = sum (map (uncurry segmentLength) segments)
    timeLen = sum (map (uncurry timeDelta) segments)
    elevlen = sum (map (uncurry avgElev) segments)
    segments = zip segs (tail segs)
    

formatTimeDeltaHMS :: Double -> String
formatTimeDeltaHMS s =
  show (floor $ s / 60 / 60) ++ ":" ++
  show (floor (s / 60) `mod` 60) ++ ":" ++
  show (floor s `mod` 60)

  
formatTimeDeltaMS :: Double -> String
formatTimeDeltaMS s = show (s * 3.6) --convert mps to kmh
  --show (floor $ s / 60) ++ ":" ++ show (floor s `mod` 60)

  
formatElevation:: Double -> String
formatElevation s = show (s)
  

parseGPX :: String -> IOStateArrow s b XmlTree
parseGPX file = readDocument [ withValidate yes, withRemoveWS yes] file

printOutput ::String ->IO ()
printOutput file = do
 trackSegs <- runX (parseGPX file >>> getTrkseg)
 let (seconds, lenKm, lenelev) = trackLength $ head trackSegs
 putStrLn (printf "Track distance (km):     %.2f" $ lenKm)
 putStrLn (printf "Track duration (h:m:s):  %s" $ formatTimeDeltaHMS seconds)
 putStrLn (printf "Average pace (km/hr):   %.4s" $ formatTimeDeltaMS (lenKm/seconds))
 putStrLn (printf "Elevation (m):   %s" $ formatElevation (lenelev)) -- this gives wrong result
 putStrLn (printf "\n")


repl :: Repl ()
repl = do
  minput <- getInputLine "Please enter input file - (path/filename) "
  case minput of
    Nothing -> outputStrLn "Quitting."
    Just input -> (liftIO $ printOutput input ) >> repl

main :: IO ()
main = runInputT defaultSettings repl
