{-# LANGUAGE Arrows #-}

module GPXparser where

import Text.XML.HXT.Core
import Data.Time(UTCTime,readTime, diffUTCTime)
import Data.Time.Format(defaultTimeLocale)
import Data.Text (Text)
import Text.Printf (printf)
import Text.Printf
import System.Environment
import Data.Tree.NTree.TypeDefs
--import Graphics.Gnuplot.Simple
--import System.Console.Haskeline
import System.Console.CmdArgs.Explicit
import System.Exit
import System.IO

--type Repl a = InputT IO a

data Trkseg = Trkseg [Trkpt] 
            deriving (Eq, Ord, Show, Read)

type Latitude = Double
type Longitude = Double

--define data structure - the child elements
data Trkpt = Trkpt
        { latitude  :: Latitude
         ,longitude :: Longitude
         ,elevation :: Double
         ,time        :: UTCTime
        }
         deriving (Eq, Ord, Show, Read)
		 
point :: Latitude -> Longitude -> Double  -> UTCTime -> Trkpt
point lt lg el tm = Trkpt lt lg el tm


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

--calculate time between two trackpoints
timeDelta :: Trkpt -> Trkpt -> Double
timeDelta a b = realToFrac $ diffUTCTime (time b) (time a)  
  
--calculate  the average elevation from the list of elevation
averageElevation :: [Trkpt] -> Double
averageElevation points = 
            let elevationVals = map ( elevation) points
                totalElevation = foldr (+) 0.0 elevationVals
                theMean = totalElevation / fromIntegral (length points)
            in theMean

--calculate  the minimum elevation from the list of elevation
minElevation :: [Trkpt] -> Double
minElevation points = 
            let elevationVals = map ( elevation) points
                theMin = minimum elevationVals
            in theMin

--calculate  the maximum elevation from the list of elevation
maxElevation :: [Trkpt] -> Double
maxElevation points = 
            let elevationVals = map (elevation) points
                theMax = maximum elevationVals
            in theMax
			
trackTime :: Trkseg -> [Double]
trackTime (Trkseg segs) = (timeLen) where
    timeLen = (map (uncurry timeDelta) segments)
    segments = (zip segs (tail segs))
	
trackDist :: Trkseg -> [Double]
trackDist (Trkseg segs) = (distLen) where
    distLen = (map (uncurry segmentLength) segments)
    segments = (zip segs (tail segs))

--get pace values for each segments	
divLists :: [Double] -> [Double] -> [Double]
divLists xs [] = xs
divLists [] ys = ys
divLists (x:xs) (y:ys) = (x / y : divLists xs ys)

maxPace :: [Double] -> Double
maxPace points = maximum(points)

minPace :: [Double] -> Double
minPace points = minimum(points)

-- Length of track as (seconds, kms)
trackLength :: Trkseg -> (Double, Double)
trackLength (Trkseg segs) = (timeLen, kmLen) where
    timeLen = sum (map (uncurry timeDelta) segments)
    kmLen = sum (map (uncurry segmentLength) segments)
    segments = zip segs (tail segs)
    
--format time in hour : minute: second 
formatTimeDeltaHMS :: Double -> String
formatTimeDeltaHMS s =
  show (floor $ s / 60 / 60) ++ ":" ++
  show (floor (s / 60) `mod` 60) ++ ":" ++
  show (floor s `mod` 60)

  
-- output as mins/km  
formatTimeDeltaMS :: Double -> String
formatTimeDeltaMS s = show (floor $ s / 60) ++ ":" ++ show (floor s `mod` 60)

--convert double to string for elevation
formatElevation:: Double -> String
formatElevation s = show (s)

--convert double to string for min/max pace
formatPace:: Double -> String
formatPace s =  show (floor $ s / 60) ++ ":" ++ show (floor s `mod` 60)
  
--read the gpx file
parseGPX :: String -> IOStateArrow s b XmlTree
parseGPX file = readDocument [ withValidate yes, withRemoveWS yes] file


--summarize data for gpx files
summarizeGPX ::String ->IO ()
summarizeGPX file = do
 trackSegs <- runX (parseGPX file >>> getTrkseg)
 trackPts <- runX (parseGPX file >>> getTrkpt)
 
 let (seconds, lenKm) = trackLength $ head trackSegs
 putStrLn (printf "\n")
 putStrLn (printf "Track distance (km):  %.2f" $ lenKm)
 putStrLn (printf "Track duration (h:m:s):  %s" $ formatTimeDeltaHMS seconds)
 putStrLn (printf "Average pace (mins/km):  %.4s" $ formatTimeDeltaMS (seconds/lenKm))

 let (avgelev) = averageElevation $ head [trackPts]
 let (minelev) = minElevation $ head [trackPts]
 let (maxelev) = maxElevation $ head [trackPts]
 putStrLn (printf "Average Elevation:  %.6s" $ formatElevation avgelev)
 putStrLn (printf "Minimum Elevation:  %.6s" $ formatElevation minelev)
 putStrLn (printf "MaximumElevation:  %.6s" $ formatElevation maxelev)
 
 let (timeTrack) = trackTime $ head trackSegs
 let (distTrack) = trackDist $ head trackSegs
 let pace = divLists timeTrack distTrack

 putStrLn (printf "\n")
 --putStrLn (printf "Maximum pace :  %s" $ formatPace(timeTrack))
 putStrLn (printf "Maximum pace :  %s" $ formatPace(maxPace pace))
 putStrLn (printf "Minimum pace :  %s" $ formatPace(minPace pace))
 putStrLn (printf "\n")


  