{-# LANGUAGE Arrows #-}

module GPXparser where

import Text.XML.HXT.Core
import Data.Time(UTCTime, diffUTCTime)
import Data.Time.LocalTime
import Data.Time.Format(defaultTimeLocale, parseTimeOrError)
import Text.Printf
import Data.Tree.NTree.TypeDefs
--import Graphics.Gnuplot.Simple
--import Graphics.EasyPlot

--import Data.Time.Clock
--import Data.Time.Calendar
--import Text.XML.XSD.DateTime


data Trkseg = Trkseg [Trkpt] 
            deriving (Eq, Ord, Show, Read)


type Latitude = Double
type Longitude = Double
type Elevation = Double
type Time = UTCTime

--type Pace = Double
--type Distance = Double

--define data structure - the child elements
data Trkpt = Trkpt
        { latitude  :: Latitude
         ,longitude :: Longitude
         ,elevation :: Elevation
         ,time      :: Time
        }
         deriving (Eq, Ord, Show, Read)


  -- derive time and timezone stuffs 
getTime :: String -> UTCTime
getTime = parseTimeOrError True defaultTimeLocale "%FT%T%Z"


--do deep search on the gpx file
atTag:: (ArrowXml a) => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)  -- search and if found the tag name, assign it to tag


--get the text value - used to get time and elevation values
text :: ArrowXml cat => cat (NTree XNode) String
text = getChildren >>> getText


--turn the elements into Haskell data, by using the features enabled by the Arrows extension
getTrkpt :: IOSLA (XIOState () ) XmlTree Trkpt
getTrkpt = atTag "trkpt" >>>
  proc x -> do
    time_ <- text <<< atTag "time" -< x   --the value of x is sent as an input to the arrow text, and matches its output against time_
    lon <- getAttrValue "lon" -< x
    lat <- getAttrValue "lat" -< x
    ele <- text <<< atTag "ele" -< x
    returnA -< Trkpt {
      latitude = read lat,
      longitude = read lon,
      elevation = read ele,
      time = getTime time_
      }


----get all the child elements(trkpt(lat,lon),ele,time) inside the root element(trkseg)
getTrkseg:: IOSLA (XIOState () ) XmlTree Trkseg 
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

getAvgMinMaxElevation :: [Trkpt] -> (Double, Double, Double)
getAvgMinMaxElevation points = (avgElev, minElev, maxElev) where
  avgElev = averageElevation points
  minElev = minElevation points
  maxElev = maxElevation points


--calculate time between two segments and return a list	of times	
trackTime :: Trkseg -> [Double]
trackTime (Trkseg segs) = (timeLen) where
    timeLen = (map (uncurry timeDelta) segments)
    segments = (zip segs (tail segs))


--calculate distance between two segments and return a list of distances
trackDist :: Trkseg -> [Double]
trackDist (Trkseg segs) = (distLen) where
    distLen = (map (uncurry segmentLength) segments)
    segments = (zip segs (tail segs))


--get pace values for each segments	-- time / distance = pace
divLists :: [Double] -> [Double] -> [Double]
divLists xs [] = xs
divLists [] ys = ys
divLists (x:xs) (y:ys) = (x / y : divLists xs ys)


-- use divlist to get the pace for each trackpoints
getPace :: Trkseg -> [Double]
getPace pts = pace where
 pace = divLists t d 
 t = trackTime pts --t = time
 d = trackDist pts --d = distance


--maximum pace in each segments
maxPace :: [Double] -> Double
maxPace points = minimum(points)


--minimum pace in each segments
minPace :: [Double] -> Double
minPace points = maximum( filter (<1000) points)

--creates a list of tuples containing (time,average_speed_at_this_time)
--averageSpeedOverTime :: [Trkpt] -> Double -> Double -> [(LocalTime, Double)] -> [(LocalTime,Double)]
--averageSpeedOverTime [] _ _ _ = []
--averageSpeedOverTime [spd] totalSpeed numPoints iteratedAvr = iteratedAvr ++ [(fst spd, (snd spd + totalSpeed) / (numPoints+1))]
--averageSpeedOverTime (spd:spds) totalSpeed numPoints iteratedAvr = averageSpeedOverTime [spd] totalSpeed numPoints iteratedAvr ++ 
-- averageSpeedOverTime spds (snd spd + totalSpeed) (numPoints+1) iteratedAvr

--may need this later on
partitionSeg :: Int -> [a] -> [[a]]
partitionSeg _ [] = []
partitionSeg n xs = (take n xs) : (partitionSeg n (drop n xs))




-- locate slowest and fast 1km segments in the data


--gives a tuple of (lat,lon,time) --> (54.7148590087891,-8.00872611999512,2010-06-07 11:54:40)
latLonTimePoints :: [Trkpt] -> [(Latitude, Longitude, LocalTime)]
latLonTimePoints  = map (\point -> (latitude point, longitude point, utcToLocalTime diffTime(time point)))
   

--get tuple of Time,Elevation --> (day-month-year hour:minutes:seconds , double)
elevationTimePoints :: [Trkpt] -> [(LocalTime,Double)]
elevationTimePoints = map (\point -> (utcToLocalTime diffTime (time point) , elevation point))

--get the timezone
diffTime :: TimeZone
diffTime = TimeZone {timeZoneMinutes=0,timeZoneSummerOnly=False,timeZoneName="GMT"}



-- Length of track as (seconds, kms)
trackLength :: Trkseg -> (Double, Double)
trackLength (Trkseg segs) = (timeLen, kmLen) where
    timeLen = sum (map (uncurry timeDelta) segments)
    kmLen = sum (map (uncurry segmentLength) segments)
    segments = zip segs (tail segs)


--format time in hour : minute: second 
formatTimeDeltaHMS :: Double -> String
formatTimeDeltaHMS s =
  show (round $ s / 60 / 60) ++ ":" ++
  show (round (s / 60) `mod` 60) ++ ":" ++
  show (round s `mod` 60)

  
-- output as mins/km  
formatTimeDeltaMS :: Double -> String
formatTimeDeltaMS s = show ( round $ s / 60) ++ ":" ++ show (round s `mod` 60)


--convert double to string for elevation
formatElevation:: Double -> String
formatElevation s = show (round $ s)


--convert (time,double) to string for elevation over time
formatElevationOverTimePoints :: [(LocalTime,Double)] -> String
formatElevationOverTimePoints s = show (s)


--convert double to string for min/max pace
formatPace:: Double -> String
formatPace s =  show ( round $ s / 60) ++ ":" ++ show (round s `mod` 60)


--read the gpx file
parseGPX :: String -> IOStateArrow s b XmlTree
parseGPX file = readDocument [ withValidate yes, withRemoveWS yes] file


--summarize data for gpx files
summarizeGPX :: String -> IO ()
summarizeGPX file = do
 trackSegs <- runX (parseGPX file >>> getTrkseg)
 trackPts <- runX (parseGPX file >>> getTrkpt)
--putStrLn (printf "trackpoints:  %s" $ show trackSegs)
 

 -- total distance and time of the track
 let (seconds, lenKm) = trackLength $ head trackSegs
 putStrLn (printf "\n")
 putStrLn (printf "Track distance    (km) :  %.2f" $ lenKm)
 putStrLn (printf "Track duration (h:m:s) :  %s"   $ formatTimeDeltaHMS seconds)
 --putStrLn (printf "\n")
 

-- average minimum and maximum pace
 let pace = getPace $ head trackSegs
 putStrLn (printf "Average pace (mins/km) :  %.5s" $ formatTimeDeltaMS (seconds/lenKm))
 putStrLn (printf "Maximum pace (mins/km) :  %.5s" $ formatPace(maxPace pace))
 putStrLn (printf "Minimum pace (mins/km) :  %.5s" $ formatPace(minPace pace))
 --putStrLn (printf "\n")
 

-- average minimum and maximum elevation
 let (avgelev, minelev, maxelev) = getAvgMinMaxElevation $ head [trackPts]
 putStrLn (printf "Average Elevation(MSL) :  %s"   $ formatElevation avgelev)
 putStrLn (printf "Minimum Elevation(MSL) :  %s"   $ formatElevation minelev)
 putStrLn (printf "Maximum Elevation(MSL) :  %s"   $ formatElevation maxelev)
 putStrLn (printf "--------------------------------------------------------")



 --let (points) = latLonTimePoints $ head [trackPts]
 --putStrLn (printf "Points:  %s" $ show (points)) 

 --let (ptEle) = elevationTimePoints $ head [trackPts]
 --putStrLn (printf "distance Elevation points:  %s" $ formatElevationOverTimePoints ptEle)


--experiments with plotting
--plotElevationOverTime :: String -> Graph2D ()
--plotElevationOverTime file = do
--trackPts <- runX (parseGPX file >>> getTrkpt)

--let dataList =  ptEle
--putStrLn (printf "distance Elevation points:  %s" $ formatElevationPoints ptEle)
--plot X11 $ Data2D [Title "Sample Data" Fractional LocalTime] [] dataList