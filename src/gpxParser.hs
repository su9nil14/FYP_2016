{-# LANGUAGE Arrows #-}

module GPXparser where

--Credit to Janne Hellsten(nurpax)
--This code is adapted and modified from example(Reading TCX in Haskell) found at http://nurpax.github.io/
-- using hxt pacakage from http://www.fh-wedel.de/~si/HXmlToolbox/index.html to parse the gpx files

import Text.XML.HXT.Core 
import Data.Time(UTCTime, diffUTCTime)
import Data.Time.LocalTime
import Data.Time.Format(defaultTimeLocale, parseTimeOrError)
import Text.Printf
import Data.Tree.NTree.TypeDefs
import Data.Maybe
import System.IO
import Control.Monad
import Data.List


type Latitude = Double
type Longitude = Double
type Elevation = Double
type Time = UTCTime
type Step = (Double
            , Double)


---------------------------------------------------------Data Types stuffs-----------------------------------------------------
data Tracksegment = Tracksegment [Trackpoint] 
            deriving (Eq, Ord, Show, Read)

--define data structure - the child elements
data Trackpoint = Trackpoint
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
atTag tag = deep (isElem >>> hasName tag)  -- search for the tag in the tree


--get the text value - used for getting time and elevation values
textRead :: ArrowXml cat => cat (NTree XNode) String
textRead = getChildren >>> getText


--turn the elements into Haskell data, by using the features enabled by the Arrows extension
getTrkpt :: IOSLA (XIOState () ) XmlTree Trackpoint
getTrkpt = atTag "trkpt" >>>
  proc x -> do
    time_ <- textRead <<< atTag "time" -< x   --the value of x is sent as an input to the arrow text, and matches its output against time_
    lon <- getAttrValue "lon" -< x
    lat <- getAttrValue "lat" -< x
    ele <- textRead <<< atTag "ele" -< x
    returnA -< Trackpoint {
      latitude = read lat,
      longitude = read lon,
      elevation = read ele,
      time = getTime time_
      }


----get all the child elements(trkpt(lat,lon),ele,time) inside the root element(trkseg)
getTrkseg:: IOSLA (XIOState () ) XmlTree Tracksegment 
getTrkseg = atTag "trkseg" >>>
  proc x -> do
    segments <- listA getTrkpt -< x
    returnA -< Tracksegment segments


--------------------------------------------------Calculate distance stuffs------------------------------------------------------

--get distance beetween two points using Haversine's formula
--credit to From http://www.movable-type.co.uk/scripts/latlong.html
calcDistance :: Trackpoint -> Trackpoint -> Double
calcDistance x y =
  let (lat1,lon1) = getRadianPair x
      (lat2,lon2) = getRadianPair y
      deltaLat    = lat2 - lat1
      deltaLon    = lon2 - lon1
      a = (sin (deltaLat / 2))^(2::Int) + cos lat1 * cos lat2 * (sin (deltaLon / 2))^(2::Int)
      c = 2 * atan2 (a**0.5) ((1-a)**0.5)
  in radiusOfEarth * c

getRadianPair :: Trackpoint -> (Double,Double)
getRadianPair p = (toRadians (latitude p), toRadians (longitude p))

toRadians :: Floating f => f -> f
toRadians = (*) (pi / 180)

-- radius of the earth in meters
radiusOfEarth :: Double
radiusOfEarth = 6371 -- Earth's mean radius (km) --6378700(m)


--0.01 convert climb to flat
adjustedDistance :: Fractional a => a -> a -> a
adjustedDistance kmLen climbLen = (adjustedLen) where
  adjustedLen = kmLen + (climbLen * 0.01)


--calculate distance between two trackpoints of a tracksegment and return a list of distances
trackDist :: Tracksegment -> [Double]
trackDist (Tracksegment segs) = (distLen) where
    distLen = (map (uncurry calcDistance) segments)
    segments = (zip segs (tail segs))


-- Total Length of track as (kms) --total distance walked
totalDistance :: Tracksegment -> (Double)
totalDistance (Tracksegment segs) = (kmLen) where
    kmLen = sum (map (uncurry calcDistance) segments)
    segments = zip segs (tail segs)
----------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------Calculate time stuffs-----------------------------------------------------------
--calculate time (difference) between two points
timeDelta :: Trackpoint -> Trackpoint -> Double
timeDelta a b = realToFrac $ diffUTCTime (time b) (time a)


--calculate time between two points and return a list of times  
trackTime :: Tracksegment -> [Double]
trackTime (Tracksegment segs) = (timeLen) where
    timeLen = (map (uncurry timeDelta) segments)
    segments = (zip segs (tail segs))



--convert UTC time to localtime -- convert D/M/Y H:M:S UTC -to- D/M/Y H:M:S
localTime :: Trackpoint -> LocalTime
localTime dteTime = utcToLocalTime diffTime (time dteTime)


--get the timezone
diffTime :: TimeZone
diffTime = TimeZone {timeZoneMinutes=0,timeZoneSummerOnly=False,timeZoneName="GMT"}



--------------------------------------------------Calculate elevation stuffs-----------------------------------------------------------
getElevation :: [Trackpoint] -> [Elevation]
getElevation = map(\point -> elevation point)


--calculate  the average elevation from the list of elevation
averageElevation :: [Trackpoint] -> Double
averageElevation points = 
            let elevationVals = map ( elevation) points
                totalElevation = foldr (+) 0.0 elevationVals
                theMean = totalElevation / fromIntegral (length points)
            in theMean


--calculate  the minimum elevation from the list of elevation
minElevation :: [Trackpoint] -> Double
minElevation points = 
            let elevationVals = map ( elevation) points
                theMin = minimum elevationVals
            in theMin


--calculate  the maximum elevation from the list of elevation
maxElevation :: [Trackpoint] -> Double
maxElevation points = 
            let elevationVals = map (elevation) points
                theMax = maximum elevationVals
            in theMax


--get average,min and max elevation at once
getAvgMinMaxElevation :: [Trackpoint] -> (Double, Double, Double)
getAvgMinMaxElevation points = (avgElev, minElev, maxElev) where
  avgElev = averageElevation points
  minElev = minElevation points
  maxElev = maxElevation points


--calculate difference between two elevation points
diffElevation :: Num a => [a] -> [a]
diffElevation (_:[]) = []
diffElevation (point:points) = (head points - point) : diffElevation points


--filter the positive values from the list
positiveClimb :: [Trackpoint] -> [Elevation]
positiveClimb (_:[]) = []
positiveClimb points = positive where
  positive = (filter (>0) difference)
  difference = diffElevation (getElevation points)


totalClimb :: [Trackpoint] -> Elevation
totalClimb points = (kmClimb) where
    kmClimb = sum (positiveClimb points)


getSteepness :: Fractional a => a -> a -> a 
getSteepness climb dist = ((climb * 0.001) / (dist)) * 100



---------------------------------------------------------------Calculate pace stuffs-----------------------------------------------------

-- use divlist to get the pace for each trackpoints
getPace :: Tracksegment -> [Double]
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


-- locate slowest and fast 1km segments in the data
phase1 :: Double -> [Step] -> Maybe ([Step], [Step])
phase1 d [] = Nothing
phase1 d ((d0,t0):steps) = phase1' d d0 t0 [(d0,t0)] steps

phase1' :: Double -> Double -> Double -> [Step] -> [Step] -> Maybe ([Step],[Step])
phase1' d currD currT walked []
 | currD < d = Nothing
 | otherwise = Just (walked,[])
phase1' d currD currT walked ((dx,tx):steps)
 | currD' < d = phase1' d currD' currT' walked' steps
 | otherwise = Just (walked',steps)
 where
   currD' = currD + dx
   currT' = currT + tx
   walked' = walked++[(dx,tx)]


-------------------------------------------------------------------Helper functions------------------------------------------------
--get pace values for each segments -- time / distance = pace
divLists :: [Double] -> [Double] -> [Double]
divLists xs [] = xs
divLists [] ys = ys
divLists (x:xs) (y:ys) = (x / y : divLists xs ys)


--gives a tuple of (lat,lon,time) --> (54.7148590087891,-8.00872611999512,2010-06-07 11:54:40)
latLonTimePoints :: [Trackpoint] -> [(Latitude, Longitude, LocalTime)]
latLonTimePoints  = map (\point -> (latitude point, longitude point, utcToLocalTime diffTime(time point)))
   

--get tuple of Time,Elevation --> (day-month-year hour:minutes:seconds , double)
elevationTimePoints :: [Trackpoint] -> [(LocalTime,Double)]
elevationTimePoints = map (\point -> (utcToLocalTime diffTime (time point) , elevation point))


getTimePoints :: [Trackpoint] -> [(LocalTime)]
getTimePoints = map (\point -> (localTime(point)))


-- x=list  d=target distance  s=distance so far
splitAtTargetDistance :: [Double] -> Double -> [[Double]]
splitAtTargetDistance x d  = go x [] 0
     where go [] _ _ = []
           go (x:xs) acc s | x+s >= d  = (acc++[x]) : go xs [] 0
                           | otherwise = go xs (acc++[x]) (s+x)

--function which merges two list together                        
merge :: [Double] -> [Double] -> [Double ]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = (x : y : merge xs ys)


--get tuple of [(distance, time)]
trks :: Tracksegment -> [(Double, Double)]
trks points = zip dist (tim) where
  dist = trackDist points
  tim = trackTime points


--alternative to trks above
--get time over distance  (datetime,distance_travelled_in_that_time)
timeOverDistance :: [Trackpoint] -> Double -> [(LocalTime,Double)]
timeOverDistance [] _ = []
timeOverDistance [x] _ = [(localTime(x),0.0)]
timeOverDistance (x:xs) acc = 
   let dist = calcDistance x (head xs)           
   in (localTime(x), dist + acc ) : timeOverDistance xs (dist + acc)


-- Length of track as (seconds)
trackDuration :: Tracksegment -> (Double)
trackDuration (Tracksegment segs) = (timeLen) where
    timeLen = sum (map (uncurry timeDelta) segments)
    segments = zip segs (tail segs)


--format time in hour : minute: second 
formatTimeHMS :: Double -> String
formatTimeHMS s =
  show (round $ s / 60 / 60) ++ ":" ++
  show (round (s / 60) `mod` 60) ++ ":" ++
  show (round s `mod` 60)

  
-- output as mins/km  
formatTimeDeltaMS :: Double -> String
formatTimeDeltaMS s = show ( round $ s / 60) ++ ":" ++ show (round s `mod` 60)


--round doubles to given decimal places
formatDistance :: (Integral b, RealFrac a) => a -> b -> a
formatDistance x n = (fromIntegral (round (x * t))) / t
    where t = 10^n


--round doubles to string--used for elevation data
roundNumbers:: Double -> String
roundNumbers s = show (round $ s)


--convert (time,double) to string for elevation over time
formatElevationOverTimePoints :: [(LocalTime,Double)] -> String
formatElevationOverTimePoints s = show (s)


--convert double to string for min/max pace
formatPace:: (Double) -> String
formatPace s =  show ( round $ s / 60) ++ ":" ++ show (round s `mod` 60)


--read the gpx file
parseGPX :: String -> IOStateArrow s b XmlTree
parseGPX file = readDocument [ withValidate yes, withRemoveWS yes] file


-- Length of track as (seconds)
--tracks :: Tracksegment -> (Double, Double)
tracks segs = zip dist (tim) where
  dist = trackDist segs
  tim = trackTime segs


--main :: IO ()
main = do
  --[trackSegs] <- runX (parseGPX "eg.gpx" >>> getTrkseg)
  trackPts <- runX (parseGPX "eg.gpx" >>> getTrkpt)

  let [trkss] = timeOverDistance trackPts 0.0
  putStrLn("tracks" ++ show trkss)


