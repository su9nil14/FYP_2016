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
import Data.Maybe
import System.IO
import Control.Monad
import Data.List


data Trkseg = Trkseg [Trkpt] 
            deriving (Eq, Ord, Show, Read)

---- a type for records
--data T = T { distance  :: Float
--           , duration :: String
--           , averagePace :: String }
--    deriving Show


type Latitude = Double
type Longitude = Double
type Elevation = Double
type Time = UTCTime
type Step = (Double, Double)

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


getRadianPair :: Trkpt -> (Double,Double)
getRadianPair p = (toRadians (latitude p), toRadians (longitude p))


toRadians :: Floating f => f -> f
toRadians = (*) (pi / 180)


-- radius of the earth in meters
radiusOfEarth :: Double
radiusOfEarth = 6371 -- Earth's mean radius (km) --6378700(m)


--get distance beetween two points
calcDistance :: Trkpt -> Trkpt -> Double
calcDistance x y =
  let (lat1,lon1) = getRadianPair x
      (lat2,lon2) = getRadianPair y
      deltaLat    = lat2 - lat1
      deltaLon    = lon2 - lon1
      a = (sin (deltaLat / 2))^(2::Int) + cos lat1 * cos lat2 * (sin (deltaLon / 2))^(2::Int)
      c = 2 * atan2 (a**0.5) ((1-a)**0.5)
  in radiusOfEarth * c


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


getElevation :: [Trkpt] -> [Elevation]
getElevation = map(\point -> elevation point)

--calculate difference between two elevation points
diffElevation :: Num a => [a] -> [a]
diffElevation (_:[]) = []
diffElevation (point:points) = (head points - point) : diffElevation points


--filter the positive values from the list
positiveClimb :: [Trkpt] -> [Elevation]
positiveClimb (_:[]) = []
positiveClimb points = positive where
  positive = (filter (>0) difference)
  difference = diffElevation (getElevation points)


totalClimb :: [Trkpt] -> Elevation
totalClimb points = (kmClimb) where
    kmClimb = sum (positiveClimb points)


--0.01 convert climb to flat
adjustedDistance :: Fractional a => a -> a -> a
adjustedDistance kmLen climbLen = (adjustedLen) where
  adjustedLen = kmLen + (climbLen * 0.01)


getSteepness :: Fractional a => a -> a -> a 
getSteepness climb dist = ((climb * 0.001) / (dist)) * 100


--calculate time between two segments and return a list	of times	
trackTime :: Trkseg -> [Double]
trackTime (Trkseg segs) = (timeLen) where
    timeLen = (map (uncurry timeDelta) segments)
    segments = (zip segs (tail segs))


--calculate distance between two segments and return a list of distances
trackDist :: Trkseg -> [Double]
trackDist (Trkseg segs) = (distLen) where
    distLen = (map (uncurry calcDistance) segments)
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


--gives a tuple of (lat,lon,time) --> (54.7148590087891,-8.00872611999512,2010-06-07 11:54:40)
latLonTimePoints :: [Trkpt] -> [(Latitude, Longitude, LocalTime)]
latLonTimePoints  = map (\point -> (latitude point, longitude point, utcToLocalTime diffTime(time point)))
   

--get tuple of Time,Elevation --> (day-month-year hour:minutes:seconds , double)
elevationTimePoints :: [Trkpt] -> [(LocalTime,Double)]
elevationTimePoints = map (\point -> (utcToLocalTime diffTime (time point) , elevation point))


getTimePoints :: [Trkpt] -> [(LocalTime)]
getTimePoints = map (\point -> (utcToLocalTime diffTime (time point)))


-- x=list  d=target distance  s=distance so far
splitAtTargetDistance :: [Double] -> Double -> [[Double]]
splitAtTargetDistance x d  = go x [] 0
     where go [] _ _ = []
           go (x:xs) acc s | x+s >= d  = (acc++[x]) : go xs [] 0
                           | otherwise = go xs (acc++[x]) (s+x)

                        
merge :: [Double] -> [Double] -> [Double ]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = (x : y : merge xs ys)


--get tuple of [(distance, time)]
trks :: Trkseg -> [Double]
trks points = merge dist tim where
  dist = trackDist points
  tim = trackTime points



phase1 d ((d0 , t0) : steps) = phase1' d d0 t0 [(d0, t0)] steps

phase1' d distSoFar timeSoFar stepsSoFar steps
         | distSoFar >= d = Just(timeSoFar, distSoFar)
         | otherwise = phase1 d ((distSoFar++distSoFar, timeSoFar++timeSoFar) : steps)



-- locate slowest and fast 1km segments in the data
--phase1 :: Double -> [Step] -> ([Step], [Step]) -> Maybe ([Step], [Step])
--phase1 d [] = Nothing
--phase1 d ((d0 , t0) : steps) = phase1' d d0 t0 [(d0, t0)] steps

--phase1' d distSoFar timeSoFar stepsSoFar steps
--         | distSoFar >= d = Just(timeSoFar, distSoFar)
--         | otherwise = phase1 d ((distSoFar++distSoFar, timeSoFar++timeSoFar) : steps)


 
localTime :: Trkpt -> LocalTime
localTime dteTime = utcToLocalTime diffTime (time dteTime)

--get the timezone
diffTime :: TimeZone
diffTime = TimeZone {timeZoneMinutes=0,timeZoneSummerOnly=False,timeZoneName="GMT"}


-- Length of track as (kms)
totalDistance :: Trkseg -> (Double)
totalDistance (Trkseg segs) = (kmLen) where
    kmLen = sum (map (uncurry calcDistance) segments)
    segments = zip segs (tail segs)


-- Length of track as (seconds)
trackDuration :: Trkseg -> (Double)
trackDuration (Trkseg segs) = (timeLen) where
    timeLen = sum (map (uncurry timeDelta) segments)
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

--formatDistance :: Double -> String
formatDistance x n = (fromIntegral (round (x * t))) / t
    where t = 10^n

--convert double to string for elevation
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


--write the summary to a text file
--writeData filename a b ap bp =
--  withFile filename WriteMode $ \h ->
--     let writeLine = hPrintf h $ "%." ++ show ap ++ "g\t%." ++ show bp ++ "s\n" in
--       zipWithM_ writeLine a b



--summarize data for gpx files
summarizeGPX :: String -> IO ()
summarizeGPX file = do
 trackSegs <- runX (parseGPX file >>> getTrkseg)
 trackPts <- runX (parseGPX file >>> getTrkpt)
--putStrLn (printf "trackpoints:  %s" $ show trackSegs)
 

 -- total distance and time of the track
 let (lenKm) = totalDistance $ head trackSegs
 let (seconds) = trackDuration $ head trackSegs
 putStrLn (printf "\n")
 putStrLn (printf "Track distance    (km)      :  %.2f" $ lenKm)
 putStrLn (printf "Track duration (h:m:s)      :  %s"   $ formatTimeDeltaHMS seconds)
 --putStrLn (printf "\n")
 

-- average minimum and maximum pace
 let pace = getPace $ head trackSegs
 putStrLn (printf "Average pace (mins/km)      :  %.5s" $ formatTimeDeltaMS (seconds/lenKm))
 putStrLn (printf "Maximum pace (mins/km)      :  %.5s" $ formatPace(maxPace pace))
 putStrLn (printf "Minimum pace (mins/km)      :  %.5s" $ formatPace(minPace pace))
 --putStrLn (printf "\n")
 

-- average minimum and maximum elevation
 let (avgelev, minelev, maxelev) = getAvgMinMaxElevation$ head [trackPts]
 putStrLn (printf "Average Elevation (MSL)     :  %s"   $ roundNumbers avgelev)
 putStrLn (printf "Minimum Elevation (MSL)     :  %s"   $ roundNumbers minelev)
 putStrLn (printf "Maximum Elevation (MSL)     :  %s"   $ roundNumbers maxelev)


 let (climb) = totalClimb $ head [trackPts]
 putStrLn (printf "Total Climb (m)             :  %s"   $ roundNumbers climb)


 let (adjustedKm) = adjustedDistance lenKm climb
 putStrLn (printf "Adjusted flat distance (km) :  %.2f" $ adjustedKm)
 

 let steepness = getSteepness climb lenKm
 putStrLn (printf "Steepness (percent)         :  %s"   $ roundNumbers steepness)


 --let allTracks = trackDist $ head trackSegs
 --let splittracks = splitAtTargetDistance allTracks
 --putStrLn (printf "Tracks  :  %s"  $ show splittracks)

 --writeFile "summary.txt" $ show $ lenKm
 --writeData "summary.txt" [lenKm] [formatTimeDeltaHMS seconds] 2 7

 --writeFile "summary.txt"  $ A.render id id id tableFormat
 --writeFile "sample1.tab"  $ S.render "\t" id id id tableFormat
 let distance = formatDistance lenKm 2 
 let paceval = formatTimeDeltaMS (seconds/lenKm)
 let duration = formatTimeDeltaHMS seconds
 let avgElevation = roundNumbers avgelev
 let totalclimb = roundNumbers climb
 writeFile "summary.txt"  $ "Distance\t" ++"Duration\t" ++ "Average Pace\t" ++ "Average Elevation\t" ++ "Total Climb\n" ++ 
   show distance ++ "\t\t| " ++duration ++ "\t| " ++ paceval ++ "\t\t| " ++avgElevation ++ "\t\t\t| " ++totalclimb



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