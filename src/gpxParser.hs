{-# LANGUAGE Arrows #-}

module GPXparser where

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
import Text.Tabular
import Text.Html
--import qualified Text.Tabular.AsciiArt as A
--import qualified Text.Tabular.SimpleText as S


type Latitude = Double
type Longitude = Double
type Elevation = Double
type Time = UTCTime
type Step = (Double
            , Double)

--type Pace = Double
--type Distance = Double
data Trkseg = Trkseg [Trkpt] 
            deriving (Eq, Ord, Show, Read)

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
textRead :: ArrowXml cat => cat (NTree XNode) String
textRead = getChildren >>> getText


--turn the elements into Haskell data, by using the features enabled by the Arrows extension
getTrkpt :: IOSLA (XIOState () ) XmlTree Trkpt
getTrkpt = atTag "trkpt" >>>
  proc x -> do
    time_ <- textRead <<< atTag "time" -< x   --the value of x is sent as an input to the arrow text, and matches its output against time_
    lon <- getAttrValue "lon" -< x
    lat <- getAttrValue "lat" -< x
    ele <- textRead <<< atTag "ele" -< x
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

----getNameGPX :: IOSLA (XIOState () ) XmlTree Name
--getNameGPX = atTag "trk" >>>
--  proc x -> do
--    name_ <- text <<< atTag "name" -< x   --the value of x is sent as an input to the arrow text, and matches its output against time_
--    returnA -< Trk {
--      name = read name_
--      }


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
trks :: Trkseg -> [(Step)]
trks points = zip dist (tim) where
  dist = trackDist points
  tim = trackTime points



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


--getGPXName :: Trk -> String
--getGPXName n = name n  


--summarize data for gpx files
summarizeGPX :: String -> IO ()
summarizeGPX file = do
 trackSegs <- runX (parseGPX file >>> getTrkseg)
 trackPts <- runX (parseGPX file >>> getTrkpt)
 --gpxName <- runX (parseGPX file >>> getNameGPX)
--putStrLn (printf "trackpoints:  %s" $ show trackSegs)
 

 -- total distance and time of the track
 --let name_gpx = getGPXName $ head gpxName
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
 putStrLn (printf "Avg Adjusted Pace (mins/km) :  %.5s" $ formatTimeDeltaMS (seconds/adjustedKm))
 

 let steepness = getSteepness climb lenKm
 putStrLn (printf "Steepness (percent)         :  %s"   $ roundNumbers steepness)

 let tuple = trks $ head trackSegs
 let bestseg = phase1 2 tuple
 putStrLn (printf "step         :  %s"   $ show bestseg)
 putStrLn (printf "--------------------------------------------------------")



--summarize data for gpx files
summarizeGPXDataToFile :: String-> IO ()
summarizeGPXDataToFile file = do
 trackSegs <- runX (parseGPX file >>> getTrkseg)
 trackPts <- runX (parseGPX file >>> getTrkpt)

 
 -- total distance and time of the track
 let (lenKm) = totalDistance $ head trackSegs
 let (seconds) = trackDuration $ head trackSegs
 let (climb) = totalClimb $ head [trackPts]

 let dateTime = getTimePoints $ head [trackPts]
 let firstdate = head dateTime
 let distance = formatDistance lenKm 2 
 let avgpaceval = formatTimeDeltaMS (seconds/lenKm)
 let duration = formatTimeDeltaHMS seconds
 let totalclimb = roundNumbers climb
 let adjustedDist = adjustedDistance lenKm climb
 let adjustedDistanceRounded = formatDistance adjustedDist 2
 let adjustedPace = formatTimeDeltaMS (seconds/adjustedDist)

 let finalStrings = tableFormat firstdate file distance duration avgpaceval totalclimb adjustedDistanceRounded adjustedPace

 putStrLn (printf $ finalStrings)
 writeFile "summary.txt" $ printf $ finalStrings
 --writeFile "summary.txt" $ finalStrings



--table :: (Show a, Show a1) => a -> [Char] -> [Char] -> [Char] -> a1 -> [Char] -> [Char]
tableFormat filename fd dt dr ap tc add adp=
  "Filename  ||DateTime\t\t||Distance  ||Duration  ||Average Pace  ||Total Climb   ||Adjusted Distance  ||Adjusted Pace\n"
  ++show fd++ "\t  ||"++show filename++ "\t||"++show dt++"\t    ||"++dr++"\t||"++ap++"\t\t||"++tc++"\t\t||"++show add++"\t\t     ||"++adp ++ 
   "\n------------------------------------------------------------------------------------------------------------------------------"




-- Takes all the Tracksegments and generates the HTML report
--generateHtmlPage :: Trkseg -> Html
generateHtmlPage file point segs = 
   let header1 = h1 $ stringToHtml ("GPX Summary Data "++file)

       title = thetitle $ stringToHtml ("GPX Summary Data"++file++"")
       theStyle = style (stringToHtml cssContent) ! [thetype "text/css"]
       theHeader = header $ concatHtml [title,theStyle]
       mainArea = thediv (concatHtml [header1,(statsTable point segs),br])
       theBody = body mainArea
   in concatHtml [theHeader,theBody,pageFooter]



--statsTable :: Trkseg -> Html --Date-Time, FileName Distance, Duration, Avg. Pace, Total Climb, Adj. Distance. Adj. Pace
statsTable point segs = 
   let tblHeader1 = th $ stringToHtml "Journey Details"
       tblHeader2 = th $ stringToHtml "Elevation Data"
       tblHeader3 = th $ stringToHtml "Pace Values"

       lenKm = totalDistance segs
       seconds = trackDuration segs
       climb = totalClimb point
       dateTime = getTimePoints point
       firstdate = head dateTime
       distance = formatDistance lenKm 2 
       avgpaceval = formatTimeDeltaMS (seconds/lenKm)
       duration = formatTimeDeltaHMS seconds
       totalclimb = roundNumbers climb
       adjustedDist = adjustedDistance lenKm climb
       adjustedDistanceRounded = formatDistance adjustedDist 2
       adjustedPace = formatTimeDeltaMS (seconds/adjustedDist)
       (avgelev, minelev, maxelev) = getAvgMinMaxElevation point

       dateCol1 = li $ stringToHtml ("Date & Time : "++ show firstdate)
       distCol1 = li $ stringToHtml ("Distance (km) : "++ show distance)
       durCol1 = li $ stringToHtml ("Duration (h:m:s): "++ duration)
       adjDistCol1 = li $ stringToHtml ("Adjusted Distance (km): "++ show adjustedDistanceRounded)

       elevCol1 = li $ stringToHtml ("Total Climb (m): "++ totalclimb)
       elevCol2 = li $ stringToHtml ("Maximum Elevation (m): "++ roundNumbers maxelev)
       elevCol3 = li $ stringToHtml ("Minimum Elevation (m): "++ roundNumbers minelev)
      
       paceCol31 = li $ stringToHtml ("Average Pace (mins/km): "++ avgpaceval)
       paceCol32 = li $ stringToHtml ("Adjusted Pace (mins/km): "++  adjustedPace)

       col1 = td (concatHtml [dateCol1,distCol1,durCol1,adjDistCol1]) ! [valign "top"]
       col2 = td  (concatHtml [elevCol1,elevCol2,elevCol3]) ! [valign "top"]
       col3 = td  (concatHtml [paceCol31,paceCol32]) ! [valign "top"]
       row1 = tr $ concatHtml [tblHeader1,tblHeader2,tblHeader3]
       row2 = tr $ concatHtml [col1,col2,col3]
       tbl = table (concatHtml [row1,row2]) -- ! [cellspacing 10]
   in center tbl

   
-- CSS style
cssContent = "h1 {color: #2C558A; font-weight: normal; font-size: x-large; "++
  "text-shadow: white 0px 1px 1px; letter-spacing: 0.1em; font-family: 'Gill Sans', "++
  "'PT Sans', 'Arial', sans-serif; text-transform: uppercase;} " ++
  "div {    width: 900px; margin-top: 50px; margin:0 auto;} "++
  "table {border-spacing: 20px 0px;} footer {text-align:right; "++
  "background-color:#EEEEEE; width:900px; margin:0 auto; margin-top: 30px} "


--The footer
pageFooter = 
   let projectLink = anchor (stringToHtml "Source files available at Github") ! [href "https://github.com/su9nil14/FYP_2016"]
       infoStr = stringToHtml "|Sunil Gautam|  "
   in footer (concatHtml [infoStr,projectLink]) ! [identifier "main"]


footer = tag "FOOTER"



writeHTML file = do
  [trackSegs] <- runX (parseGPX file >>> getTrkseg)
  trackPts <- runX (parseGPX file >>> getTrkpt)
  writeFile ("summary.html") (renderHtml $ generateHtmlPage file trackPts trackSegs)