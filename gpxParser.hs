{-# LANGUAGE Arrows #-}
import Text.XML.HXT.Core
import Data.Time(UTCTime, readTime, diffUTCTime)
import Data.Time.Format(defaultTimeLocale)
import Text.Printf (printf)
import Text.Printf

data Trkseg = Trkseg [Trkpt] deriving (Eq, Show)

--define data structure
data Trkpt = Trkpt {
  time :: UTCTime,
  longitude :: Double,
  latitude :: Double,
  elevation :: Double
  } deriving (Eq, Show)

-- derive time and timezone stuffs
readt :: String -> UTCTime
readt = readTime defaultTimeLocale "%FT%T%Z"

--do deep search on the gpx file
atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText

getTrkpt = atTag "trkpt" >>>
  proc x -> do
    time_ <- text <<< atTag "time" -< x
    lon <- getAttrValue "lon" -< x
    lat <- getAttrValue "lat" -< x
    ele <- text <<< atTag "ele" -< x
    returnA -< Trkpt {
      time = readt time_,
      longitude = read lon,
      latitude = read lat,
      elevation = read ele
      }

getTrkseg = atTag "trkseg" >>>
  proc x -> do
    segments <- listA getTrkpt -< x
    returnA -< Trkseg segments

-- Haversine's formula for computing the length of a segment expressed
-- in longitude/latitude.
--
-- From http://www.movable-type.co.uk/scripts/latlong.html
segmentLength :: Trkpt -> Trkpt -> Double
segmentLength pta ptb = r * c where
  r = 6371 -- Earth's mean radius
  deg2rad x = 2 * pi * x / 360
  lat1 = deg2rad $ latitude pta
  lat2 = deg2rad $ latitude ptb
  dlat = lat2 - lat1
  dlon = deg2rad $ longitude ptb - longitude pta
  a = (sin dlat/2)^2 + (sin dlon/2)^2 * cos lat1 * cos lat2
  c = 2 * atan2 (sqrt a) (sqrt (1-a))

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
    timeDelta :: Trkpt -> Trkpt -> Double
    timeDelta a b = realToFrac $ diffUTCTime (time b) (time a)
    
    

formatTimeDeltaHMS :: Double -> String
formatTimeDeltaHMS s =
  show (floor $ s / 60 / 60) ++ ":" ++
  show (floor (s / 60) `mod` 60) ++ ":" ++
  show (floor s `mod` 60)

formatTimeDeltaMS :: Double -> String
formatTimeDeltaMS s =
  show (floor $ s / 60) ++ ":" ++ show (floor s `mod` 60)

formatElevation:: Double -> String
formatElevation s =
  show (s)



--TODO  REPL (Read-Evaluation-Print Loop)

main :: IO ()
main =
  do
    trackSegs <- runX (readDocument [withValidate no] "test_files/test1.gpx" >>> getTrkseg)
    let (seconds, lenKm, lenelev) = trackLength $ head trackSegs
    putStrLn (printf "Track distance (km):     %f" lenKm)
    putStrLn (printf "Track duration (h:m:s):  %s" $ formatTimeDeltaHMS seconds)
    putStrLn (printf "Average pace (min/km):   %s" $ formatTimeDeltaMS (seconds / lenKm))
    putStrLn (printf "Above mean sea level (m):   %s" $ formatElevation (lenelev))

