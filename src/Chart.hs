{-# LANGUAGE Arrows #-}
--Drawing chart in png format. Got help from example 8 shown on https://github.com/timbod7/haskell-chart/wiki/example%208

module Chart where

import GPXparser
import Data.Maybe
import Data.Time(UTCTime)
import Text.Printf
import Graphics.Rendering.Chart
import Data.Accessor
import Data.Colour
import Data.Colour.Names
import Data.Time.LocalTime
import Text.XML.HXT.Core
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour.SRGB
import Data.Default.Class
import Control.Lens



--Takes all trackpoints and an Eq function and then returns a single trackpoint containing time and (elevation point)
--more info on eq function --https://www.haskell.org/tutorial/classes.html
getPoint :: [Trackpoint] -> Trackpoint -> (Trackpoint -> Double) -> (Double -> Double -> Bool) -> Maybe(LocalTime,Double)
getPoint [] _ _ _ = Nothing
getPoint (point:points) currSelected trkPointElement equalityF  --eq function
   | equalityF (trkPointElement point) (trkPointElement currSelected) = getPoint points point trkPointElement equalityF
   | otherwise = if null points
                      then Just(localTime currSelected, elevation currSelected)
                      else getPoint points currSelected trkPointElement equalityF


printpoint :: Maybe(LocalTime,Double) -> String
printpoint s = show $ fromJust s


--get distance,elevation and time and plot the points to chart
distanceElevationOverTimeChart :: [Trackpoint] -> Renderable ()
distanceElevationOverTimeChart points = toRenderable layout
 where

  layout = layoutlr_title .~ "Distance && Elevation vs Time"
         $ layoutlr_background .~ solidFillStyle bg
         $ layoutlr_right_axis . laxis_override .~ axisGridHide
         $ layoutlr_right_axis . laxis_title .~ "Distance (km)"
         $ layoutlr_left_axis . laxis_override .~ axisGridHide
         $ layoutlr_left_axis . laxis_title .~ "Elevation (m)"
         $ layoutlr_plots  .~ [ Right (toPlot dist), Left (toPlot elev),Left (toPlot spots)]
         $ layoutlr_grid_last .~ False
         $ def


  elev = plot_lines_style  .~ lineStyle
         $ plot_lines_values .~ [[ (theTime,elevation) | (theTime,elevation) <- elevationTimePoints points]]
         $ plot_lines_title .~ "Elevation"
         $ def

  
  spots = area_spots_title .~ "Elevation(Max && Min)"
        $ area_spots_max_radius .~ 15
        $ area_spots_values .~ (if  isJust spotMinElev && isJust spotMaxElev then [fromJust spotMinElev,fromJust spotMaxElev] else [])
        $ def

  dist = plot_fillbetween_style .~ solidFillStyle fillC
         $ plot_fillbetween_values .~  [(theTime,(0.0,distance)) | (theTime,distance) <- timeOverDistance points 0.0]
         $ plot_fillbetween_title .~ "Distance"
         $ def


  spotMaxElev = let point = getPoint points (head points) elevation (>) 
                 in (if isJust point then Just (fst $ fromJust point, snd $ fromJust point, 5 :: Double) else Nothing)

  spotMinElev = let point = getPoint points (head points) elevation (<)
                 in (if isJust point then Just (fst $ fromJust point, snd $ fromJust point, 5 :: Double) else Nothing)

  lineStyle = line_width .~ 3 * 0.50
            $ line_color .~ opaque blue
            $ def

  bg = opaque $ white
  fillC = opaque $ yellow
  blue1 = opaque $ sRGB 0.5 0.5 1



paceDistanceOverTimeChart :: [Trackpoint]->Tracksegment -> Renderable ()
paceDistanceOverTimeChart point points = toRenderable layout
  where

  layout = layoutlr_title .~ "Distance && Pace vs Time"
         $ layoutlr_background .~ solidFillStyle bg
         $ layoutlr_right_axis . laxis_override .~ axisGridHide
         $ layoutlr_right_axis . laxis_title .~ "Distance (km)"
         $ layoutlr_left_axis . laxis_override .~ axisGridHide
         $ layoutlr_left_axis . laxis_title .~ "Pace (mins/km)"
         $ layoutlr_plots  .~ [Right(toPlot dist), Left (toPlot pace)]
         $ layoutlr_grid_last .~ False
         $ def


  dist = plot_fillbetween_style .~ solidFillStyle fillC
         $ plot_fillbetween_values .~  [(theTime,(0.0,distance)) | (theTime,distance) <- timeOverDistance point 0.0]
         $ plot_fillbetween_title .~ "Distance"
         $ def


  pace = plot_lines_style  .~ lineStyle
         $ plot_lines_values .~ [[ (theTime,paceVal) | (theTime,paceVal) <- paceTime point points]]
         $ plot_lines_title .~ "Pace"
         $ def

  lineStyle = line_width .~ 3 * 0.50
            $ line_color .~ opaque blue
            $ def

  bg = opaque $ white
  fillC = opaque $ yellow
  blue1 = opaque $ sRGB 0.5 0.5 1



paceTime :: [Trackpoint] -> Tracksegment -> [(LocalTime, Double)]
paceTime point points = zip (tim) (pace) where
  tim = getTimePoints point
  pace = getPace points
  

drawChart :: [Trackpoint] -> FilePath -> IO (PickFn ())
drawChart points filename = do
  renderableToFile def (filename++"1") (distanceElevationOverTimeChart points)


drawChart2 :: [Trackpoint] ->Tracksegment-> FilePath -> IO (PickFn ())
drawChart2 point points filename = do
  renderableToFile def (filename++"2") (paceDistanceOverTimeChart point points)


printpoint2 :: [(LocalTime,Double)] -> String
printpoint2 s = show $ s


--main = do
--  [trackSegs] <- runX (parseGPX "3.gpx" >>> getTrkseg)
--  trackPts <- runX (parseGPX "3.gpx" >>> getTrkpt)
--  --drawChart trackPts "chart.png"
--  drawChart2 trackPts trackSegs "chart2.png"

  
 