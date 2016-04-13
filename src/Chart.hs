{-# LANGUAGE Arrows #-}

module Chart where

import GPXparser
import Data.Maybe
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

green1 = opaque $ sRGB 0.5 1 0.5
blue1 = opaque $ sRGB 0.5 0.5 1

data OutputType = Window | PNG | PS | PDF | SVG



getPoint :: [Trackpoint] -> Trackpoint -> (Trackpoint -> Double) -> (Double -> Double -> Bool) -> Maybe(LocalTime,Double)
getPoint [] _ _ _ = Nothing
getPoint (point:points) currSelected trkPointElement equalityF
   | equalityF (trkPointElement point) (trkPointElement currSelected) = getPoint points point trkPointElement equalityF
   | otherwise = if null points
                      then Just(localTime currSelected, elevation currSelected)
                      else getPoint points currSelected trkPointElement equalityF


printpoint :: Maybe(LocalTime,Double) -> String
printpoint s = show $ fromJust s


distanceElevationOverTimeChart :: [Trackpoint] -> Renderable ()
distanceElevationOverTimeChart points = toRenderable layout
 where


  layout = layout_title .~ "Distance vs Elevation"
         $ layout_grid_last .~ True
         $ layout_plots .~ [toPlot dist, toPlot elev, toPlot spots]
         $ def


  dist = plot_fillbetween_style .~ solidFillStyle green1
         $ plot_fillbetween_values .~  [(theTime,(0,distance)) | (theTime,distance) <- timeOverDistance points 0.0]
         $ plot_fillbetween_title .~ "Distance"
         $ def

  elev = plot_lines_style  .~ lineStyle
         $ plot_lines_values .~ [[ (theTime,elevation) | (theTime,elevation) <- elevationTimePoints points]]
         $ plot_lines_title .~ "Elevation"
         $ def

  
  spots = area_spots_title .~ "Elevation(Max && Min)"
        $ area_spots_max_radius .~ 15
        $ area_spots_values .~ (if  isJust spotMinElev && isJust spotMaxElev then [fromJust spotMinElev,fromJust spotMaxElev] else [])
        $ def


  spotMaxElev = let point = getPoint points (head points) elevation (>) 
                 in (if isJust point then Just (fst $ fromJust point, snd $ fromJust point, 5 :: Double) else Nothing)

  spotMinElev = let point = getPoint points (head points) elevation (<)
                 in (if isJust point then Just (fst $ fromJust point, snd $ fromJust point, 5 :: Double) else Nothing)

  lineStyle = line_width .~ 3 * 0.25
            $ line_color .~ opaque blue
            $ def


--drawChart :: [Trackpoint] -> FilePath -> IO (PickFn ())
drawChart points filename = do
  renderableToFile def filename (distanceElevationOverTimeChart points)

