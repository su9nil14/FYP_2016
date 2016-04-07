{-# LANGUAGE Arrows #-}

module Summary where

import GPXparser
import Text.XML.HXT.Core
import Text.Printf
import Text.Tabular
import Text.Html

--summarize data for gpx files
summarizeGPX :: String -> IO ()
summarizeGPX file = do
 [trackSegments] <- runX (parseGPX file >>> getTrkseg)
 trackPoints <- runX (parseGPX file >>> getTrkpt)


 let (lenKm) = totalDistance trackSegments
 let (seconds) = trackDuration trackSegments
 putStrLn (printf "\n")
 putStrLn (printf "Track distance    (km)      :  %.2f" $ lenKm)
 putStrLn (printf "Track duration (h:m:s)      :  %s"   $ formatTimeHMS seconds)
 --putStrLn (printf "\n")
 

-- average minimum and maximum pace
 let pace = getPace trackSegments
 putStrLn (printf "Average pace (mins/km)      :  %.5s" $ formatTimeDeltaMS (seconds/lenKm))
 putStrLn (printf "Maximum pace (mins/km)      :  %.5s" $ formatPace(maxPace pace))
 putStrLn (printf "Minimum pace (mins/km)      :  %.5s" $ formatPace(minPace pace))
 --putStrLn (printf "\n")
 

-- average minimum and maximum elevation
 let (avgelev, minelev, maxelev) = getAvgMinMaxElevation trackPoints
 putStrLn (printf "Average Elevation (MSL)     :  %s"   $ roundNumbers avgelev)
 putStrLn (printf "Minimum Elevation (MSL)     :  %s"   $ roundNumbers minelev)
 putStrLn (printf "Maximum Elevation (MSL)     :  %s"   $ roundNumbers maxelev)


 let (climb) = totalClimb trackPoints
 putStrLn (printf "Total Climb (m)             :  %s"   $ roundNumbers climb)


 let (adjustedKm) = adjustedDistance lenKm climb
 putStrLn (printf "Adjusted flat distance (km) :  %.2f" $ adjustedKm)
 putStrLn (printf "Avg Adjusted Pace (mins/km) :  %.5s" $ formatTimeDeltaMS (seconds/adjustedKm))
 

 let steepness = getSteepness climb lenKm
 putStrLn (printf "Steepness (percent)         :  %s"   $ roundNumbers steepness)

 putStrLn (printf "--------------------------------------------------------")



--summarize data for gpx files
summarizeGPXDataToTxtFile :: String-> IO ()
summarizeGPXDataToTxtFile file = do
 [trackSegs] <- runX (parseGPX file >>> getTrkseg)
 trackPts <- runX (parseGPX file >>> getTrkpt)

 
 -- total distance and time of the track
 let (lenKm) = totalDistance trackSegs
 let (seconds) = trackDuration trackSegs
 let (climb) = totalClimb trackPts

 let dateTime = getTimePoints trackPts
 let firstdate = head dateTime
 let distance = formatDistance lenKm 2 
 let avgpaceval = formatTimeDeltaMS (seconds/lenKm)
 let duration = formatTimeHMS seconds
 let totalclimb = roundNumbers climb
 let adjustedDist = adjustedDistance lenKm climb
 let adjustedDistanceRounded = formatDistance adjustedDist 2
 let adjustedPace = formatTimeDeltaMS (seconds/adjustedDist)

 let finalStrings = tableFormat file firstdate distance duration avgpaceval totalclimb adjustedDistanceRounded adjustedPace

 putStrLn (printf $ finalStrings)
 appendFile ("reportsTxt/summary.txt") $ finalStrings ++"\n"


--table :: (Show a, Show a1) => a -> [Char] -> [Char] -> [Char] -> a1 -> [Char] -> [Char]
tableFormat file firstdate distance duration avgpaceval totalclimb adjustedDistanceRounded adjustedPace =
  "Filename\t||DateTime\t\t||Distance\t||Duration\t||Average Pace\t||Total Climb\t||Adjusted Distance\t||Adjusted Pace\n"++
   show file++"\t\t||"++show firstdate++"\t||"++show distance++"\t\t||"++duration++"\t||"++avgpaceval++"\t\t||"++totalclimb++
   "\t\t||"++show adjustedDistanceRounded++"\t\t\t||"++adjustedPace ++"\n" 
     ++ "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"



-- Takes all the Tracksegments and generates the HTML report
generateHtmlPage :: [Char] -> [Trackpoint] -> Tracksegment -> Html
generateHtmlPage file point segs = 
   let header1 = h1 $ stringToHtml ("GPX Summary Data "++file)

       title = thetitle $ stringToHtml ("GPX Summary Data "++file++"")
       theStyle = style (stringToHtml cssContent) ! [thetype "text/css"]
       theHeader = header $ concatHtml [title,theStyle]
       mainArea = thediv (concatHtml [header1,(statsTable point segs),br,chartTable])
       theBody = body mainArea
   in concatHtml [theHeader,theBody]--,pageFooter]



statsTable :: [Trackpoint] -> Tracksegment -> Html --Date-Time, FileName Distance, Duration, Avg. Pace, Total Climb, Adj. Distance. Adj. Pace
statsTable point segs = 
   let tblHeader1 = th $ stringToHtml "Journey Details"
       tblHeader2 = th $ stringToHtml "Elevation Data"
       tblHeader3 = th $ stringToHtml "Performance"

       lenKm = totalDistance segs
       seconds = trackDuration segs
       paceVal = getPace segs

       climb = totalClimb point
       dateTime = getTimePoints point
       firstdate = head dateTime
       distance = formatDistance lenKm 2 
       avgpaceval = formatTimeDeltaMS (seconds/lenKm)
       duration = formatTimeHMS seconds
       totalclimb = roundNumbers climb
       adjustedDist = adjustedDistance lenKm climb
       adjustedDistanceRounded = formatDistance adjustedDist 2
       adjustedPace = formatTimeDeltaMS (seconds/adjustedDist)
       (avgelev, minelev, maxelev) = getAvgMinMaxElevation point

       adjustedPaceVal = formatTimeDeltaMS (seconds/adjustedDist)


       dateCol1 = li $ stringToHtml ("Date & Time : "++ show firstdate)
       distCol1 = li $ stringToHtml ("Distance (km) : "++ show distance)
       durCol1 = li $ stringToHtml ("Duration (h:m:s): "++ duration)

       elevCol1 = li $ stringToHtml ("Total Climb (m): "++ totalclimb)
       elevCol2 = li $ stringToHtml ("Maximum Elevation (m): "++ roundNumbers maxelev)
       elevCol3 = li $ stringToHtml ("Minimum Elevation (m): "++ roundNumbers minelev)
      
       paceCol31 = li $ stringToHtml ("Average Pace (mins/km): "++ avgpaceval)
       paceCol32 = li $ stringToHtml ("Max Pace (mins/km): "++  formatPace(maxPace paceVal))
       paceCol33 = li $ stringToHtml ("Min Pace (mins/km): "++ formatPace(minPace paceVal))
       sepCol34 = li $ stringToHtml ("_________________________")
       paceCol34 = li $ stringToHtml ("Adjusted Pace (mins/km): "++ adjustedPaceVal)
       adjDistCol34 = li $ stringToHtml ("Adjusted Distance (km): "++ show adjustedDistanceRounded)


       col1 = td (concatHtml [dateCol1,distCol1,durCol1]) ! [valign "top"]
       col2 = td  (concatHtml [elevCol1,elevCol2,elevCol3]) ! [valign "top"]
       col3 = td  (concatHtml [paceCol31,paceCol32,paceCol33,sepCol34,paceCol34,adjDistCol34]) ! [valign "top"]
       row1 = tr $ concatHtml [tblHeader1,tblHeader2,tblHeader3]
       row2 = tr $ concatHtml [col1,col2,col3]
       tbl = table (concatHtml [row1,row2])
   in center tbl

   
-- CSS style
cssContent :: [Char]
cssContent = "h1 {color: #2C558A; font-weight: normal; font-size: x-large; "++
  "text-shadow: white 0px 1px 1px; letter-spacing: 0.1em; font-family: 'Gill Sans', "++
  "'PT Sans', 'Arial', sans-serif; text-transform: uppercase;} " ++
  "div {    width: 900px; margin-top: 50px; margin:0 auto;} "++
  "table {border-spacing: 20px 0px;} footer {text-align:right; "++
  "background-color:#EEEEEE; width:1000px; margin:0 auto; margin-top: 30px} "


chartTable :: Html
chartTable = 
   let img1 = image ! [src "elevationTime.png"]
       img2 = image ! [src "timeDuration.png"]
       cell1 =  td img1
       cell2 = td img2
       row1 = tr $ concatHtml [cell1,cell2]
       tbl = table row1
   in center tbl

pageFooter :: Html
pageFooter = 
   let projectLink = anchor (stringToHtml "Source files available at Github") ! [href "https://github.com/su9nil14/FYP_2016"]
       infoStr = stringToHtml "|Sunil Gautam|  "
   in footer (concatHtml [infoStr,projectLink]) ! [identifier "main"]

footer :: Html -> Html
footer = tag "FOOTER"