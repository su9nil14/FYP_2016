{-# LANGUAGE Arrows #-}

module Summary where

import GPXparser
import Text.XML.HXT.Core
import Text.Printf
--import Text.Tabular
import Text.Html
import Data.Maybe
import Data.List
--import qualified Text.Tabular.AsciiArt  as A
--import qualified Text.PrettyPrint.Boxes as P


getFastestKMPace filename distance = do
  [trackSegments] <- runX (parseGPX filename >>> getTrksegment)
  let tracks = trks trackSegments
      result = catMaybes (phase2 distance tracks)
      result' = addSndItem' result
  putStrLn(printf "\nFastest " ++ show distance ++"km pace = "++formatTimeDeltaMS(getFastestPace result') ++ " mins/km")


--summarize data for gpx files
summarizeGPX :: String -> IO ()
summarizeGPX file = do
 [trackSegments] <- runX (parseGPX file >>> getTrksegment)
 trackPoints <- runX (parseGPX file >>> getTrkpoint)


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
summarizeGPXDataToTxtFile :: String -> String-> IO ()
summarizeGPXDataToTxtFile file path = do
 [trackSegs] <- runX (parseGPX file >>> getTrksegment)
 trackPts <- runX (parseGPX file >>> getTrkpoint)
 
 
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

 let finalStrings = (tableFormatt file firstdate distance duration avgpaceval totalclimb adjustedDistanceRounded adjustedPace)
 putStrLn(printf $ finalStrings)

 appendFile path $ (tableFormatt file firstdate distance duration avgpaceval totalclimb adjustedDistanceRounded adjustedPace)


--headerTable filename date dist dur avgpace totclm adjdist adjpace=
--  empty ^..^ colH "Date" ^|^ colH "Distance(km)" ^|^ colH "Duration(h:m:s)" ^|^ colH "AvgPace(mins/km)" ^|^ colH "TotalClimb(m)" 
--        ^|^ colH "Adj Distance(km)" ^|^ colH "Adj Pace(mins/km)"
--  +.+ row filename [show date,show dist,dur,avgpace,totclm,show adjdist,adjpace]

tableFormatt file firstdate distance duration avgpaceval totalclimb adjustedDistanceRounded adjustedPace =
  show firstdate++" ||"++show distance++"   ||"++duration++"\t||"++avgpaceval++"\t\t ||"++totalclimb++
   "\t\t  ||"++show adjustedDistanceRounded++"\t ||"++adjustedPace ++"\t\t  ||"++ show file ++ "\n" 



--print_table :: [[String]] -> IO ()
--print_table rows = P.printBox $ P.hsep 2 P.left (map (P.vcat P.left . map P.text) (transpose rows))


-- Takes all the Tracksegments and generates the HTML report
generateHtmlPage :: [Char] ->[Trackpoint] -> Tracksegment -> Html
generateHtmlPage file point segs = 
   let header1 = h1 $ stringToHtml ("GPX Summary Data "++file)

       title = thetitle $ stringToHtml ("GPX Summary Data "++file++"")
       theStyle = style (stringToHtml cssContent) ! [thetype "text/css"]
       theHeader = header $ concatHtml [title,theStyle]
       mainArea = thediv (concatHtml [header1,(statsTable point segs),br,chartTable file])
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
  "div {    width: 700px; margin-top: 50px; margin:0 auto;} "++
  "table {border-spacing: 20px 0px;} footer {text-align:right; "++
  "background-color:#EEEEEE; width:1000px; margin:0 auto; margin-top: 30px} "


chartTable :: String -> Html
chartTable fname = 
   let img1 = image ! [src (fname++"1.png")]
       img2 = image ! [src (fname++"2.png")]
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