# FYP_2016

GPS trace analysis using Haskell

# Requirements
NOTE: It is highly recommended to install this program in Linux. It will work on Unix or Windows but installing gtk+ and its libraries may be problematic or frustarting

 hxt package  ```cabal install hxt```
 
 tabular ```cabal install tabular```
 
 directory ```cabal install directory```
 
 filepath ```cabal install filepath```

 html ```cabal install html```
 
 ```cabal update``` and then ```cabal install chart-diagrams```
 for more info visit https://github.com/timbod7/haskell-chart/wiki
 
 For the cairo backend, it is recommended to install and test gtk2hs
first.  There are
[instructions](http://www.haskell.org/haskellwiki/Gtk2Hs/Installation)
on the haskell wiki. Once that is done, installation of the chart
library and cairo backend is also straightforward:

    cabal update
    cabal install chart-cairo


#Installation process
cd to src folder and execute ```ghc Main.hs``` or ```ghci Main.hs```on terminal to start the program

# Usage

```USAGE  [COMMAND]

   S - Summarize all GPX file in the directory to terminal and write to text file
   
   s - Summarize given GPX file to terminal
   
   r - Summarize all GPX file in the directory and write to HTML file
   
   ? - Get this help message
   
   q - Quit the program ```

# More resources and credits

Similar project at (and credit to) -- https://github.com/nurpax/gpx-utils

http://www.topografix.com/GPX/1/1/

https://hackage.haskell.org/package/GPX

https://www.schoolofhaskell.com/school/advanced-haskell/xml-parsing-with-validation

https://www.haskell.org/hoogle/?hoogle=uncurry+%2bbase

http://haskell-cookbook.com/

https://hackage.haskell.org/package/tabular-0.2.2.7
