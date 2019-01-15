## R Script for stacking NEON Data Files 
## Created 5 November, 2018. Megan A. Jones

## This R script allows you to combine multiple months & sites of NEON data 
## downloaded as a .zip file from the NEON data portal (data.neonscience.org).  
## This script is only for NEON data products that are delivered as zipped folders 
### of .csv files (most OS and IS data products but not AOP data products). 

## TO USE: Don't panic there are only 3 lines of code!
## 1. Download data of interest from NEON data portal to your computer. 
## 2. Open this file in R (or RStudio)
## 3. Modify the file path to the downloaded data (see line #44)
## 4. Run the entire script or each line that doesn't start with a # (there are 3).

## The neonUtilities package is hosted on CRAN: 
## https://CRAN.R-project.org/package=neonUtilities

## This script only uses a single function from the neonUtilites package. For 
## other functionality, see the related tutorial: 
## https://www.neonscience.org/neonDataStackR 

############ CODE #############

## Step 1: Install necessary packages
## You only need to do this once on your computer. 
install.packages("neonUtilities")

## Step 2: Load neonUtilities
## You can start here if you've recently completed Step 1 on your computer. 

# load neonUtilities (must be loaded to use the stackByTable function)
library(neonUtilities)

## Step 3: Stack NEON data 
## You will need to modify the file path below to the correct file path for 
## your data of interest. If you are having problems with file paths, see Trouble 
## Shooting, below. 
## The example file is NEON single aspirated air temp in the Download directory.
## Example Windows file path: stackByTable("C:/Downloads/NEON_temp-air-single.zip" )
## Example Mac file path: ("~neon/data/NEON_temp-air-single.zip")

# use stackByTable function to stack data.
stackByTable("C:/Downloads/NEON_temp-air-single.zip")


## You should now see output showing the program working. When compelete, check
## out the original data folder. You should now have an unzipped file containing a
## new file called StackedFiles this will contain the data tables that now 
## include all the months/sites you downloaded. 

######## TROUBLE SHOOTING ############
## 1. How do I get my file path?  
## The easiest way is to find the downloaded .zip file in the normal computer 
## window (Explorer/Finder window). Then: 
## Windows:  Right click on the file and choose "Copy as path" (older versions 
## may say "Copy address"). Paste this into the code above. Ensure that the 
## file name and extension (.zip) are included. Now change all "\" to "/". You 
## are done. 
## Macs:  Right click on the file and click "Get Info". Highlight the text of 
## Where:... Copy into the code above -- the little arrows will automatically 
## turn into /. Now type/copy in the acutal file name of the zipped folder 
## including the .zip extention (e.g., "NEON_temp-air-single.zip"). You are done. 

## 2. I'm getting errors! 
## First, read the error. The messages are designed to help you know what went
## wrong. 
## If the error is file path/file not there related? Double check your file path:
## * is it all spelled correctly & actually points to the correct zipped file? 
## * did you accidentally use "_" instead of "-" (the file name uses both!)? 
## * did you include the file extension (.zip)? 
## * did you include the quotes around the file path?
## * did you use "\" in the file path instead of "/"? 
## * are the data on a remote network and not your computer? 
## * are you trying to stack data that are not zipped or don't have .csv files?
## * does the error say "zip file 'filepath' cannot be opened"? Is your 
## downloaded file actually a zip file? On some Windows machines, zip files 
## are automatically unzipped. In that case, use this:

# stackByTable("C:/Downloads/NEON_temp-air-single", folder=T)



