##############################################################
# A file to plot the output text file frrom kintersections 
#############################################################

rm(list = ls(all = TRUE)) #clears the 


# The simpliest way to use this script is to put all the .txt output files into the same directory with this file 
# then use the next command to set the working directory to that directory. 
# 
# setwd() # Sets the workign directory so all file calls are local. Not needed if using RStudio Projects


#######################################################
##  Install Needed Packages
#######################################################

## The next 4 lines need to be run only once per computer.
##  ## You must run them in order one at a time. 
##  ## Wait for each command to finish before you run the next line
##  ## ## They are commented out because you only want to run them once.

# install.packages("sf")
# install.packages("devtools")
# library(devtools)
# devtools::install_github("tidyverse/ggplot2")




#######################################################
##  Load Needed Packages
#######################################################
# These give you the commands that are needed to run the function.
# They have to be run after any clearing of the workspace. 
library(sf)
library(ggplot2)
## NOTE AS OF 10.17.17 YOU NEED THE DEVELOPMENT VERSION of {ggplot2} TO GET geom_sf() in ggplot(). 
## Thats what the devtools::install_githum() command above did for you. 




#######################################################
##  The Function
#######################################################
# # Extracts the POLYGONS in Well Known Text Format from teh .txt output files.
# # Casts the more general POLYGONS into MULTIPOLYGONS to explicitly account for holes. 
# # Converts the object type from an sf Geometry Set to an sf Simple Feature Collection. 
# # Adds an ID number for each MULTIPOLYGON
# # Plots the MULTIPOLYGONS using ggplot()

plotKintersectionsPolys <- function(outputFileName) {
  #Reads in all of the lines in the .txt file.
  outputPolys <- readLines(outputFileName)
  
  # Extracts only the POLYGON date from the object.
  outputPolys <- outputPolys[2:length(outputPolys)]
  
  #Convert the Well Known Text into R objects
  outputPolysAsGeometrySet <-  st_as_sfc(outputPolys)
  
  # Just to be safe I cast these POLYGONS to the more specific MULTIPOLYGONS.
  outputMultipolysAsGeometrySet <- st_cast(outputPolysAsGeometrySet, "MULTIPOLYGON")
  
  # To plot with ggplot() and geom_sf() you need convert the "Geometry set" into a "Simple feature collection" using st_sf()
  outputMultipolysAsSfCollection <- st_sf(geometry=outputMultipolysAsGeometrySet)
  
  # Add an ID for each Multipoly
  outputMultipolysAsSfCollection$id <- c(1:length(outputMultipolysAsSfCollection$geometry))
  
  outPlot <- ggplot(outputMultipolysAsSfCollection) + geom_sf(aes(fill = as.factor(id)))
  
  print(outPlot)
}




#######################################################
##  Use the function
#######################################################

plotKintersectionsPolys(outputFileName = "out-1.txt")

plotKintersectionsPolys(outputFileName = "out-2.txt")

plotKintersectionsPolys(outputFileName = "out-3.txt")

plotKintersectionsPolys(outputFileName = "out-4.txt")

plotKintersectionsPolys(outputFileName = "out-5.txt") # There is no polygon for k=5 will return error.

plotKintersectionsPolys(outputFileName = "out-6.txt") # There is no polygon for k=6 will return error.

plotKintersectionsPolys(outputFileName = "out-7.txt") # There is no polygon for k=7 will return error.








