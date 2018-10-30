#####################################################
# This is an R script I use when doing development work on the package. The end user should not have a need to use this script.
# Looking at the WinSet for lots of different sets of voters
#####################################################
rm(list = ls(all = TRUE))


#################
# R OPTIONS
#################
options(scipen=2) #scientific notation off
options(digits=10)

##################
# Globals
##################
#numRunsGlobal <- 4
#numDimsGlobal <- 2
#numVotersGlobal <- 100
#numAltsGlobal <- 2

##############################################
## LOAD THE PACKAGES (Install if not loaded):
##############################################
# install.packages("Rcpp")


# install.packages("BH")


# install.packages("ggplot2")
library(ggplot2)
library(voteR)
library(sf)


# Create a voter data frame. 
voterSet0 <- data.frame(voterID = c(1, 2, 3, 4, 5), xIdeal=c(15, 20, 35, 40, 55), yIdeal=c(45, 15, 50, 35, 30), minkoOrder=c(1, 2, 3, 2, 1), xSalience = c(1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1) )


voterSet1 <- data.frame(voterID = c(1, 2), xIdeal=c(15, 20), yIdeal=c(45, 15), minkoOrder=c(1, 2), xSalience = c(1, 1), ySalience = c(1, 1) )

altPoint <- c(30,40)

# Extract the indifference curves
allVotersICPointsList0 <- list()

for (j in 1:nrow(voterSet0)){
    allVotersICPointsList0[[j]] <- as.matrix(findICPoints(voterID = voterSet0$voterID[j], idealPoint = c(voterSet0$xIdeal[j], voterSet0$yIdeal[j]), orderScalar = voterSet0$minkoOrder[j], salienceVector = c(voterSet0$xSalience[j], voterSet0$ySalience[j]), altPointVector = altPoint, precision = .01))
}


allVotersICPointsList1 <- list()

for (j in 1:nrow(voterSet1)){
    allVotersICPointsList1[[j]] <- as.matrix(findICPoints(voterID = voterSet1$voterID[j], idealPoint = c(voterSet1$xIdeal[j], voterSet0$yIdeal[j]), orderScalar = voterSet1$minkoOrder[j], salienceVector = c(voterSet1$xSalience[j], voterSet1$ySalience[j]), altPointVector = altPoint, precision = .01))
}




#Write out the polygons to a .txt file



fileConn<-file("txtFiles/testFromR.txt")
writeLines(c("Hello","World"), fileConn)
close(fileConn)





system2(command = "/Users/ragan_ra/Dropbox/Documents/RDev/IntersectionCpp3/kintersection", c("1", "0.00001", "/Users/ragan_ra/Dropbox/Documents/RDev/IntersectionCpp3/test.txt", "/Users/ragan_ra/Dropbox/Documents/RDev/IntersectionCpp3/outTest.txt"))



findWinSet(indifferenceCurves =  allVotersICPointsList0)

plotteRvoteR(votersDataFrame = voterSet0, altPoints = c(30,40), plotIdeals = TRUE, plotICs = TRUE, plotPareto = FALSE, plotAlt = TRUE, yToXRatio = 1)

# A Test Plot using the demo shapes for boost::intersect()
blue <- cbind(c(1),currentOut$blue)
green <- cbind(c(2),currentOut$green)
intersectBlueGreen <- cbind(c(3),currentOut$intersection2[[1]])

outDataFrame1 <- data.frame(rbind(blue,green,intersectBlueGreen))



outDataFrame1$X1 <- as.factor(outDataFrame1$X1)

interPlot1 <- ggplot() + 
    geom_polygon(data = outDataFrame1, mapping = aes(group = X1, color = X1, fill = X1, x = X2, y = X3), alpha = 1/2) + 
    coord_fixed()

interPlot1


# A Second Test Plot using the demo shapes for boost::intersect(). Checking the original Shapes
outDataFrame3 <- data.frame(rbind(blue,green))

outDataFrame3$X1 <- as.factor(outDataFrame3$X1)

interPlot3 <- ggplot() + 
    geom_polygon(data = outDataFrame3, mapping = aes(group = X1, color = X1, fill = X1, x = X2, y = X3), alpha = 1/2) + 
    coord_fixed()

interPlot3




## A Test Plot for Voters ICs and their intersetion
voter1 <- cbind(c(1),currentOut$voter1AsMatrix)
voter2 <- cbind(c(2),currentOut$voter2AsMatrix)
# intersectVoter1Voter2 <- cbind(c(3),currentOut$intersection)


# First lets look at the original shapes and make sure they overlap
outDataFrame2 <- data.frame(rbind(voter1,voter2))

outDataFrame2$X1 <- as.factor(outDataFrame2$X1)

interPlot2 <- ggplot() + 
    geom_polygon(data = outDataFrame2, mapping = aes(group = X1, color = X1, fill = X1, x = X2, y = X3), alpha = 1/2) + 
    coord_fixed()

interPlot2


# Now Lets look at the intersections:


voter1 <- cbind(c(1),currentOut$voter1AsMatrix)
voter2 <- cbind(c(2),currentOut$voter2AsMatrix)
intersectVoter1Voter2 <- cbind(c(3),currentOut$intersection)


# First lets look at the original shapes and make sure they overlap
outDataFrame4 <- data.frame(rbind(voter1,voter2,intersectVoter1Voter2))

outDataFrame4$X1 <- as.factor(outDataFrame4$X1)

interPlot4 <- ggplot() + 
    geom_polygon(data = outDataFrame4, mapping = aes(group = X1, color = X1, fill = X1, x = X2, y = X3), alpha = 1/2) + 
    coord_fixed()

interPlot4



#1) A set of voters to look at the WinSet:      

voterSet1 <- data.frame(voterID = c(1, 2, 3, 4, 5), xIdeal=c(15, 20, 35, 40, 55), yIdeal=c(45, 15, 50, 35, 30), minkoOrder=c(1, 2, 3, 2, 1), xSalience = c(1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1) )

plotteRvoteR(votersDataFrame = voterSet1, altPoints = c(30,40), plotIdeals = TRUE, plotICs = TRUE, plotPareto = FALSE, plotAlt = TRUE, yToXRatio = 1)

#2) A set of voters to look at the WinSet:      

voterSet2 <- data.frame(voterID = c(1, 2, 3, 4, 5), xIdeal=c(15, 20, 35, 40, 55), yIdeal=c(45, 15, 50, 35, 30), minkoOrder=c(5, 4, 3, 2, 1), xSalience = c(1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1) )

plotteRvoteR(votersDataFrame = voterSet2, altPoints = c(30,40), plotIdeals = TRUE, plotICs = TRUE, plotPareto = FALSE, plotAlt = TRUE, yToXRatio = 1)

#3) A set of voters to look at the WinSet:      

voterSet3 <- data.frame(voterID = c(1, 2, 3, 4, 5), xIdeal=c(15, 20, 35, 40, 55), yIdeal=c(45, 15, 50, 35, 30), minkoOrder=c(1, 2, 3, 4, 5), xSalience = c(1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1) )

plotteRvoteR(votersDataFrame = voterSet3, altPoints = c(30,40), plotIdeals = TRUE, plotICs = TRUE, plotPareto = FALSE, plotAlt = TRUE, yToXRatio = 1)

#4) A set of voters to look at the WinSet:      

voterSet4 <- data.frame(voterID = c(1, 2, 3, 4, 5), xIdeal=c(10, 20, 30, 40, 50), yIdeal=c(50, 40, 30, 20, 10), minkoOrder=c(1, 2, 3, 2, 1), xSalience = c(1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1) )

plotteRvoteR(votersDataFrame = voterSet4, altPoints = c(30,40), plotIdeals = TRUE, plotICs = TRUE, plotPareto = FALSE, plotAlt = TRUE, yToXRatio = 1)

#5) A set of voters to look at the WinSet:      

voterSet5 <- data.frame(voterID = c(1, 2, 3, 4, 5), xIdeal=c(15, 20, 35, 40, 55), yIdeal=c(45, 15, 50, 35, 30), minkoOrder=c(1, 2, 3, 2, 1), xSalience = c(1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1) )

plotteRvoteR(votersDataFrame = voterSet5, altPoints = c(30,40), plotIdeals = TRUE, plotICs = TRUE, plotPareto = FALSE, plotAlt = TRUE, yToXRatio = 1)

#6) Cross Reference Case:      

voterSet6 <- data.frame(voterID = c(1, 2, 3, 4, 5), xIdeal=c(15, 20, 35, 40, 55), yIdeal=c(45, 15, 50, 35, 30), minkoOrder=c(1, 2, 100, 2, 1), xSalience = c(1, 1, 2, 1, 1), ySalience = c(1, 1, 1, 1, 1) )

plotteRvoteR(votersDataFrame = voterSet6, altPoints = c(30,40), plotIdeals = TRUE, plotICs = TRUE, plotPareto = FALSE, plotAlt = TRUE, yToXRatio = 1)

