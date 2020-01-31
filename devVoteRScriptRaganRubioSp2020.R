#####################################################
# This is an R script I use when doing development work on the package. The end user should not have a need to use this script.
#
# Used to write and test code for looking at different voting Rules
#
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
numRunsGlobal <- 4
numDimsGlobal <- 2
numVotersGlobal <- 100
numAltsGlobal <- 2

##############################################
## LOAD THE PACKAGES (Install if not loaded):
##############################################



library(voteR)


####################################################################
# 
####################################################################

set.seed(123) # for development purposes




# 1) Create three sets of alternatives for testing purposes

generatedAltsDataFrame1 <- genAlts(numberOfDimensionsGenAlts = numDimsGlobal, numberOfAltsGenAlts = 2, distributionTypeGenAlts = "norm", distributionParametersGenAlts =  c(0,.2))


generatedAltsDataFrame2 <- genAlts(numberOfDimensionsGenAlts = numDimsGlobal, numberOfAltsGenAlts = 25, distributionTypeGenAlts = "unif", distributionParametersGenAlts =  c(-1,1))


staticAltsDataFrame1 <- data.frame(pointType = rep(x = "alternative", 3), ID = c("A-1", "A-2", "A-3"), xLocation=c(-3/8, 1/8, 2/8), yLocation=c(-3/8, 1/8, 7/8) )

# 2) Create three voter data frames to test with:

generatedVoterDataFrame1 <- genVoters(numberOfDimensionsGenVoters = numDimsGlobal, numberOfVotersGenVoters = 25, distributionTypeGenVoters = "unif")

generatedVoterDataFrame2 <- genVoters(numberOfDimensionsGenVoters = numDimsGlobal, numberOfVotersGenVoters = numVotersGlobal, distributionTypeGenVoters = "unif")

staticVoterDataFrame1 <- data.frame(pointType = rep(x = "voter", 3), ID = c("V-1", "V-2", "V-3"), xLocation=c(-1/8, 7/8, 4/8), yLocation=c(3/8, 4/8, -3/8), minkoOrder=c(1, 2, 100), xSalience = c(1, 1, 1), ySalience = c(1, 1, 1), lossOrder = c(1, 2, 1) )


# 3) Calulate the Minkowski Distance and Minkowsi Utility of each voter from each alt:


minkoDist1 <- calcMinkowskiDistanceVotersAlts(votersCalcMinkowskiDistanceVotersAlts = staticVoterDataFrame1, alternativesCalcMinkowskiDistanceVotersAlts = staticAltsDataFrame1)

minkoDist1


minkoUtil1 <- calcMinkowskiUtilityVotersAlts(votersCalcMinkowskiDistanceVotersAlts = staticVoterDataFrame1, alternativesCalcMinkowskiDistanceVotersAlts = staticAltsDataFrame1)

minkoUtil1



# 4) Find the Pareto Set of the Group of Voters using undelying Rcpp function

paretoSetG1 <- findParetoSet( cbind(generatedVoterDataFrame1$xLocation, generatedVoterDataFrame1$yLocation) )

paretoSetG1



paretoSetG2 <- findParetoSet( cbind(generatedVoterDataFrame2$xLocation, generatedVoterDataFrame2$yLocation) )

paretoSetG2



paretoSetS1 <- findParetoSet( cbind(staticVoterDataFrame1$xLocation, staticVoterDataFrame1$yLocation) )

paretoSetS1





# 5) Find the Pareto Set of the Group of Voters using the voters data frame

paretoSetG1Again <- findParetoSetForDataFrame(generatedVoterDataFrame1)

paretoSetG1Again


paretoSetG2Again <- findParetoSetForDataFrame(generatedVoterDataFrame2)

paretoSetG2Again



paretoSetS1Again <- findParetoSetForDataFrame( staticVoterDataFrame1 )

paretoSetS1Again





# 6) Plot the voters and Pareto Set
# Leave commented out when workign because they are slow. 


#   plotParetoSet(generatedVoterDataFrame1, idealsOn  = TRUE)

#   plotParetoSet(generatedVoterDataFrame1, idealsOn = FALSE)


#   plotParetoSet(generatedVoterDataFrame2, idealsOn  = TRUE)

#   plotParetoSet(generatedVoterDataFrame2, idealsOn = FALSE)



#   plotParetoSet(staticVoterDataFrame1, idealsOn  = TRUE)

#   plotParetoSet(staticVoterDataFrame1, idealsOn = FALSE)



#7 ) Is an alternative in the Pareto Set, given the alt and the Pareto Set?

isInParetoSetFromPointAndPS(aSexpPoint = c(0,0), aSexpMatrix = as.matrix(paretoSetG1)) 

isInParetoSetFromPointAndPS(aSexpPoint = c(.25,.5), aSexpMatrix = as.matrix(paretoSetG1)) 



isInParetoSetFromPointAndPS(aSexpPoint = c(0,0), aSexpMatrix = as.matrix(paretoSetG2)) 

isInParetoSetFromPointAndPS(aSexpPoint = c(.25,.5), aSexpMatrix = as.matrix(paretoSetG2))


isInParetoSetFromPointAndPS(aSexpPoint = c(0,0), aSexpMatrix = as.matrix(paretoSetS1)) # Should be false

isInParetoSetFromPointAndPS(aSexpPoint = c(2/8, 2/8), aSexpMatrix = as.matrix(paretoSetS1)) # Should be true



#8) Is an alternaive in the Pareto Set given the alt and a set of ideals.

idealsG1 <- cbind(generatedVoterDataFrame1$xLocation, generatedVoterDataFrame1$yLocation)

isInParetoSetFromPointAndIdeals(aSexpPoint = c(0,0), aSexpMatrix = idealsG1) 

isInParetoSetFromPointAndIdeals(aSexpPoint = c(.25,.5), aSexpMatrix = idealsG1) 



idealsG2 <- cbind(generatedVoterDataFrame2$xLocation, generatedVoterDataFrame2$yLocation)

isInParetoSetFromPointAndIdeals(aSexpPoint = c(0,0), aSexpMatrix = idealsG2) 

isInParetoSetFromPointAndIdeals(aSexpPoint = c(.25,.5), aSexpMatrix = idealsG2) 



idealsS1 <- cbind(staticVoterDataFrame1$xLocation, staticVoterDataFrame1$yLocation)

isInParetoSetFromPointAndIdeals(aSexpPoint = c(0,0), aSexpMatrix = idealsS1) # Should be false

isInParetoSetFromPointAndIdeals(aSexpPoint = c(2/8, 2/8), aSexpMatrix = idealsS1)  # Should be true





#9) Is an alternaive in the Pareto Set given the alt and the Voter Data Frame


isInParetoSetForVoterDataFrame(alternativeToCheck = c(0,0), votersDataFrame = generatedVoterDataFrame1) 

isInParetoSetForVoterDataFrame(alternativeToCheck = c(.25,.5), votersDataFrame = generatedVoterDataFrame1) 




isInParetoSetForVoterDataFrame(alternativeToCheck = c(0,0), votersDataFrame = generatedVoterDataFrame2) 

isInParetoSetForVoterDataFrame(alternativeToCheck = c(.25,.5), votersDataFrame = generatedVoterDataFrame2) 




isInParetoSetForVoterDataFrame(alternativeToCheck = c(0,0), votersDataFrame = staticVoterDataFrame1) # Should be false

isInParetoSetForVoterDataFrame(alternativeToCheck = c(2/8, 2/8), votersDataFrame = staticVoterDataFrame1)  # Should be true





#9) For a set of Alternatives are they in the Pareto Set given the Alternatives Data Frame and the Voter Data Frame


isInParetoSetForVoterAndAltsDataFrame(alternativesDataFrame = generatedAltsDataFrame1, votersDataFrame = generatedVoterDataFrame1) 


isInParetoSetForVoterAndAltsDataFrame(alternativesDataFrame = generatedAltsDataFrame2, votersDataFrame = generatedVoterDataFrame2) 


isInParetoSetForVoterAndAltsDataFrame(alternativesDataFrame = staticAltsDataFrame1, votersDataFrame = staticVoterDataFrame1) # Shouldl be FALSE TRUE FALSE



# 10) Find the Minlowski Distance between two points to verify it is the same as the superelipse code:

oneIdeal <- matrix(c(-.25,-.25),nrow = 1, ncol = 2)
oneAlt <- matrix(c(.20,.10),nrow = 1, ncol = 2)
theOrder <-  c(3)
salience <- c(1,2)
oneVoterID <- c(1)
theLoss <-  c(1)

# First Find the Utility 
testMinkoDist <- minkowskiUtilitySets(idealsMatrix = oneIdeal, altsMatrix = oneAlt, minkoOrderVector = theOrder, salienceMatrix = matrix(salience, nrow = 1, ncol = 2), lossOrderVector = theLoss)

# Now find the Super elipse Radius 
testSuperElipseRadius <- findSuperElipseRadius(idealPoint = as.vector(oneIdeal), altPoint = as.vector(oneAlt), orderScalar = theOrder, salienceVector = salience)

# Now check to make sure they are the same. 
abs(testMinkoDist) == testSuperElipseRadius




# 10) Find the Preferred-to Sets for a group of voters and plot them 


# A couple more Voter Data Frames with Lots of heterogeneity of ideals, saliences and minkoOrders
staticVoterDataFrame2 <- data.frame(pointType= rep("voter",11), ID = c("V-1", "V-2", "V-3", "V-4", "V-5", "V-6", "V-7", "V-8", "V-9", "V-10", "V-11"), xLocation=c(-.8, -.5, .2, .9, -.2, .15, -.15, -.35, .35, .32, -.44), yLocation=c(.1, .5, -.3, 0, .9, -.7, .35, -.45, -.26, .21, -.03), minkoOrder=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), xSalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), lossOrder=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) )


staticVoterDataFrame3 <- data.frame(pointType= rep("voter",11), ID = c("V-1", "V-2", "V-3", "V-4", "V-5", "V-6", "V-7", "V-8", "V-9", "V-10", "V-11"), xLocation=c(-.8, -.5, .2, .9, -.2, .15, -.15, -.35, .35, .32, -.44), yLocation=c(.1, .5, -.3, 0, .9, -.7, .35, -.45, -.26, .21, -.03), minkoOrder=c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), xSalience = c(1, 2, 1, 3, 1, 4, 1, 5, 1, 6, 1), ySalience = c(1, 1, 2, 1, 3, 1, 4, 1, 5, 1, 6), lossOrder=c(1, 2, 2, 1, 1, 1, 2, 1, 1, 2, 1))





# First for one voter and alt created just for this:
findICPoints(voterID = "V-1", idealPoint = c(.5, .5), altPointVector = c(0,0), orderScalar = 2, salienceVector = c(1,1), precision = .01)

# Now pull one voter form a voter matrix
vt <- 2 

findICPoints( voterID = staticVoterDataFrame1$ID[vt], 
              idealPoint = c(staticVoterDataFrame1$xLocation[vt], staticVoterDataFrame1$yLocation[vt]), 
              altPointVector = c(0,0), 
              orderScalar = staticVoterDataFrame1$minkoOrder[vt], 
              salienceVector = c(staticVoterDataFrame1$xSalience[vt], staticVoterDataFrame1$ySalience[vt]), 
              precision = .01 )


## Now find the IC's for all the voters
allICsStaticVoterDataFrame1 <- findICsForSetOfVoters(votersDataFrame = staticVoterDataFrame1, altPoint = c(0,0), precision = .01)


plotIC(votersDataFrame = staticVoterDataFrame1, altPoint = c(0,0), precision = .01)


plotIC(votersDataFrame = staticVoterDataFrame2, altPoint = c(0,0), precision = .01)


plotIC(votersDataFrame = staticVoterDataFrame3, altPoint = c(0,0), precision = .01)

## If you change everything to minkoOrder 1 you get Diamond Shaped ICs

# All MinkoOrder = 1
staticVoterDataFrameAllMinko1 <- data.frame(pointType= rep("voter",11), ID = c("V-1", "V-2", "V-3", "V-4", "V-5", "V-6", "V-7", "V-8", "V-9", "V-10", "V-11"), xLocation=c(-.8, -.5, .2, .9, -.2, .15, -.15, -.35, .35, .32, .11), yLocation=c(.1, .5, -.3, 0, .9, -.7, .35, -.45, -.26, .21, -.11), minkoOrder=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), xSalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), lossOrder=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) )

voterDataFrameAllMinko1ICs <- findICsForSetOfVoters(votersDataFrame = staticVoterDataFrameAllMinko1, altPoint = c(0,0), precision = .01)

plotIC(votersDataFrame = staticVoterDataFrameAllMinko1, altPoint = c(0,0),  precision = .01)


## What about all minko Order 2?

# All MinkoOrder = 2
staticVoterDataFrameAllMinko2 <- data.frame(pointType= rep("voter",11), ID = c("V-1", "V-2", "V-3", "V-4", "V-5", "V-6", "V-7", "V-8", "V-9", "V-10", "V-11"), xLocation=c(-.8, -.5, .2, .9, -.2, .15, -.15, -.35, .35, .32, .11), yLocation=c(.1, .5, -.3, 0, .9, -.7, .35, -.45, -.26, .21, -.11), minkoOrder=c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), xSalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), lossOrder=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) )

voterDataFrameAllMinko2ICs <- findICsForSetOfVoters(votersDataFrame = staticVoterDataFrameAllMinko2, altPoint = c(0,0), precision = .01)


plotIC(votersDataFrame = staticVoterDataFrameAllMinko2, altPoint = c(0,0), precision = .01)



## What about all minko Order 4?

# All MinkoOrder = 1000
staticVoterDataFrameAllMinko4 <- data.frame(pointType= rep("voter",11), ID = c("V-1", "V-2", "V-3", "V-4", "V-5", "V-6", "V-7", "V-8", "V-9", "V-10", "V-11"), xLocation=c(-.8, -.5, .2, .9, -.2, .15, -.15, -.35, .35, .32, .11), yLocation=c(.1, .5, -.3, 0, .9, -.7, .35, -.45, -.26, .21, -.11), minkoOrder= rep(1000, 11), xSalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), lossOrder=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) )

voterDataFrameAllMinko4ICs <- findICsForSetOfVoters(votersDataFrame = staticVoterDataFrameAllMinko4, altPoint = c(0,0), precision = .01)

plotIC(votersDataFrame = staticVoterDataFrameAllMinko4, altPoint = c(0,0), precision = .01)


# 11) Now lets look at the plotteRvoteR function, using a common homework question scenario:      

staticVoterDataFrame4 <- data.frame(pointType= rep("voter", 5), ID = c("V-1", "V-2", "V-3", "V-4", "V-5"), xLocation=c(15, 20, 35, 40, 55), yLocation=c(45, 15, 50, 35, 30), minkoOrder=c(2, 2, 2, 2, 2), xSalience = c(1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1) )



plotteRvoteR(votersDataFrame = staticVoterDataFrame4, altPoints = c(30,40), plotIdeals = TRUE, plotICs = TRUE, plotPareto = TRUE, plotAlts = TRUE, yToXRatio = 1)


# 12) Now lets look at the plotteRvoteR function, with only 3 ideals, but with different Minko Orders:      

staticVoterDataFrame5 <- data.frame(pointType= rep("voter", 3), ID = c("V-1", "V-2", "V-3"), xLocation=c(15, 20, 35), yLocation=c(45, 15, 50), minkoOrder=c(1, 2, 100), xSalience = c(1, 1, 1), ySalience = c(1, 1, 1) )

plotteRvoteR(votersDataFrame = staticVoterDataFrame5, altPoints = c(30,40), plotIdeals = TRUE, plotICs = TRUE, plotPareto = FALSE, plotAlt = TRUE, yToXRatio = 1)


# 13) Now lets look at the plotteRvoteR function, using the staticVoterDataFrameAll:      

plotteRvoteR(votersDataFrame = staticVoterDataFrame1, altPoints = c(0,0), plotIdeals = TRUE, plotICs = TRUE, plotPareto = TRUE, plotAlt = TRUE, yToXRatio = 1)


# 14) Is alternaive B in the preferred to set of alternative A for a voter.

## Looking at only voter 1
vt <- 1
ICToCheck <- findICPoints(voterID = staticVoterDataFrame5$ID, idealPoint = c(staticVoterDataFrame5$xLocation[vt], staticVoterDataFrame5$yLocation[vt]), altPointVector = c(0,0), orderScalar = staticVoterDataFrame5$minkoOrder[vt], salienceVector = c(staticVoterDataFrame5$xSalience[vt], staticVoterDataFrame5$ySalience[vt]), precision = .01)

# Voter 1's IC for (0,0)
plotIC(votersDataFramePlotIC = staticVoterDataFrame5[1, ], altPointPlotIC = c(0,0), precisionPlotIC = .01)


### This function isInICFromPointAndIC() needs to be generalized into a couple or wrappers like the isInPareto family of funcitons (01.31.2020) ##
isInICFromPointAndIC(aSexpPoint = c(0,0), aSexpMatrix = as.matrix(ICToCheck[ ,2:3])) # Should be TRUE (0,0) is on the IC

isInICFromPointAndIC(aSexpPoint = c(-0.0001, -0.0001), aSexpMatrix = as.matrix(ICToCheck[ ,2:3])) # Should be FALSE as this is just outside the IC

isInICFromPointAndIC(aSexpPoint = c(15,45), aSexpMatrix = as.matrix(ICToCheck[ ,2:3])) # Should be TRUE (15,45) is the voters ideal point

isInICFromPointAndIC(aSexpPoint = c(-25,25), aSexpMatrix = as.matrix(ICToCheck[ ,2:3])) # Should be TRUE as it is on the IC 

isInICFromPointAndIC(aSexpPoint = c(-25.001,25), aSexpMatrix = as.matrix(ICToCheck[ ,2:3])) # Should be FALSE as it is just outside the IC.

isInICFromPointAndIC(aSexpPoint = c(50,75), aSexpMatrix = as.matrix(ICToCheck[ ,2:3])) # Should be FALSE 




# 16) For a set of voters and a set of alternatives, calculate each voter's minkowski distance between their ideal point and each alterantive (This is the voters full preference ordering over the alternatives) 

