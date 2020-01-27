#####################################################
# This is an R script I use when doing development work on the package. The end user should not have a need to use this script.
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
# install.packages("Rcpp")


# install.packages("BH")


# install.packages("ggplot2")
library(ggplot2)


library(voteR)
  

####################################################################
# 
####################################################################

set.seed(123) # for development purposes

#1) Create a static Voter Data Frame for testing

staticVoterDataFrame1 <- data.frame(voterID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), xIdeal=c(-.8, -.5, .2, .9, -.2, .15, -.15, -.35, .35, .32, -.44), yIdeal=c(.1, .5, -.3, 0, .9, -.7, .35, -.45, -.26, .21, -.03), minkoOrder=c(1, 2, 4, 2, 1, 1, 2, 10, 2, 1, 2), xSalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))


# 2) Gen a set of voters and store them for use across all runs of the model. (everntually we need a genVoterDataFrame function)

    #2a) Generate Ideal Points

    ideals <- genIdeals(numberOfDimensionsGenIdeals = numDimsGlobal, numberOfIdealsGenIdeals = numVotersGlobal, distributionTypeGenIdeals = "norm", distributionParametersGenIdeals = c(0,.2))

    # idealsDF <- data.frame(ideals)

    # 2b) Generate the Salience

    salience <- genSalience(numVotersGenSalience = numVotersGlobal, numDimsGenSalience = numDimsGlobal, salienceHeterogeneityGenSalience = 0, maxRelativeSalienceGenSalience = 3)

    # 2c) Generate a Minkowski Order

    minkoOrder <- sample(x = c(1,2,3,10,1000), size = numVotersGlobal, replace = TRUE, prob = c(.25,.5,.1,.05,.1))

# 3) Gen a set of Alternatives (note you can use genIdeals() for this too)

    altsGen1 <- genAlts(numberOfDimensionsGenAlts = numDimsGlobal, numberOfVotersGenAlts = 2, distributionTypeGenAlts = "norm", distributionParametersGenAlts =  c(0,.2))

    
    altsGen2 <- genAlts(numberOfDimensionsGenAlts = numDimsGlobal, numberOfVotersGenAlts = 25, distributionTypeGenAlts = "unif", distributionParametersGenAlts =  c(-1,1))
    


# 4) Create three voter data frames to test with:

generatedVoterDataFrame1 <- data.frame(voterID = seq(from = 1,to = numVotersGlobal), xIdeal=ideals[ ,1], yIdeal=ideals[ ,2], minkoOrder=minkoOrder, xSalience = salience[ ,1], xSalience = salience[ ,2] )

staticVoterDataFrame1 <- data.frame(voterID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), xIdeal=c(-.8, -.5, .2, .9, -.2, .15, -.15, -.35, .35, .32, -.44), yIdeal=c(.1, .5, -.3, 0, .9, -.7, .35, -.45, -.26, .21, -.03), minkoOrder=c(1, 2, 4, 2, 1, 1, 2, 10, 2, 1, 2), xSalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))


# 5) Calulate the Minkowski Distance and Minkowsi Utility of each voter from each alt:

altsGen1

minkoDist1 <- minkowskiDistanceSets(idealsMatrix = as.matrix(ideals), altsMatrix = altsGen1, orderVector = rep(2,nrow(ideals)), salienceMatrix = salience)

minkoUtil1 <- minkowskiUtilitySets(idealsMatrix = as.matrix(ideals), altsMatrix = altsGen2, minkoOrderVector = rep(2,nrow(ideals)), lossOrderVector =  rep(2,nrow(ideals)), salienceMatrix = salience)


altsGen2

minkoDist2 <- minkowskiDistanceSets(idealsMatrix = as.matrix(ideals), altsMatrix = altsGen2, orderVector = rep(2,nrow(ideals)), salienceMatrix = salience)


minkoUtil2 <- minkowskiUtilitySets(idealsMatrix = as.matrix(ideals), altsMatrix = altsGen2, minkoOrderVector = rep(2,nrow(ideals)), lossOrderVector =  rep(2,nrow(ideals)), salienceMatrix = salience)


# 6) Find the Pareto Set of the Group of Voters

paretoSet <- findParetoSet( cbind(generatedVoterDataFrame1$xIdeal, generatedVoterDataFrame1$yIdeal) )
                  
paretoSet


aParetoSet <- findParetoSet( cbind(staticVoterDataFrame1$xIdeal, staticVoterDataFrame1$yIdeal) )

aParetoSet


# 6) Plot the voters and Pareto Set


plotParetoSet(generatedVoterDataFrame1, idealsOn  = TRUE)

plotParetoSet(generatedVoterDataFrame1, idealsOn = FALSE)


plotParetoSet(staticVoterDataFrame1, idealsOn  = TRUE)

plotParetoSet(staticVoterDataFrame1, idealsOn = FALSE)



#7 ) Is an alternative in the Pareto Set, given the alt and the Pareto Set?

isInParetoSetFromPointAndPS(aSexpPoint = c(0,0), aSexpMatrix = as.matrix(paretoSet)) # Should be TRUE for set.seed(123)

isInParetoSetFromPointAndPS(aSexpPoint = c(.25,.5), aSexpMatrix = as.matrix(paretoSet)) # Should be FALSE for set.seed(123)


#8) Is an alternaive in the Pareto Set given the alt and a set of ideals.

isInParetoSetFromPointAndIdeals(aSexpPoint = c(0,0), aSexpMatrix = ideals) # Should be TRUE for set.seed(123)

isInParetoSetFromPointAndIdeals(aSexpPoint = c(.25,.5), aSexpMatrix = ideals) # Should be FALSE for set.seed(123)


#9) Find the Minlowski Distance between two points to verify it is the same as the superelipse code:

oneIdeal <- matrix(c(-.25,-.25),nrow = 1, ncol = 2)
oneAlt <- matrix(c(.20,.10),nrow = 1, ncol = 2)
theOrder <-  c(3)
salience <- c(1,2)
oneVoterID <- c(1)

minkowskiUtilityDistanceSets(idealsMatrix = oneIdeal, altsMatrix = oneAlt, orderVector = theOrder, salienceMatrix = matrix(salience, nrow = 1, ncol = 2))

minkowskiUtilityPairOfPoints(idealVector = as.vector(oneIdeal), altVector = as.vector(oneAlt), orderScalar = theOrder, salienceVector = salience)

testRadius <- findSuperElipseRadius(idealPoint = as.vector(oneIdeal), altPoint = as.vector(oneAlt), orderScalar = theOrder, salienceVector = salience)


# Now Find the Indifference Curve Point

# 10) Find the Preferred to Sets for a group of voters and plot them 


# Lots of heterogeneity of ideals, saliences and minkoOrders
staticVoterDataFrame2 <- data.frame(voterID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), xIdeal=c(-.8, -.5, .2, .9, -.2, .15, -.15, -.35, .35, .32, -.44), yIdeal=c(.1, .5, -.3, 0, .9, -.7, .35, -.45, -.26, .21, -.03), minkoOrder=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), xSalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))


staticVoterDataFrame3 <- data.frame(voterID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), xIdeal=c(-.8, -.5, .2, .9, -.2, .15, -.15, -.35, .35, .32, -.44), yIdeal=c(.1, .5, -.3, 0, .9, -.7, .35, -.45, -.26, .21, -.03), minkoOrder=c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), xSalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))



## First for one voter called vt
vt <- 2 

findICPoints(voterID = staticVoterDataFrame1$voterID, idealPoint = c(staticVoterDataFrame1$xIdeal[vt], staticVoterDataFrame1$yIdeal[vt]), altPointVector = c(0,0), orderScalar = staticVoterDataFrame1$minkoOrder[vt], salienceVector = c(staticVoterDataFrame1$xSalience[vt], staticVoterDataFrame1$ySalience[vt]), precision = .01)

## Now find the IC's for all the voters

voterDataFrameAllICs <- findICsForSetOfVoters(votersDataFrame = staticVoterDataFrame1, altPoint = c(0,0), precision = .01)


plotIC(votersDataFrame = staticVoterDataFrame1, altPoint = c(0,0), precision = .01)


plotIC(votersDataFrame = staticVoterDataFrame2, altPoint = c(0,0), precision = .01)


plotIC(votersDataFrame = staticVoterDataFrame3, altPoint = c(0,0), precision = .01)

    ## If you change everything to minkoOrder 1 you get Diamond Shaped ICs

        # All MinkoOrder = 1
        staticVoterDataFrameAllMinko1 <- data.frame(voterID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), xIdeal=c(-.8, -.5, .2, .9, -.2, .15, -.15, -.35, .35, .32), yIdeal=c(.1, .5, -.3, 0, .9, -.7, .35, -.45, -.26, .21), minkoOrder=c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), xSalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

        voterDataFrameAllMinko1ICs <- findICsForSetOfVoters(votersDataFrame = staticVoterDataFrameAllMinko1, altPoint = c(0,0), precision = .01)
        
        plotIC(votersDataFrame = staticVoterDataFrameAllMinko1, altPoint = c(0,0),  precision = .01)

        
    ## What about all minko Order 2?

        # All MinkoOrder = 2
staticVoterDataFrameAllMinko2 <- data.frame(voterID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), xIdeal=c(-.8, -.5, .2, .9, -.2, .15, -.15, -.35, .35, .32), yIdeal=c(.1, .5, -.3, 0, .9, -.7, .35, -.45, -.26, .21), minkoOrder=c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2), xSalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

    voterDataFrameAllMinko2ICs <- findICsForSetOfVoters(votersDataFrame = staticVoterDataFrameAllMinko2, altPoint = c(0,0), precision = .01)


        plotIC(votersDataFrame = staticVoterDataFrameAllMinko2, altPoint = c(0,0), precision = .01)
        
        
        
        ## What about all minko Order 4?
        
        # All MinkoOrder = 4
        staticVoterDataFrameAllMinko4 <- data.frame(voterID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), xIdeal=c(-.8, -.5, .2, .9, -.2, .15, -.15, -.35, .35, .32), yIdeal=c(.1, .5, -.3, 0, .9, -.7, .35, -.45, -.26, .21), minkoOrder=c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4), xSalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
        
        voterDataFrameAllMinko4ICs <- findICsForSetOfVoters(votersDataFrame = staticVoterDataFrameAllMinko4, altPoint = c(0,0), precision = .01)
        
        plotIC(votersDataFrame = staticVoterDataFrameAllMinko4, altPoint = c(0,0), precision = .01)
        

    # 11) Now lets look at the plotteRvoteR function, using a common homework question scenario:      
        
    simpleVoterDF <- data.frame(voterID = c(1, 2, 3, 4, 5), xIdeal=c(15, 20, 35, 40, 55), yIdeal=c(45, 15, 50, 35, 30), minkoOrder=c(2, 2, 2, 2, 2), xSalience = c(1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1) )
        
plotteRvoteR(votersDataFrame = simpleVoterDF, altPoints = c(30,40), plotIdeals = TRUE, plotICs = TRUE, plotPareto = TRUE, plotAlt = TRUE, yToXRatio = 1)
        

# 12) Now lets look at the plotteRvoteR function, with only 3 ideals, but with different Minko Orders:      

verySimpleVoterDF <- data.frame(voterID = c(1, 2, 3), xIdeal=c(15, 20, 35), yIdeal=c(45, 15, 50), minkoOrder=c(1, 2, 3), xSalience = c(1, 1, 1), ySalience = c(1, 1, 1) )

plotteRvoteR(votersDataFrame = verySimpleVoterDF, altPoints = c(30,40), plotIdeals = TRUE, plotICs = TRUE, plotPareto = FALSE, plotAlt = TRUE, yToXRatio = 1)


# 13) Now lets look at the plotteRvoteR function, using the staticVoterDataFrameAll:      

plotteRvoteR(votersDataFrame = staticVoterDataFrame1, altPoints = c(0,0), plotIdeals = TRUE, plotICs = TRUE, plotPareto = TRUE, plotAlt = TRUE, yToXRatio = 1)

########################## HERE #######################################################################
# 14) Is an alternaive in a set of a voter given an alt and an IC.

vt <- 1

ICToCheck <- findICPoints(voterID = staticVoterDataFrameAll$voterID, idealPoint = c(staticVoterDataFrameAll$xIdeal[vt], staticVoterDataFrameAll$yIdeal[vt]), altPointVector = c(0,0), orderScalar = staticVoterDataFrameAll$minkoOrder[vt], salienceVector = c(staticVoterDataFrameAll$xSalience[vt], staticVoterDataFrameAll$ySalience[vt]), precision = .01)

plotIC(votersDataFramePlotIC = staticVoterDataFrameAll[1, ], altPointPlotIC = c(0,0), precisionPlotIC = .01)

isInICFromPointAndIC(aSexpPoint = c(0,0), aSexpMatrix = as.matrix(ICToCheck[ ,2:3])) # Should be TRUE for set.seed(123)   ##TODO

isInICFromPointAndIC(aSexpPoint = c(-1.5,0), aSexpMatrix = as.matrix(ICToCheck[ ,2:3])) # Should be TRUE for set.seed(123)   ##TODO

isInICFromPointAndIC(aSexpPoint = c(-1.5,0.5), aSexpMatrix = as.matrix(ICToCheck[ ,2:3])) # Should be FALSE for set.seed(123)   ##TODO

isInICFromPointAndIC(aSexpPoint = c(0,1), aSexpMatrix = as.matrix(ICToCheck[ ,2:3])) # Should be FALSE for set.seed(123)   ##TODO




# 16) For a set of voters and a set of alternatives, calculate each voter's minkowski distance between their ideal point and each alterantive (This is the voters full preference ordering over the alternatives) 



minkoDist2 <- minkowskiUtilityDistanceSets(idealsMatrix = as.matrix(ideals), altsMatrix = altsGen2, orderVector = rep(2,nrow(ideals)), salienceMatrix = salience)
