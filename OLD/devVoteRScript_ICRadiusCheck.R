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
# Older Plot Function


#' @param votersDataFrame The voters data frame must have a specific format, and it must be an R data.frame object. There must be 6 variables and they must have the following names. The order of the variables in the data.frame is not important as long as the variables have the proper names.
#' 
#'  voterID: A numeric identifier unique to the voter.
#'  xIdeal: The x coordinate of the voter's ideal point.
#'  yIdeal: The y coordinate of the voter's ideal point.
#'  minkoOrder: The Minkowski order of the voters MInkowski metric based utility function. = 1, is City Block. = 2 is Euclidian and 100 = is  See ?Minkowski. 
#'  xSalience: The salience of the x dimension for the voter. The dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#'  ySalience: The salience of the y dimension for the voter. he dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#'  
#' @param altPoint The alternative that the voter's are evaluating. The indifference curves are the set of all points that gives the voter the same utility as this point. 
#' 
#' @param nPointsPerIC The number of points to use to trace out the voters' indifference curves. The higher the number the more accurate and smooth the indifference curve will be, but for large numbers of voters larger numbers will slow down plotting. 
#' 
#' @return Plots the preferred to sets for all the voters in teh voter data frame.
#' 
#' @export

plotPrefToSets <- function(votersDataFrame, statusQuo, nPointsPerIC){

    # create
    allVotersPrefToSetsList <- list()

    for (j in 1:nrow(votersDataFrame)){
        allVotersPrefToSetsList[[j]] <- findPrefToSetPoints(voterID = votersDataFrame$voterID[j], idealPoint = c(votersDataFrame$xIdeal[j], votersDataFrame$yIdeal[j]), orderScalar = votersDataFrame$minkoOrder[j], salienceVector = c(votersDataFrame$xSalience[j],votersDataFrame$ySalience[j]), altPoint = statusQuo, numberOfPoints = nPointsPerIC)
    }

allVotersPrefToSetsDF <- do.call(rbind.data.frame, allVotersPrefToSetsList)

allVotersPrefToSetsDF$V1 <- as.factor(allVotersPrefToSetsDF$V1)

idealPoints <- data.frame(voterID = votersDataFrame$voterID, xIdeal=votersDataFrame$xIdeal, yIdeal=votersDataFrame$yIdeal)

statusQuoDF <- data.frame(xSQ=statusQuo[1], ySQ=statusQuo[2])

idealPoints$voterID <- as.factor(idealPoints$voterID)

ICPlot <- ggplot() + 
    geom_polygon(data = allVotersPrefToSetsDF, mapping = aes(group = V1, fill = V1, x = V2, y = V3), alpha = 1/2) + 
    geom_point(data = idealPoints, mapping = aes(color = voterID,x = xIdeal, y = yIdeal), size = 3) +
    geom_point(data = statusQuoDF, mapping = aes(x = xSQ, y = ySQ), size = 3) +
    coord_fixed()

ICPlot
}



# New Plot Function
        
        plotPrefToSets2 <- function(votersDataFrame, statusQuo, precision){
            
            # create
            allVotersPrefToSetsList <- list()
            
            for (j in 1:nrow(votersDataFrame)){
                allVotersPrefToSetsList[[j]] <- altFindPrefToSetPointsTwo(voterID = votersDataFrame$voterID[j], idealPoint = c(votersDataFrame$xIdeal[j], votersDataFrame$yIdeal[j]), orderScalar = votersDataFrame$minkoOrder[j], salienceVector = c(votersDataFrame$xSalience[j], votersDataFrame$ySalience[j]), altPoint = statusQuo, precision = .01)
            }
            
            allVotersPrefToSetsDF <- do.call(rbind.data.frame, allVotersPrefToSetsList)
            
            allVotersPrefToSetsDF$V1 <- as.factor(allVotersPrefToSetsDF$V1)
            
            idealPoints <- data.frame(voterID = votersDataFrame$voterID, xIdeal=votersDataFrame$xIdeal, yIdeal=votersDataFrame$yIdeal)
            
            statusQuoDF <- data.frame(xSQ=statusQuo[1], ySQ=statusQuo[2])
            
            idealPoints$voterID <- as.factor(idealPoints$voterID)
            
            ICPlot <- ggplot() + 
                geom_polygon(data = allVotersPrefToSetsDF, mapping = aes(group = V1, fill = V1, x = V2, y = V3), alpha = 1/2) + 
                geom_point(data = idealPoints, mapping = aes(color = voterID,x = xIdeal, y = yIdeal), size = 3) +
                geom_point(data = statusQuoDF, mapping = aes(x = xSQ, y = ySQ), size = 3) +
                coord_fixed()
            
            ICPlot
        }
        
      
        
        
## Plot Function for Comparing
        
        
        
        
        plotPrefToSets3 <- function(votersDataFrame, statusQuo, precision, nPointsPerIC){
            
            # create
            allVotersPrefToSetsList <- list()
            
            for (j in 1:nrow(votersDataFrame)){
                allVotersPrefToSetsList[[j]] <- altFindPrefToSetPointsTwo(voterID = votersDataFrame$voterID[j], idealPoint = c(votersDataFrame$xIdeal[j], votersDataFrame$yIdeal[j]), orderScalar = votersDataFrame$minkoOrder[j], salienceVector = c(votersDataFrame$xSalience[j], votersDataFrame$ySalience[j]), altPoint = statusQuo, precision = .01)
            }
            
            allVotersPrefToSetsDF2 <- do.call(rbind.data.frame, allVotersPrefToSetsList)
            
            allVotersPrefToSetsDF2$V1 <- as.factor(allVotersPrefToSetsDF2$V1)
            
            idealPoints <- data.frame(voterID = votersDataFrame$voterID, xIdeal=votersDataFrame$xIdeal, yIdeal=votersDataFrame$yIdeal)
            
            statusQuoDF <- data.frame(xSQ=statusQuo[1], ySQ=statusQuo[2])
            
            idealPoints$voterID <- as.factor(idealPoints$voterID)
            
            
            

            allVotersPrefToSetsList1 <- list()
            
            for (j in 1:nrow(votersDataFrame)){
                allVotersPrefToSetsList1[[j]] <- findPrefToSetPoints(voterID = votersDataFrame$voterID[j], idealPoint = c(votersDataFrame$xIdeal[j], votersDataFrame$yIdeal[j]), orderScalar = votersDataFrame$minkoOrder[j], salienceVector = c(votersDataFrame$xSalience[j],votersDataFrame$ySalience[j]), altPoint = statusQuo, numberOfPoints = nPointsPerIC)
            }
            
            allVotersPrefToSetsDF1 <- do.call(rbind.data.frame, allVotersPrefToSetsList1)
            
            allVotersPrefToSetsDF1$V1 <- as.factor(allVotersPrefToSetsDF1$V1+1)
            

            # Stack the two IC's into a single DF.
            allVotersPrefToSetsDF <- rbind(allVotersPrefToSetsDF2, allVotersPrefToSetsDF1)
            
            ICPlot <- ggplot() + 
                geom_polygon(data = allVotersPrefToSetsDF, mapping = aes(group = V1, fill = V1, x = V2, y = V3), alpha = 1/2) + 
                geom_point(data = idealPoints, mapping = aes(color = voterID,x = xIdeal, y = yIdeal), size = 3) +
                geom_point(data = statusQuoDF, mapping = aes(x = xSQ, y = ySQ), size = 3) +
                coord_fixed()

            
            ICPlot
        }
        
        
        
        aVoterDataFrame3 <- data.frame(voterID = c(3), xIdeal=c(.2), yIdeal=c(-.3), minkoOrder=c(4), xSalience = c(1), ySalience = c(1))
        
        plotPrefToSets(votersDataFrame = aVoterDataFrame3, statusQuo = c(0,0), nPointsPerIC =  1000)
        
        plotPrefToSets2(votersDataFrame = aVoterDataFrame3, statusQuo = c(0,0), precision = .01)
        
        plotPrefToSets3(votersDataFrame = aVoterDataFrame3, statusQuo = c(0,0), precision = .01, nPointsPerIC =  1000)     
        
        
        
        
        aVoterDataFrame6 <- data.frame(voterID = c(6), xIdeal=c(.15), yIdeal=c(-.7), minkoOrder=c(1), xSalience = c(1), ySalience = c(1))
        
        plotPrefToSets(votersDataFrame = aVoterDataFrame6, statusQuo = c(0,0), nPointsPerIC =  1000)
        
        plotPrefToSets2(votersDataFrame = aVoterDataFrame6, statusQuo = c(0,0), precision = .01)
        
        plotPrefToSets3(votersDataFrame = aVoterDataFrame6, statusQuo = c(0,0), precision = .01, nPointsPerIC =  1000)    
        
        
        
        aVoterDataFrame9 <- data.frame(voterID = c(9), xIdeal=c(.35), yIdeal=c(-.26), minkoOrder=c(2), xSalience = c(1), ySalience = c(1))
        
        plotPrefToSets(votersDataFrame = aVoterDataFrame9, statusQuo = c(0,0), nPointsPerIC =  1000)
        
        plotPrefToSets2(votersDataFrame = aVoterDataFrame9, statusQuo = c(0,0), precision = .01)
        
        plotPrefToSets3(votersDataFrame = aVoterDataFrame9, statusQuo = c(0,0), precision = .01, nPointsPerIC =  1000)  
        
        
      
    # Ok lets try a simple 3 agent case with 2 of the agents having ICs that were problamatic for the older IC finder. 
        
    aVoterDataFrame3and6and9 <- data.frame(voterID = c(3,6,9), xIdeal=c(.2,.15,.35), yIdeal=c(-.3,-.7,-.26), minkoOrder=c(4,1,2), xSalience = c(1,1,1), ySalience = c(1,1,1))
    
    plotPrefToSets2(votersDataFrame = aVoterDataFrame3and6and9, statusQuo = c(0,0), precision = .01)
    
    plotPrefToSets3(votersDataFrame = aVoterDataFrame3and6and9, statusQuo = c(0,0), precision = .01, nPointsPerIC=1000)
        