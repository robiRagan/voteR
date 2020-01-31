#' findICsForSetOfVoters
#' Finds the indifference curves for the set of voterDataFrames provided. 
#' 
#' @param votersDataFrame The voters data frame must have a specific format, and it must be an R data.frame object. There must be at least these 6 variables and they must have the following names. The order of the variables in the data.frame is not important as long as the variables have the proper names.
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
#' @param percision How precisely you want to trace out the voters' indifference curves. The lower the number the more accurate and smooth the indifference curve will be, but for large numbers of voters larger numbers will slow down plotting. Default: precision = .01
#' 
#' @return A list where each element of the list contains the coordinantes of the voter's indfference curve.
#' 
#' @export
findICsForSetOfVoters <- function(votersDataFrame, altPoint, precision){
    
    # ### FOR TESTING ###
    # votersDataFrame <- data.frame(pointType = rep(x = "voter", 3), ID = c("V-1", "V-2", "V-3"), xLocation=c(-1/8, 7/8, 4/8), yLocation=c(3/8, 4/8, -3/8), minkoOrder=c(1, 2, 100), xSalience = c(1, 1, 1), ySalience = c(1, 1, 1), lossOrder = c(1, 2, 1) )
    # altPoint <- c(1/8,1/8)
    # precision <- .01
    # 
    # ### FOR TESTING ###
    
    # create
    allVotersICPointsList <- list()
    
    for (j in 1:nrow(votersDataFrame)){
        allVotersICPointsList[[j]] <- findICPoints(voterID = votersDataFrame$ID[j], 
                                                    idealPoint = c(votersDataFrame$xLocation[j], votersDataFrame$yLocation[j]), 
                                                    orderScalar = votersDataFrame$minkoOrder[j], 
                                                    salienceVector = c(votersDataFrame$xSalience[j], votersDataFrame$ySalience[j]), 
                                                    altPointVector = altPoint, 
                                                    precision = .01)
    }
    
#    allVotersPrefToSetsDF <- do.call(rbind.data.frame, allVotersICPointsList)
    
#    allVotersPrefToSetsDF$voterID <- as.factor(allVotersPrefToSetsDF$voterID)
    
    allVotersICPointsList
} 
## Note this could be sped up by moving the loop over findICPoints() that finds the IC for each voter directly into a Cpp function. 
