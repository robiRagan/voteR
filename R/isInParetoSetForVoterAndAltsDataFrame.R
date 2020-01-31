#' isInParetoSetForVoterAndAltsDataFrame
#' Finds the indifference curves for the set of voterDataFrames provided. 
#' 
#' @param alternativeToCheck An R vector that contains the corrdinates of the alternative that you want to check.
#' 
#' @param votersDataFrame The voters data frame must have a specific format, and it must be an R data.frame object. There must be at least these 6 variables and they must have the following names. The order of the variables in the data.frame is not important as long as the variables have the proper names.
#' 
#'  ID: A numeric identifier unique to the voter.
#'  xLocation: The x coordinate of the voter's ideal point.
#'  yLocation: The y coordinate of the voter's ideal point.
#'  minkoOrder: The Minkowski order of the voters MInkowski metric based utility function. = 1, is City Block. = 2 is Euclidian and 100 = is  See ?Minkowski. 
#'  xSalience: The salience of the x dimension for the voter. The dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#'  ySalience: The salience of the y dimension for the voter. he dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#'  lossOrder: The loss order for the agents utility function. See the parameter lossOrderVector in ?minkowskiUtilitySets().
#' 
#' #' @param alternativesDataFrame The alternatives data frame must have a specific format, and it must be an R data.frame object. There must be at least these 3 variables and they must have the following names. The order of the variables in the data.frame is not important as long as the variables have the proper names.
#' 
#'  ID: A numeric identifier unique to the alternative.
#'  xLocation: The x coordinate of the alternative's location.
#'  yLocation: The y coordinate of the alternative's location. 
#' 
#' @return DataFrame with two columns the first is the Alternatives ID and the second is a logical that is TRUE if the alt is in the pareto set and FALSE of the alt is not in the pareto set.
#' 
#' @export
isInParetoSetForVoterAndAltsDataFrame <- function(alternativesDataFrame ,votersDataFrame){
    
    
    # ### FOR TESTING ##
    # alternativesDataFrame <- data.frame(pointType = rep(x = "alternative", 3), ID = c("A-1", "A-2", "A-3"), xLocation=c(-3/8, 1/8, 2/8), yLocation=c(-3/8, 1/8, 7/8) )
    # 
    # votersDataFrame <- data.frame(pointType = rep(x = "voter", 3), ID = c("V-1", "V-2", "V-3"), xLocation=c(-1/8, 7/8, 4/8), yLocation=c(3/8, 4/8, -3/8), minkoOrder=c(1, 2, 100), xSalience = c(1, 1, 1), ySalience = c(1, 1, 1), lossOrder = c(1, 2, 1) )
    # ### FOR TESTING ##
    
    theParetoSet <- findParetoSetForDataFrame(votersDataFrame)

    # Create Output Matrix
    isInParetoSetOut <- data.frame(ID=alternativesDataFrame$ID, inParetoSet=rep(NA,nrow(alternativesDataFrame) ) )
    
    # Check each Alt
    for (i in 1:nrow(alternativesDataFrame) ){
        
        isInParetoSetOut[i, 2] <- isInParetoSetFromPointAndPS(  aSexpPoint =  cbind(alternativesDataFrame$xLocation, alternativesDataFrame$yLocation)[i, ], 
                                                                aSexpMatrix = as.matrix(theParetoSet) 
                                                            ) 
    
        } # Ends for loop
        
    isInParetoSetOut
} 
