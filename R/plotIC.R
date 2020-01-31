#' plotIC
#' Plots the indifference curves for the set of voterDataFrames provided. 
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
#' @return Plots the preferred to sets for all the voters in teh voter data frame.
#' 
#' @export
plotIC <- function(votersDataFramePlotIC, altPointPlotIC, precisionPlotIC){
    
    
    # ### FOR TESTING ###
    # votersDataFramePlotIC <- data.frame(pointType = rep(x = "voter", 3), ID = c("V-1", "V-2", "V-3"), xLocation=c(-1/8, 7/8, 4/8), yLocation=c(3/8, 4/8, -3/8), minkoOrder=c(1, 2, 100), xSalience = c(1, 1, 1), ySalience = c(1, 1, 1), lossOrder = c(1, 2, 1) )
    # altPointPlotIC <- c(1/8,1/8)
    # precisionPlotIC <- .01
    # 
    # ### FOR TESTING ###
    # 
    
    allVotersICPointsList <- findICsForSetOfVoters(votersDataFrame = votersDataFramePlotIC, altPoint = altPointPlotIC, precision = precisionPlotIC) 
    
    allVotersPrefToSetsDF <- do.call(rbind.data.frame, allVotersICPointsList)
    
    allVotersPrefToSetsDF$voterID <- as.factor(allVotersPrefToSetsDF$voterID)
    
    idealPoints <- data.frame(voterID = votersDataFramePlotIC$ID, xIdeal=votersDataFramePlotIC$xLocation, yIdeal=votersDataFramePlotIC$yLocation)
    
    altPointDF <- data.frame(xSQ=altPointPlotIC[1], ySQ=altPointPlotIC[2])
    
    idealPoints$voterID <- as.factor(idealPoints$voterID)
    
    #### THERE IS A PROBLEM HERE ###
    #### 01.30.2020 ##############
    
    ICPlot <- ggplot2::ggplot() + 
        ggplot2::geom_polygon(data = allVotersPrefToSetsDF, mapping = ggplot2::aes(group = voterID, color = voterID, fill = voterID, x = xCoords, y = yCoords), alpha = 1/2) + 
        ggplot2::geom_point(data = idealPoints, mapping = ggplot2::aes(color = voterID, x = xIdeal, y = yIdeal), size = 3) +
        ggplot2::geom_point(data = altPointDF, mapping = ggplot2::aes(x = xSQ, y = ySQ), size = 3) +
        ggplot2::coord_fixed()
    
    ICPlot
} 
## Note this could be sped up by moving the loop over findICPoints() that finds the IC for each voter directly into a Cpp function. 
