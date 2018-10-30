#' plotParetoSet
#' Plots the Pareto Set for a set of supplied ideal points. 
#' 
#' @param votersDataFrame The voters data frame must have a specific format, and it must be an R data.frame object. There must be at least these 6 variables and they must have the following names. The order of the variables in the data.frame is not important as long as the variables have the proper names.
#' 
#'  voterID: A numeric identifier unique to the voter.
#'  xIdeal: The x coordinate of the voter's ideal point.
#'  yIdeal: The y coordinate of the voter's ideal point.
#'  minkoOrder: The Minkowski order of the voters Minkowski metric based utility function. = 1, is City Block. = 2 is Euclidian and 100 = is  See ?Minkowski. 
#'  xSalience: The salience of the x dimension for the voter. The dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#'  ySalience: The salience of the y dimension for the voter. he dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#'
#' @param idealsOn If TRUE the ideal points are also plotted with the Pareto Set. If false the ideal points are not plotted with the Pareto Set. 
#'  
#' @return Plots the pareto set for all the voters in the dataFrame.
#' 
#' @export
plotParetoSet <- function(votersDataFrame, idealsOn = TRUE){
    
    justIdealPoints = cbind(votersDataFrame$xIdeal, votersDataFrame$yIdeal)
    
    paretoSetDF <- findParetoSet(justIdealPoints)
    
    votersDataFrame$voterID <- as.factor(votersDataFrame$voterID)
    
    if (idealsOn == TRUE){
        paretoPlot <- ggplot() + 
            geom_polygon(data = paretoSetDF, mapping = aes(x = V1, y = V2, alpha = 1/8) ) + 
            geom_point(data = votersDataFrame, mapping = aes(color = voterID, x = xIdeal, y = yIdeal), size = 2) +
            coord_fixed() + scale_alpha(labels = "") +  labs(x = "Dimension 1", y = "Dimension 2", color = "Voters", alpha = "Pareto Set")
    } else {
        paretoPlot <- ggplot() + 
            geom_polygon(data = paretoSetDF, mapping = aes(x = V1, y = V2, alpha = 1/8) ) + 
            coord_fixed() + scale_alpha(labels = "") + labs(x = "Dimension 1", y = "Dimension 2", color = "Voters", alpha = "Pareto Set")
    }
    
    paretoPlot
} 
