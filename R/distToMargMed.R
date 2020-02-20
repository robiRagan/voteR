#' distToMargMed
#' Finds the distance between a set of points and the marginal median. 
#' 
#' The marginal median is the vector of unidimensional medians.
#' @param competitorDataFrameDistToMargMed competitor data frame
#' @param marginalMedianDistToMargMedian the marginal median
#' @return outDistanceToMarginalMedians 
#' @export
distToMargMed <- function(competitorDataFrameDistToMargMed, marginalMedianDistToMargMedian){
    
    outDistToMargMed <- data.frame(matrix(rep(NA,nrow(currentCompetitorPositions)), nrow = 1))
    names(outDistToMargMed) <- competitorDataFrameDistToMargMed$competitorID
    
    for (i in 1:nrow(competitorDataFrameDistToMargMed) ){
        outDistToMargMed[i] <- dist(rbind(c(competitorDataFrameDistToMargMed$xLocation[i],competitorDataFrameDistToMargMed$yLocation[i]), c(marginalMedianDistToMargMedian$xMedian, marginalMedianDistToMargMedian$yMedian) ))
    }
    
    outDistToMargMed

    }   
