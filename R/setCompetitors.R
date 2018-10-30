#' setCompetitors
#' Properly formats a user supplied a set of competitors. 
#' 
#' Takes a set of competitors and puts them in the proper format for voteR to use. The user should supply the competitors as
#' a numberOfCompetitors x numOfDimensions matrix.
#' @param matrixOfCompetitors a numberOfCompetitors x numOfDimensions matrix containing the location of the competitors 
#' @return outCompetitorDataFrame 
#' @examples
#'   setCompetitors(matrixOfCompetitors = cbind( c(.1,.2,.3,.4), c(.5,.6,.7,.8) ) )
#' @export
setCompetitors <- function(matrixOfCompetitors=NA){
    
    tempPoints <- matrixOfCompetitors
    
    numberOfDimensionsSetCompetitors <- ncol(matrixOfCompetitors)
    
    numberOfCompetitorsSetCompetitors <- nrow(matrixOfCompetitors)
    #2) Create a vector of the pointType called "alternative"
    
    pointType <- rep("competitor", numberOfCompetitorsSetCompetitors )
    
    #4 Create CompetitorIDs
    
    competitorID = paste( "C",seq(from = 1,to = numberOfCompetitorsGenCompetitors), sep="-" )
    
    
    #5) Store Everything in a Data Frame 
    
    if(numberOfDimensionsSetCompetitors==1){
        outCompetitorDataFrame <- data.frame(pointType, competitorID, xLocation=tempPoints)    
    }
    
    if(numberOfDimensionsSetCompetitors==2){
        outCompetitorDataFrame <- data.frame(pointType, competitorID, xLocation=tempPoints[ ,1], yLocation=tempPoints[ ,2])    
    }
    
    # # Rename the x and y coordinates to xLocation and yLocation
    # names(outCompetitorDataFrame)[names(outCompetitorDataFrame) == 'xIdeal'] <- 'xLocation'
    # names(outCompetitorDataFrame)[names(outCompetitorDataFrame) == 'yIdeal'] <- 'yLocation'
    
    outCompetitorDataFrame
    
    
    }   
