#' setAlts
#' Properly formats a user supplied a set of alternatives. 
#' 
#' Takes a set of alternatives and puts them in the proper format. 
#' The user should supply the alternatives as
#' a numberOfALts x numOfDimensions matrix.
#' @param matrixOfAlts a numberOfALts x numOfDimensions matrix containing the location of the alternatives. 
#' @return outAlternativeDataFrame 
#' @examples
#'   setAlts(matrixOfAlts = cbind( c(0), c(0) ) )
#'   setAlts(matrixOfAlts = cbind( c(.1,.2,.3,.4), c(.5,.6,.7,.8) ) )
#' @export
setAlts <- function(matrixOfAlts=NA){
    
    tempPoints <- matrixOfAlts
    
    numberOfDimensionsSetAlts <- ncol(matrixOfAlts)
    
    numberOfAltsSetAlts <- nrow(matrixOfAlts)
    #2) Create a vector of the pointType called "alternative"
    
    pointType <- rep("alternative", numberOfAltsSetAlts )
    
    #4 Create alternativeIDs
    
    alternativeID = paste( "A",seq(from = 1,to = numberOfAltsGenAlts), sep="-" )
    
    
    #5) Store Everything in a Data Frame 
    
    if(numberOfDimensionsSetAlts==1){
        outAlternativeDataFrame <- data.frame(pointType, alternativeID, xLocation=tempPoints)    
    }
    
    if(numberOfDimensionsSetAlts==2){
        outAlternativeDataFrame <- data.frame(pointType, alternativeID, xLocation=tempPoints[ ,1], yLocation=tempPoints[ ,2])    
    }
    
    # # Rename the x and y coordinates to xLocation and yLocation
    # names(outCompetitorDataFrame)[names(outCompetitorDataFrame) == 'xIdeal'] <- 'xLocation'
    # names(outCompetitorDataFrame)[names(outCompetitorDataFrame) == 'yIdeal'] <- 'yLocation'
    
    outAlternativeDataFrame

    }   
