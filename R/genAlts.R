#' genAlts
#' Generates a set of alternatives or a single status quo.
#'  
#' Generates one or more one or two dimensional alternatives from one of given set of distributions, from calibrated data, or from a user provided distribution. 
#' it is a wrapper for \code{\link{genPoints}}. 
#' 
#' @param numberOfDimensionsGenAlts The number of policy dimensions.
#' @param numberOfAltsGenAlts Number of points to generate.
#' @param distributionTypeGenAlts A string identifying the base R discribution to draw
#'   the ideal points from. Uses the base R random number generation family of
#'   commands rxxxx (see ?distributions). The user should specify the
#'   distribution as a string using the standard R abreviation for the
#'   distribution (see ?distributions for a list). Currently supported are: "norm",
#'   "unif", "binom", "cauchy", "chisq", "weibull"
#' @param distributionParametersGenAlts A vector that contains the additional
#'   parameters needed by the particular rxxxx function for a distribtuion. (see
#'   ?rxxxx where xxxx is a function listed under ?distribution). Example for a
#'   Normal(0,1), use: c(0,1).
#' @param dimOneBoundsGenAlts A vector that contains the starting and ending poitns of t
#' he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf) if no boundary is provided. 
#' @param dimTwoBoundsGenAlts A vector that contains the starting and ending poitns of t
#' he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf)  if no boundary is provided.
#' @return outIdeals An ideal point matrix that is numVoters x numDimensions
#' @examples
#'   genAlts(numberOfDimensionsGenAlts=2, numberOfAltsGenAlts=10, distributionTypeGenAlts ="norm", distributionParametersGenAlts = c(0,1), dimOneBoundsGenAlts = c(-Inf,0), dimTwoBoundsGenAlts = c(0,Inf))
#'  genAlts(numberOfDimensionsGenAlts=1, numberOfAltsGenAlts=100, distributionTypeGenAlts ="unif", distributionParametersGenAlts = c(0,10)) 
#'  
#'   genAlts(numberOfDimensionsGenAlts=1, numberOfAltsGenAlts=100, distributionTypeGenAlts ="unif", distributionParametersGenAlts = c(-1,1))
#' @export
genAlts <- function(numberOfDimensionsGenAlts=1, numberOfAltsGenAlts=5, distributionTypeGenAlts ="unif", distributionParametersGenAlts = c(-1,1), dimOneBoundsGenAlts = c(-Inf,Inf), dimTwoBoundsGenAlts = c(-Inf,Inf)){
    
    tempPoints <- genPoints(numberOfDimensionsGenPoints = numberOfDimensionsGenAlts, numberOfPointsGenPoints = numberOfAltsGenAlts, distributionTypeGenPoints = distributionTypeGenAlts, distributionParametersGenPoints = distributionParametersGenAlts, dimOneBoundsGenPoints = dimOneBoundsGenAlts, dimTwoBoundsGenPoints = dimTwoBoundsGenAlts)

    
    #2) Create a vector of the pointType called "alternative"
    
    pointType <- rep("alternative", numberOfAltsGenAlts)
    
    #4 Create alternativeIDs
    
    alternativeID = paste( "A",seq(from = 1,to = numberOfAltsGenAlts), sep="-" )
    

    #4) Store Everything in a Data Frame 
    
    if(numberOfDimensionsGenAlts==1){
        outAlternativeDataFrame <- data.frame(pointType, alternativeID, xLocation=tempPoints)    
    }
    
    if(numberOfDimensionsGenAlts==2){
        outAlternativeDataFrame <- data.frame(pointType, alternativeID, xLocation=tempPoints[ ,1], yLocation=tempPoints[ ,2])    
    }
    
    # # Rename the x and y coordinates to xLocation and yLocation
    # names(outCompetitorDataFrame)[names(outCompetitorDataFrame) == 'xIdeal'] <- 'xLocation'
    # names(outCompetitorDataFrame)[names(outCompetitorDataFrame) == 'yIdeal'] <- 'yLocation'
    
    outAlternativeDataFrame
    
    
    
    
    
    }   
