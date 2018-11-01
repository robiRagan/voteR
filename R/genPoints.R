#' genPoints
#' Generates one or two dimensional points.
#'  
#' Generates one or two dimensional points from one of distributions in base R.
#' The user can set up bounds on the dimensions. If bounds are set then genPoints() 
#' will discard any points outside the bounds and resample. 
#' 
#' Note that if the user chooses to set bounds on one or more dimensions and the parameters 
#' and distribution chosen by user generates many poitns outside the bounds, then genPoints()
#' can take a long time to generate the set of points. 
#' This is rarely called directly and is wrapped \code{\link{genIdeals}} and \code{\link{genAlts}}. \code{\link{genIdeals}} is further wrapped by 
#' \code{\link{genVoters}}. 
#' @section TODO:
#' Add calibration functionality and move this into C++.
#' Maybe add a warning if the user sets bounds and the resampling takes a long time. 
#' 
#' @param numberOfDimensionsGenPoints The number of policy dimensions.
#' @param numberOfPointsGenPoints Number of points to generate.
#' @param distributionTypeGenPoints A string identifying the base R discribution to draw
#'   the ideal points from. Uses the base R random number generation family of
#'   commands rxxxx (see ?distributions). The user should specify the
#'   distribution as a string using the standard R abreviation for the
#'   distribution (see ?distributions for a list). Currently supported are: "norm",
#'   "unif", "beta", "cauchy", "chisq", "weibull"
#' @param distributionParametersGenPoints A vector that contains the additional
#'   parameters needed by the particular rxxxx function for a distribtuion. (see
#'   ?rxxxx where xxxx is a function listed under ?distribution). Example for a
#'   Normal(0,1), use: c(0,1).
#' @param dimOneBoundsGenPoints A vector that contains the starting and ending poitns of t
#' he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf) if no boundary is provided. 
#' @param dimTwoBoundsGenPoints A vector that contains the starting and ending poitns of t
#' he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf)  if no boundary is provided. 
#' @return outIdeals An ideal point matrix that is numVoters x numDimensions
#' @examples
#'   genPoints(numberOfDimensionsGenPoints = 2, numberOfPointsGenPoints = 100, distributionTypeGenPoints = "norm", distributionParametersGenPoints = c(0,.2), dimOneBoundsGenPoints = c(0,1), dimTwoBoundsGenPoints = c(-1,1))
#'   
#'   genPoints(numberOfDimensionsGenPoints = 2, numberOfPointsGenPoints = 100, distributionTypeGenPoints = "beta", distributionParametersGenPoints = c(.1,.1), dimOneBoundsGenPoints = c(0,1), dimTwoBoundsGenPoints = c(0,1))
#'   
#'   genPoints(numberOfDimensionsGenPoints = 1, numberOfPointsGenPoints = 100, distributionTypeGenPoints = "unif", distributionParametersGenPoints = c(-1,1))
#'   
#'   
#'   
#' @export
# numberOfDimensionsGenPoints <- 2              ## FOR TESTING
# numberOfPointsGenPoints <- 100                ## FOR TESTING
# distributionTypeGenPoints <- "beta"          ## FOR TESTING
# distributionParametersGenPoints <- c(.1,.1)   ## FOR TESTING
# dimOneBoundsGenPoints <- c(0,1)               ## FOR TESTING
# dimTwoBoundsGenPoints <- c(0,1)               ## FOR TESTING

genPoints <- function(numberOfDimensionsGenPoints=1, numberOfPointsGenPoints=100, distributionTypeGenPoints ="unif", dimOneBoundsGenPoints = c(-Inf,Inf), dimTwoBoundsGenPoints = c(-Inf,Inf), distributionParametersGenPoints = c(-1,1)){

    if(numberOfDimensionsGenPoints==1){
    dimOne <- truncdist::rtrunc( n = (numberOfPointsGenPoints*numberOfDimensionsGenPoints), spec = distributionTypeGenPoints, a = dimOneBoundsGenPoints[1], b = dimOneBoundsGenPoints[2], distributionParametersGenPoints[1], distributionParametersGenPoints[2])
    rawPoints <- cbind(dimOne) 
    }
    
# rDistn <- match.fun(paste("r",distributionTypeGenPoints, sep=""))
    if(numberOfDimensionsGenPoints==2){
dimOne <- truncdist::rtrunc( n = (numberOfPointsGenPoints*numberOfDimensionsGenPoints)/2, spec = distributionTypeGenPoints, a = dimOneBoundsGenPoints[1], b = dimOneBoundsGenPoints[2], distributionParametersGenPoints[1], distributionParametersGenPoints[2]) 
dimTwo <- truncdist::rtrunc( n = (numberOfPointsGenPoints*numberOfDimensionsGenPoints)/2, spec = distributionTypeGenPoints, a = dimTwoBoundsGenPoints[1], b = dimTwoBoundsGenPoints[2], distributionParametersGenPoints[1], distributionParametersGenPoints[2])
rawPoints <- cbind(dimOne,dimTwo)
}
rawPoints
}   
