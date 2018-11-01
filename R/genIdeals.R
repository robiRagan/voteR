#' genIdeals
#' Generates ideal points for a set of voters.
#'  
#' Generates one or two dimensional ideal points from one of given set of distributions, from calibrated data, or from a user provided distribution. 
#' it is a wrapper for \code{\link{genPoints}} and is itself further wrapped by \code{\link{genVoters}}. 
#' 
#' @param numberOfDimensionsGenIdeals The number of policy dimensions.
#' @param numberOfIdealsGenIdeals Number of points to generate.
#' @param distributionTypeGenIdeals A string identifying the base R discribution to draw
#'   the ideal points from. Uses the base R random number generation family of
#'   commands rxxxx (see ?distributions). The user should specify the
#'   distribution as a string using the standard R abreviation for the
#'   distribution (see ?distributions for a list). Currently supported are: "norm",
#'   "unif", "beta", "cauchy", "chisq", "weibull"
#' @param distributionParametersGenIdeals A vector that contains the additional
#'   parameters needed by the particular rxxxx function for a distribtuion. (see
#'   ?rxxxx where xxxx is a function listed under ?distribution). Example for a
#'   Normal(0,1), use: c(0,1).
#' @param dimOneBoundsGenIdeals A vector that contains the starting and ending poitns of t
#' he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf) if no boundary is provided. 
#' @param dimTwoBoundsGenIdeals A vector that contains the starting and ending poitns of t
#' he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf)  if no boundary is provided. 
#' @return outIdeals An ideal point matrix that is numVoters x numDimensions
#' @examples
#'   genIdeals(numberOfDimensionsGenIdeals=1, numberOfIdealsGenIdeals=100, distributionTypeGenIdeals ="norm", distributionParametersGenIdeals = c(0,.5), dimOneBoundsGenIdeals = c(0,Inf), dimTwoBoundsGenIdeals = c(-Inf,0))
#'   
#'   genIdeals(numberOfDimensionsGenIdeals=2, numberOfIdealsGenIdeals=100, distributionTypeGenIdeals ="unif", distributionParametersGenIdeals = c(-1,1), dimOneBoundsGenIdeals = c(-Inf,Inf), dimTwoBoundsGenIdeals = c(-Inf,Inf) )
#'   
#'  genIdeals(numberOfDimensionsGenIdeals=2, numberOfIdealsGenIdeals=100, distributionTypeGenIdeals ="beta", distributionParametersGenIdeals = c(.1,1), dimOneBoundsGenIdeals = c(-Inf,Inf), dimTwoBoundsGenIdeals = c(-Inf,Inf) )
#'   
#' @export
genIdeals <- function(numberOfDimensionsGenIdeals=1, numberOfIdealsGenIdeals=100, distributionTypeGenIdeals ="unif", distributionParametersGenIdeals = c(-1,1), dimOneBoundsGenIdeals = c(-Inf,Inf), dimTwoBoundsGenIdeals = c(-Inf,Inf)){
    genPoints(numberOfDimensionsGenPoints = numberOfDimensionsGenIdeals, numberOfPointsGenPoints = numberOfIdealsGenIdeals, distributionTypeGenPoints = distributionTypeGenIdeals, distributionParametersGenPoints = distributionParametersGenIdeals, dimOneBoundsGenPoints = dimOneBoundsGenIdeals, dimTwoBoundsGenPoints = dimTwoBoundsGenIdeals)
}   
