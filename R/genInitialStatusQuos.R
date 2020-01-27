#' genInitialStatusQuos
#' Generates a set of inital status quos for all of the "runs" of the model. 
#'  This allows for a comparision of how the policies develop across the different electoral systems
#' 
#' Generates a set of initial staus quo policies, each with a initialStatusQuoID number, an initial location using \code{\link{genIdeals}}.
#'  
#' @param numberOfDimensionsGenInitialStatusQuos scalar Number of policy dimensions. Can be 1 or 2. 
#' @param numberOfInitialStatusQuosGenInitialStatusQuos Number of competitors to generate.
#' @param distributionTypeGenInitialStatusQuos A string identifying the base R discribution to draw
#'   the ideal points from. Uses the base R random number generation family of
#'   commands rxxxx (see ?distributions). The user should specify the
#'   distribution as a string using the standard R abreviation for the
#'   distribution (see ?distributions for a list). Currently supported are: "norm",
#'   "unif", "beta", "cauchy", "chisq", "weibull"
#' @param distributionParametersGenInitialStatusQuos A vector that contains the additional
#'   parameters needed by the particular rxxxx function for a distribtuion. (see
#'   ?rxxxx where xxxx is a function listed under ?distribution). Example for a
#'   Normal(0,1), use: c(0,1).
#' @param dimOneBoundsGenInitialStatusQuos A vector that contains the starting and ending points of the first dimension. 
#'   Example: c(0,1). Defaults to c(-Inf, Inf) if no boundary is provided. 
#' @param dimTwoBoundsGenInitialStatusQuos A vector that contains the starting and ending points of the second dimension.
#'   Example: c(0,1). Defaults to c(-Inf, Inf)  if no boundary is provided. 
#' 
#' @return outInitialIdealsDataFrame data.frame. The initial status quos data frame will have the following format.
#' 
#'  statusQuoID: A numeric identifier unique to the voter.
#'  xCord: The x coordinate of the initial status quo.
#'  yCord: The y coordinate of the voter's ideal point.

#' @examples
#'   genInitialStatusQuos(numberOfDimensionsGenInitialStatusQuos=1, numberOfInitialStatusQuosGenInitialStatusQuos=10, distributionTypeGenInitialStatusQuos ="unif", distributionParametersGenInitialStatusQuos = c(-1,1), dimOneBoundsGenInitialStatusQuos = c(-Inf,Inf), dimTwoBoundsGenInitialStatusQuos = c(-Inf,Inf) )
#'  
#'   genInitialStatusQuos(numberOfDimensionsGenInitialStatusQuos=1, numberOfInitialStatusQuosGenInitialStatusQuos=100, distributionTypeGenInitialStatusQuos ="beta", distributionParametersGenInitialStatusQuos = c(.5,2) )
#'   
#'  genInitialStatusQuos(numberOfDimensionsGenInitialStatusQuos=2, numberOfInitialStatusQuosGenInitialStatusQuos=1000, distributionTypeGenInitialStatusQuos ="norm", distributionParametersGenInitialStatusQuos = c(0,1) )
#'   
#' @export
# numberOfDimensionsGenInitialStatusQuos=2
# numberOfInitialStatusQuosGenInitialStatusQuos=10
# distributionTypeGenInitialStatusQuos ="unif"
# distributionParametersGenInitialStatusQuos = c(-1,1)
# dimOneBoundsGenInitialStatusQuos = c(-Inf,Inf)
# dimTwoBoundsGenInitialStatusQuos = c(-Inf,Inf)
genInitialStatusQuos <- function(numberOfDimensionsGenInitialStatusQuos=2, numberOfInitialStatusQuosGenInitialStatusQuos=3, distributionTypeGenInitialStatusQuos ="unif", distributionParametersGenInitialStatusQuos = c(-1,1), dimOneBoundsGenInitialStatusQuos = c(-Inf,Inf), dimTwoBoundsGenInitialStatusQuos = c(-Inf,Inf)){

    # 1) Generate Ideals
    tempIdeals <- genIdeals(numberOfDimensionsGenIdeals = numberOfDimensionsGenInitialStatusQuos, numberOfIdealsGenIdeals = numberOfInitialStatusQuosGenInitialStatusQuos, distributionTypeGenIdeals = distributionTypeGenInitialStatusQuos, distributionParametersGenIdeals = distributionParametersGenInitialStatusQuos, dimOneBoundsGenIdeals = dimOneBoundsGenInitialStatusQuos, dimTwoBoundsGenIdeals = dimTwoBoundsGenInitialStatusQuos)
    
    # 2) Create statusQuoIDs
    
    ID = paste( "SQ",seq(from = 1,to = numberOfInitialStatusQuosGenInitialStatusQuos), sep="-" )
    
    #3) Create a vector of the pointType called "competitor"
    
    pointType <- rep("statusQuo", numberOfInitialStatusQuosGenInitialStatusQuos)
    
    
    
    #4) Store Everything in a Data Frame 
    
    if(numberOfDimensionsGenInitialStatusQuos==1){
      outInitialIdealsDataFrame <- data.frame(pointType, ID, xLocation=tempIdeals[ ,1] )    
    }
        
    if(numberOfDimensionsGenInitialStatusQuos==2){
    outInitialIdealsDataFrame <- data.frame(pointType, ID, xLocation=tempIdeals[ ,1], yLocation=tempIdeals[ ,2] )    
    }
    
    
    outInitialIdealsDataFrame

}   
