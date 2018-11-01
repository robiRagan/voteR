#' genRandomProposals
#' Generates a set of random proposal to face the status quo policy.
#' 
#' Generates a set of Random proposals to face the status quo. 
#' Several different probability distributions can be used to generate the proposals. 
#'  
#' @param numberOfDimensionsGenRandomProposals The number of policy dimensions.
#' @param numberOfRandomProposalsGenRandomProposals Number of RandomProposals to generate.
#' @param distributionTypeGenRandomProposals A string identifying the base R discribution to draw
#'   the ideal points from. Uses the base R random number generation family of
#'   commands rxxxx (see ?distributions). The user should specify the
#'   distribution as a string using the standard R abreviation for the
#'   distribution (see ?distributions for a list). Currently supported are: "norm",
#'   "unif", "beta", "cauchy", "chisq", "weibull"
#' @param distributionParametersGenRandomProposals A vector that contains the additional
#'   parameters needed by the particular rxxxx function for a distribtuion. (see
#'   ?rxxxx where xxxx is a function listed under ?distribution). Example for a
#'   Normal(0,1), use: c(0,1).
#' @param dimOneBoundsGenRandomProposals A vector that contains the starting and ending poitns of t
#' he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf) if no boundary is provided. 
#' @param dimTwoBoundsGenRandomProposals A vector that contains the starting and ending poitns of t
#' he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf)  if no boundary is provided. 
#' 
#' @return outRandomProposalsDataFrame data.frame The RandomProposals data frame will have the following format.
#' 
#'  proposalID: A numeric identifier unique to the voter.
#'  xCoord: The x coordinate of the voter's ideal point.
#'  yCoord: The y coordinate of the voter's ideal point.
#'  minkoOrder: The Minkowski order of the voters MInkowski metric based utility function. = 1, is City Block. = 2 is Euclidian and 100 = is  See ?Minkowski. 
#' @examples
#'   genRandomProposals(numberOfDimensionsGenRandomProposals=1, numberOfRandomProposalsGenRandomProposals=10, distributionTypeGenRandomProposals ="unif", distributionParametersGenRandomProposals = c(-1,1), dimOneBoundsGenRandomProposals = c(-Inf,Inf), dimTwoBoundsGenRandomProposals = c(-Inf,Inf) ) 
#'  
#'   genRandomProposals(numberOfDimensionsGenRandomProposals=2, numberOfRandomProposalsGenRandomProposals=100, distributionTypeGenRandomProposals ="norm")
#'   
#'  genRandomProposals(numberOfDimensionsGenRandomProposals=2, numberOfRandomProposalsGenRandomProposals=10, distributionTypeGenRandomProposals ="beta", distributionParametersGenRandomProposals = c(.1,1), dimOneBoundsGenRandomProposals = c(0,1), dimTwoBoundsGenRandomProposals = c(0,1) ) 
#' @export
# numberOfDimensionsGenRandomProposals=2
# numberOfRandomProposalsGenRandomProposals=10
# distributionTypeGenRandomProposals ="unif"
# distributionParametersGenRandomProposals = c(-1,1)
# dimOneBoundsGenRandomProposals = c(-Inf,Inf)
# dimTwoBoundsGenRandomProposals = c(-Inf,Inf)
genRandomProposals <- function(numberOfDimensionsGenRandomProposals=2, numberOfRandomProposalsGenRandomProposals=3, distributionTypeGenRandomProposals ="unif", distributionParametersGenRandomProposals = c(-1,1), dimOneBoundsGenRandomProposals = c(-Inf,Inf), dimTwoBoundsGenRandomProposals = c(-Inf,Inf) ){

    # 1) Generate Ideals
    tempIdeals <- genIdeals(numberOfDimensionsGenIdeals = numberOfDimensionsGenRandomProposals, numberOfIdealsGenIdeals = numberOfRandomProposalsGenRandomProposals, distributionTypeGenIdeals = distributionTypeGenRandomProposals, distributionParametersGenIdeals = distributionParametersGenRandomProposals, dimOneBoundsGenIdeals = dimOneBoundsGenRandomProposals, dimTwoBoundsGenIdeals = dimTwoBoundsGenRandomProposals)
    
    
    #2) Create proposalIDs
    
    proposalID = paste( "P",seq(from = 1,to = numberOfRandomProposalsGenRandomProposals), sep="-" )
    
    
    #3) Store Everything in a Data Frame 
    
    if(numberOfDimensionsGenRandomProposals==1){
        outRandomProposalsDataFrame <- data.frame(proposalID, xCoord=tempIdeals[ ,1] )    
    }
        
    if(numberOfDimensionsGenRandomProposals==2){
    outRandomProposalsDataFrame <- data.frame(proposalID, xLocation=tempIdeals[ ,1], yLocation=tempIdeals[ ,2] )    
    }
    
    # Rename the x and y coordinates to xLocation and yLocation
    # names(outCompetitorDataFrame)[names(outCompetitorDataFrame) == 'xCoord'] <- 'xCompetitor'
    # names(outCompetitorDataFrame)[names(outCompetitorDataFrame) == 'yCoord'] <- 'yCompetitor'
    
    outRandomProposalsDataFrame

}   
