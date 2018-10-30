#' genVoters
#' Generates a set of voters with ideal points and the pararmeters for their utility functions.
#'  
#' Generates a set of voters, each with a voterID number, an ideal point using \code{\link{genIdeals}}, 
#' a salience weighting matrix using \code{\link{genSalience}}, a Minkowsi Order number using 
#' \code{\link{genMinkoOrder}}, and a loss function order using \code{\link{genLossOrder}}.
#' These ideal point, salience weights, minkowski distance order and loss function order can be used
#' by \code{\link{minkoUtilitySets}} to calulate utility and find indifference curves. 
#'  
#' @section TODO:
#' Add calibration functionality and move this into C++.
#' 
#' @param numberOfDimensionsGenVoters The number of policy dimensions.
#' @param numberOfVotersGenVoters Number of voters to generate.
#' @param distributionTypeGenVoters A string identifying the base R discribution to draw
#'   the ideal points from. Uses the base R random number generation family of
#'   commands rxxxx (see ?distributions). The user should specify the
#'   distribution as a string using the standard R abreviation for the
#'   distribution (see ?distributions for a list). Currently supported are: "norm",
#'   "unif", "binom", "cauchy", "chisq", "weibull"
#' @param distributionParametersGenVoters A vector that contains the additional
#'   parameters needed by the particular rxxxx function for a distribtuion. (see
#'   ?rxxxx where xxxx is a function listed under ?distribution). Example for a
#'   Normal(0,1), use: c(0,1).
#' @param numDimsGenVoters scalar Number of policy dimensions. Can be 1 or 2. If this is set to 1, then the salience of that one dimension will be 1 for all voters. 
#' @param dimOneBoundsGenVoters A vector that contains the starting and ending poitns of t
#' he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf) if no boundary is provided. 
#' @param dimTwoBoundsGenVoters A vector that contains the starting and ending poitns of t
#' he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf)  if no boundary is provided. 
#' @param salienceHeterogeneityGenVoters scalar [0,1] The probability that any given dimension will also have a salience of 1 for a voter. If this is set to 1 then all dimensions will have the same salience. 
#' @param maxRelativeSalienceGenVoters scalar The maximum relative salience for a dimension you want to allow your voters to have. 
#' @param allEllipticalGenVoters logical If If FALSE (the default) then the agent's Minkowski order for their utility function is determined by the other parameters in this function. If TRUE then all agents will have eliptical indifference curves (Minkowski Order = 2). If their salience's across all dimesnions is 1 then the indifference curves will be circles. 
#' @param probabilityElipticalGenVoters scalar [0,1] The probability that any given voter will have elliptical (Minkowski Order of 2) indifference curves.
#' @param probabilityDiamondGenVoters scalar [0,1] The probability that any given voter will have Manhattan/Diamond (Minkowski Order = 1) indifference curves.
#' @param probabilitySquareGenVoters scalar [0,1] The probability that any given voter will have Chebyshev/Square (Minkowski Order = 100) indifference curves.
#' @param lossOrderForAllGenVoters logical or scalar If If FALSE (the default) then the agent's loss order for their utility function is determined by the other parameters in this function. If a number is provided this will be the order of the loss function for all voters. 
#' @param probabilityLinearGenVoters scalar [0,1] The probability that any given voter will have elliptical (Minkowski Order of 2) indifference curves.
#' @param probabilityQuadraticGenVoters scalar [0,1] The probability that any given voter will have Manhattan/Diamond (Minkowski Order = 1) indifference curves.
#' @param probabilityCubicGenVoters scalar [0,1] The probability that any given voter will have Chebyshev/Square (Minkowski Order = 100) indifference curves.
#' 
#' @return outVotersDataFrame data.frame The voters data frame will have the following format.
#' 
#'  voterID: A numeric identifier unique to the voter.
#'  xIdeal: The x coordinate of the voter's ideal point.
#'  yIdeal: The y coordinate of the voter's ideal point.
#'  minkoOrder: The Minkowski order of the voters MInkowski metric based utility function. = 1, is City Block. = 2 is Euclidian and 100 = is  See ?Minkowski. 
#'  xSalience: The salience of the x dimension for the voter. The dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#'  ySalience: The salience of the y dimension for the voter. he dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#' @examples
#'   genVoters(numberOfDimensionsGenVoters=2, numberOfVotersGenVoters=100, distributionTypeGenVoters ="norm", distributionParametersGenVoters = c(0,1), dimOneBoundsGenVoters = c(-Inf,0), dimTwoBoundsGenVoters = c(0,Inf), salienceHeterogeneityGenVoters=1, maxRelativeSalienceGenVoters=2)
#'  
#'   genVoters(numberOfDimensionsGenVoters=1, numberOfVotersGenVoters=100, distributionTypeGenVoters ="unif", distributionParametersGenVoters = c(0,1), salienceHeterogeneityGenVoters=1, maxRelativeSalienceGenVoters=2)
#' @export
# 
# numberOfDimensionsGenVoters <- 2 
# numberOfVotersGenVoters=100
# distributionTypeGenVoters ="unif"
# distributionParametersGenVoters = c(-1,1) 
# salienceHeterogeneityGenVoters=1 
# maxRelativeSalienceGenVoters=2 
# allEllipticalGenVoters = TRUE
# probabilityElipticalGenVoters = 1 
# probabilityDiamondGenVoters = 0
# probabilitySquareGenVoters = 0
genVoters <- function(numberOfDimensionsGenVoters=1, numberOfVotersGenVoters=15, distributionTypeGenVoters ="unif", distributionParametersGenVoters = c(-1,1), dimOneBoundsGenVoters = c(-Inf,Inf), dimTwoBoundsGenVoters = c(-Inf,Inf), salienceHeterogeneityGenVoters=1, maxRelativeSalienceGenVoters=2, allEllipticalGenVoters = TRUE, probabilityElipticalGenVoters = 1, probabilityDiamondGenVoters = 0, probabilitySquareGenVoters = 0, lossOrderForAllGenVoters=FALSE, probabilityLinearGenVoters=1, probabilityQuadraticGenVoters=0, probabilityCubicGenVoters=0){

    # 1) Generate Ideals
    tempIdeals <- genIdeals(numberOfDimensionsGenIdeals = numberOfDimensionsGenVoters, numberOfIdealsGenIdeals = numberOfVotersGenVoters, distributionTypeGenIdeals = distributionTypeGenVoters, distributionParametersGenIdeals = distributionParametersGenVoters, dimOneBoundsGenIdeals = dimOneBoundsGenVoters, dimTwoBoundsGenIdeals = dimTwoBoundsGenVoters)
    
    # 2) Genetate Salience
    tempSalience <- genSalience(numVotersGenSalience = numberOfVotersGenVoters, numDimsGenSalience = numberOfDimensionsGenVoters, salienceHeterogeneityGenSalience = salienceHeterogeneityGenVoters, maxRelativeSalienceGenSalience = maxRelativeSalienceGenVoters)
    
    # 3) Generate a Minkowski Order
    
    tempMinkoOrder <- genMinkoOrder(numVotersGenMinko = numberOfVotersGenVoters, allEllipticalGenMinko = allEllipticalGenVoters, probabilityElipticalGenMinko = probabilityElipticalGenVoters, probabilityDiamondGenMinko = probabilityDiamondGenVoters, probabilitySquareGenMinko = probabilitySquareGenVoters)    

    #4) Generate Loss Order
    tempLossOrder <- genLossOrder(numVotersGenLoss = numberOfVotersGenVoters, lossOrderForAllGenLoss = lossOrderForAllGenVoters, probabilityLinearGenLoss = probabilityLinearGenVoters, probabilityQuadraticGenLoss = probabilityQuadraticGenVoters, probabilityCubicGenLoss = probabilityCubicGenVoters)
     
    #5) Create a vector of the pointType called "voter"
    
    pointType <- rep("voter", numberOfVotersGenVoters)
    
    #6) Create voterIDs
    
    voterID = paste( "V",seq(from = 1,to = numberOfVotersGenVoters), sep="-" )
    
    
    #7) Store Everything in a Data Frame 
    
    if(numberOfDimensionsGenVoters==1){
        outVoterDataFrame <- data.frame(pointType, voterID, xIdeal=tempIdeals[ ,1], minkoOrder=tempMinkoOrder, xSalience = tempSalience[ ,1], lossOrder = tempLossOrder )    
    }
        
    if(numberOfDimensionsGenVoters==2){
outVoterDataFrame <- data.frame(pointType, voterID, xIdeal=tempIdeals[ ,1], yIdeal=tempIdeals[ ,2], minkoOrder=tempMinkoOrder, xSalience = tempSalience[ ,1], ySalience = tempSalience[ ,2],  lossOrder = tempLossOrder)    
    }
    

outVoterDataFrame

}   
