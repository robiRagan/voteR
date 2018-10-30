#' genMinkoOrder 
#' Generates Minkowski Orders for voters in a generated voter set. 
#' 
#' Randomly generate a MinkowskiOrder for all voters in a data set. \code{\link{genMinkoOrder}} is used in conjuction 
#' with \code{\link{genSalience}} and \code{\link{genIdeals}} by \code{\link{genVoters}} to generate
#' the parameters of each agent's Salience Weighted Minkowski Utility function. 
#' @param numVotersGenMinko scalar Number of voters
#' @param allEllipticalGenMinko logical If If FALSE (the default) then the agent's Minkowski order for their utility function is determined by the other parameters in this function. If TRUE then all agents will have eliptical indifference curves (Minkowski Order = 2). If their salience's across all dimesnions is 1 then the indifference curves will be circles. 
#' @param probabilityElipticalGenMinko scalar [0,1] The probability that any given voter will have elliptical (Minkowski Order of 2) indifference curves.
#' @param probabilityDiamondGenMinko scalar [0,1] The probability that any given voter will have Manhattan/Diamond (Minkowski Order = 1) indifference curves.
#' @param probabilitySquareGenMinko scalar [0,1] The probability that any given voter will have Chebyshev/Square (Minkowski Order = 100) indifference curves.
#' 
#' @return outMinkowskiOrder numVoters x 1 A vector of minkowski orders, one for each voter that determines the overall shape of the voter's indifference curves.
 # numVotersGenMinko <- 100 ###
 # allEllipticalGenMinko <- FALSE ###
 # probabilityElipticalGenMinko <-.90 ###
 # probabilityDiamondGenMinko <- .05 ###
 # probabilitySquareGenMinko <- .05 ###
#' @export

genMinkoOrder <- function(numVotersGenMinko, allEllipticalGenMinko=FALSE, probabilityElipticalGenMinko=1, probabilityDiamondGenMinko=0, probabilitySquareGenMinko=0){
  
    if(allEllipticalGenMinko==TRUE){probabilityElipticalGenMinko <- 1}
      
    if(probabilityElipticalGenMinko+probabilityDiamondGenMinko+probabilitySquareGenMinko!=1){stop('Something is not quite right here. If you want all the agents to have elliptical indifference curves then set allEllipticalGenMinko=TRUE, and leave off three probability parameters. If instead you want to specify the probability of each type then the parameters probabilityElipticalGenMinko, probabilityDiamondGenMinko, and probabilitySquareGenMinko must sum to 1.')}
      
    outMinkowskiOrder <- sample(x = c(1,2,100), size = numVotersGenMinko, replace = TRUE, prob = c(probabilityDiamondGenMinko,probabilityElipticalGenMinko,probabilitySquareGenMinko))
     
    outMinkowskiOrder
  }