#' genLossOrder 
#' Generates Loss Function Orders for voters in a generated voter set. 
#' 
#' Randomly generate a Loss Function Order for all voters in a data set. \code{\link{genLossOrder}} is used in conjuction 
#' with \code{\link{GenMinkoOrder}},  \code{\link{genSalience}} and \code{\link{genIdeals}} by \code{\link{genVoters}} to generate
#' the parameters of each agent's Salience Weighted Minkowski Utility function. 
#' @param numVotersGenLoss scalar Number of voters
#' @param lossOrderForAllGenLoss logical or scalar If If FALSE (the default) then the agent's loss order for their utility function is determined by the other parameters in this function. If a number is provided this will be the order of the loss function for all voters. 
#' @param probabilityLinearGenLoss scalar [0,1] The probability that any given voter will have elliptical (Minkowski Order of 2) indifference curves.
#' @param probabilityQuadraticGenLoss scalar [0,1] The probability that any given voter will have Manhattan/Diamond (Minkowski Order = 1) indifference curves.
#' @param probabilityCubicGenLoss scalar [0,1] The probability that any given voter will have Chebyshev/Square (Minkowski Order = 100) indifference curves.
#' 
#' @return outMinkowskiOrder numVoters x 1 A vector of minkowski orders, one for each voter that determines the overall shape of the voter's indifference curves.
 # numVotersGenLoss <- 100 ###
 # lossOrderForAll <-  4###
 # probabilityLinearGenLoss <-.90 ###
 # probabilityQuadraticGenLoss <- .05 ###
 # probabilityCubicGenLoss <- .05 ###
#' @export
genLossOrder <- function(numVotersGenLoss, lossOrderForAllGenLoss=FALSE, probabilityLinearGenLoss=1, probabilityQuadraticGenLoss=0, probabilityCubicGenLoss=0){
  
    if(lossOrderForAllGenLoss!=FALSE){outLossOrder <- rep(lossOrderForAllGenLoss,numVotersGenLoss)}
      
    if(lossOrderForAllGenLoss==FALSE){
    if(probabilityLinearGenLoss+probabilityQuadraticGenLoss+probabilityCubicGenLoss!=1){stop('Something is not quite right here. If you want all the agents to have the same loss order for example 2, then set lossOrderForAll=2. If you want the loss orders to be different across voters then set lossOrderForAll=FALSE and specify the probability of each type then the parameters probabilityLinearGenLoss, probabilityQuadraticGenLoss, and probabilityCubicGenLoss must sum to 1.')}
      
    outLossOrder <- sample(x = c(1,2,3), size = numVotersGenLoss, replace = TRUE, prob = c(probabilityQuadraticGenLoss,probabilityLinearGenLoss,probabilityCubicGenLoss))
    }
    outLossOrder
  }