#' genSalience 
#' 
#' Randomly generate a vector of salience weights for each policy dimension for a set of voters.
#'
#' This function used in conjunction with \code{\link{genVoters}} to randomly generate a set of voters based on a set of user provided parameters. This function randomly generates a vector of salience weights for each policy dimension for a set of voters. For each voter, it randomly sets one dimension as the numeriare dimension with a salience of 1.  The salience of the other dimension is set stocasticly based on the parameters \code{salienceHeterogeneityGenSalience} and \code{maxRelativeSalienceGenSalience}. \code{salienceHeterogeneityGenSalience} allows a user can decide how much heterogeneity of salience they want to allow their set of voters to have. \code{maxRelativeSalienceGenSalience} allows a user to set a bounds on the maximum sailence that can be for their generated voters. You are not required to use this function to provide the voter's saliences. You may simply provide a numberOfVoters x numberOfDimensions matrix to \code{\link{setVoters}}
#' @param numVotersGenSalience scalar Number of voters
#' @param numDimsGenSalience scalar Number of policy dimensions. Can be 1 or 2. If this is set to 1, then the salience of that one dimension will be 1 for all voters. 
#' @param salienceHeterogeneityGenSalience scalar [0,1] The probability that any given dimension will also have a salience of 1 for a voter. If this is set to 1 then all dimensions will have the same salience. 
#' @param maxRelativeSalienceGenSalience scalar The maximum relative salience for a dimension you want to allow your voters to have. 
#' 
#' @return outSalience numVoters x numDimensions matrix of salience weights. For one voter/row, each element of the vector represents the 
#'                        *relative* saliance of each dimension for a voter. The dimension with the lowest salience serves as the "numeraire" 
#'                        dimension and should recieve a salience of 1. All the other saliences are expressed in units of this "numeraire". 
#'                        So if a dimension is twice as important to a voter as the numeraire dimension it recieves a salience of 2.
#'                            Example:  1 2
#'                                      1 3 
#'                                      2 1 
#'                                      1 1
#'                                      4 1
#'                            For voter two (second row) the salience of dimension one (first column) is the numeraire. For this voter dimension two is twice  as salient as dimension one.

# 
# numVotersGenSalience <- 100 ###
# numDimsGenSalience <- 2 ###
# salienceHeterogeneityGenSalience <-1 ###
# maxRelativeSalienceGenSalience <- 4 ###

#' @export
genSalience <- function(numVotersGenSalience,numDimsGenSalience,salienceHeterogeneityGenSalience=1, maxRelativeSalienceGenSalience=2){
  
    if(numDimsGenSalience==1){
      
      outSalience <- matrix(data = rep(1, numVotersGenSalience*numDimsGenSalience), nrow = numVotersGenSalience, ncol = numDimsGenSalience)

  }else{
      
      equalSalienceOrNot <- rbinom(n = numVotersGenSalience, size = 1, prob = salienceHeterogeneityGenSalience)
     
      rawSalience <- matrix(data = NA, nrow = numVotersGenSalience, ncol = numDimsGenSalience)
     
      for (i in 1:numVotersGenSalience){
          
          if(equalSalienceOrNot[i]==1){
              
              rawSalience[i, ] <- c(1,1)
          
              }else{
            
              rawSalience[i, ] <- sample(c(0,1))
          }
      
          }
      
      
      rawSalience[rawSalience == 0] <- sample( x = c(2:maxRelativeSalienceGenSalience), size = (numVotersGenSalience*numDimsGenSalience)-sum(rawSalience), replace = TRUE)
      
     outSalience <- rawSalience
      }
    
  
    outSalience
  }

