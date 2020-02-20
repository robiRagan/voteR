#' useBordaCount
#' Applies plurality rule to a set of preferences. Ties broken at random.
#' 
#' Takes in a set of voter preferences, applies plurality rule to them and returns the choice set. 
#' @param voterPreferencesBordaCount A matrix of voter preferences that is numVoters x numAlts.
#' @param breakVoterTiesBordaCount Determines how a voter with two equal top choices cast their vote. Can be "random", "first" or "last".
#' @return A list where the first element is the group's choice set and the subsequent elelmets are the vote toatls form each round.  
#' @export
useBordaCount <- function( voterPreferencesBordaCount, 
                           breakVoterTiesBordaCount = "random"){
    
    ### TEST ###
    staticAltsDataFrame2 <- data.frame( pointType = rep(x = "alternative", 5),
        ID = c("Reeces", "Skittles", "Air Heads", "Milky Way", "Blow Pops"),
        xLocation=c(-3/8, 1/8, 2/8, 7/8, 4/8),
        yLocation=c(-3/8, 1/8, 7/8, -4/8, -5/8)
    )
    generatedVoterDataFrame2 <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 45, distributionTypeGenVoters = "unif")
    voterPreferencesBordaCount <- calcMinkowskiUtilityVotersAlts(votersCalcMinkowskiDistanceVotersAlts = generatedVoterDataFrame2, alternativesCalcMinkowskiDistanceVotersAlts = staticAltsDataFrame2)
    breakVoterTiesBordaCount <- "random"
    ### TEST ###
    
    # Matrix to store the borda counts.
    # Here I just copy the original matrix in order to get the rownames and column names. 
    bordaCounts <- voterPreferencesBordaCount
    
    # Create the Borda Count 
      for(i in 1:nrow(voterPreferencesBordaCount)){
        bordaCounts[i, ] <- dplyr::row_number(voterPreferencesBordaCount[i, ])
      }
    
    bordaCountTotals <- colSums(bordaCounts)
    
    bordaCountWinner <- which.max(bordaCountTotals)

    bordaCountOut <- names(bordaCountWinner)
    
    bordaCountOut
    }    
    
