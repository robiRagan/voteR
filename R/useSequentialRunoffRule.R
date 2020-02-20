#' useSequentialRunoffRule
#' Applies plurality rule to a set of preferences. Ties broken at random.
#' 
#' Takes in a set of voter preferences, applies plurality rule to them and returns the choice set. 
#' @param voterPreferencesSequentialRunoff A matrix of voter preferences that is numVoters x numAlts.
#' @param breakVoterTiesSequentialRunoff Determines how a voter with two equal top choices cast their vote. Can be "random", "first" or "last".
#' @return A list where the first element is the group's choice set and the subsequent elelmets are the vote toatls form each round.  #STILL TO DO 
#' @export
useSequentialRunoffRule <- function( voterPreferencesSequentialRunoff, 
                                    breakVoterTiesSequentialRunoff = "random"){
    # 
    # # ### TEST ###
    # staticAltsDataFrame2 <- data.frame( pointType = rep(x = "alternative", 5),
    #     ID = c("Reeces", "Skittles", "Air Heads", "Milky Way", "Blow Pops"),
    #     xLocation=c(-3/8, 1/8, 2/8, 7/8, 4/8),
    #     yLocation=c(-3/8, 1/8, 7/8, -4/8, -5/8)
    # )
    # generatedVoterDataFrame2 <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 45, distributionTypeGenVoters = "unif")
    # voterPreferencesSequentialRunoff <- calcMinkowskiUtilityVotersAlts(votersCalcMinkowskiDistanceVotersAlts = generatedVoterDataFrame2, alternativesCalcMinkowskiDistanceVotersAlts = staticAltsDataFrame2)
    # breakVoterTiesSequentialRunoff <- "random"
    # forceUniqueChoiceSequentialRunoff <- "TRUE"
    # howToSelectUniqueChoiceSequentialRunoff <- "random"
    # ### TEST ###
    
    
  while ( ncol(voterPreferencesSequentialRunoff) > 2) {
    # Run plurality rule to get the vote totals
    voteTotals <- usePluralityRule(voterPreferencesSequentialRunoff)$totals
    
    # Sort by vote count
    voteTotals <- dplyr::arrange(voteTotals, desc(Count) )
    
    # Drop the alt with the lowest vote total .
    altsLeftAfterDrop <- voteTotals[1:nrow(voteTotals)-1, ]$Alternative
    
    # Select only the runnoff alternatives in a new voter preferences data frame
    voterPreferencesSequentialRunoff <- voterPreferencesSequentialRunoff[ , altsLeftAfterDrop]
    
  }

  sequentialRunoffOut <- usePluralityRule(voterPreferencesSequentialRunoff)$winners$Alternative
    
  sequentialRunoffOut 
}    
    
