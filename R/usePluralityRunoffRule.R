#' usePluralityRunoffRule
#' Applies plurality rule to a set of preferences. Ties broken at random.
#' 
#' Takes in a set of voter preferences, applies plurality rule to them and returns the choice set. 
#' @param voterPreferencesPluralityRunoff A matrix of voter preferences that is numVoters x numAlts.
#' @param breakVoterTiesPluralityRunoff Determines how a voter with two equal top choices cast their vote. Can be "random", "first" or "last".
#' @return A list where the first element is the group's choice set and the second element is the vote totals. 
#' @export
usePluralityRunoffRule <- function( voterPreferencesPluralityRunoff, 
                                    breakVoterTiesPluralityRunoff = "random"){
    
    # ### TEST ###
    # staticAltsDataFrame2 <- data.frame( pointType = rep(x = "alternative", 5),
    #     ID = c("Reeces", "Skittles", "Air Heads", "Milky Way", "Blow Pops"),
    #     xLocation=c(-3/8, 1/8, 2/8, 7/8, 4/8),
    #     yLocation=c(-3/8, 1/8, 7/8, -4/8, -5/8)
    # )
    # generatedVoterDataFrame2 <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 45, distributionTypeGenVoters = "unif", )
    # voterPreferencesPluralityRunoff <- calcMinkowskiUtilityVotersAlts(votersCalcMinkowskiDistanceVotersAlts = generatedVoterDataFrame2, alternativesCalcMinkowskiDistanceVotersAlts = staticAltsDataFrame2)
    # breakVoterTiesPluralityRunoff <- "random"
    # forceUniqueChoicePluralityRunoff <- "TRUE"
    # howToSelectUniqueChoicePluralityRunoff <- "random"
    # ### TEST ###
    
    
    # Run the regular plurality rule
    firstPluralityRuleOutput <- usePluralityRule(  voterPreferences = voterPreferencesPluralityRunoff, 
                                        breakVoterTies = breakVoterTiesPluralityRunoff
                                            )
    
    # Sort by vote count
    firstPluralityRuleOutput <- dplyr::arrange(firstPluralityRuleOutput$totals, desc(Count) )
    
    # Find the top two choices for runoff choice.
    TopTwoForRunoff <- firstPluralityRuleOutput[1:2, ]$Alternative
    
    # Select only the runnoff alternatives in a new voter preferences data frame
    voterPreferencesForRunoff <- voterPreferencesPluralityRunoff[ , TopTwoForRunoff]
    
    # Now run the plurality rule on the voter preferences over the 2 alts. 
    pluralityRunoffOut <- usePluralityRule(voterPreferencesForRunoff)
    
    pluralityRunoffOut
}    
    
