#' usePluralityRule
#' Applies plurality rule to a set of preferences. Ties broken at random.
#' 
#' Takes in a set of voter preferences, applies plurality rule to them and returns the choice set. 
#' @param voterPreferences A matrix of voter preferences that is numVoters x numAlts.
#' @param breakVoterTies Determines how a voter with two equal top choices cast their vote. Can be "random", "first" or "last".
#' @param forceUniqueChoice If there is more than one alternative in the choice set should only one be returned as the gorup choice. If set to TRUE then choose a method with the parameter \code{howToSelectUniqueChoice}.
#' @param howToSelectUniqueChoice If there is more than one alternative in the choice set, and \code{forceUniqueChoice == "TRUE"}, this determines which alternative is selected as the gorup choice. Can be "random", "first" or "last".
#' @return A list where the first element is the group's choice set and the second element is the vote totals. 
#' @export
usePluralityRule <- function(voterPreferences, breakVoterTies = "random", forceUniqueChoice = "TRUE", howToSelectUniqueChoice = "random"){
    
# ### TEST ###
# staticAltsDataFrame1 <- data.frame( pointType = rep(x = "alternative", 5),
#                                     ID = c("Reeces", "Skittles", "Air Heads", "Milky Way", "Blow Pops"),
#                                     xLocation=c(-3/8, 1/8, 2/8, 7/8, 4/8),
#                                     yLocation=c(-3/8, 1/8, 7/8, -4/8, -5/8), 
#                                     stringsAsFactors = FALSE
#                                     )
# generatedVoterDataFrame1 <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 45, distributionTypeGenVoters = "unif", )
# voterPreferences <- calcMinkowskiUtilityVotersAlts(votersCalcMinkowskiDistanceVotersAlts = generatedVoterDataFrame1, alternativesCalcMinkowskiDistanceVotersAlts = staticAltsDataFrame1)
# breakVoterTies <- "random"
# forceUniqueChoice <- "TRUE"
# howToSelectUniqueChoice <- "random"
#     ### TEST ###
    
    # Find each voters top choice breaking any preference ties by breakVoterTies
    topChoice <- colnames(voterPreferences)[max.col(voterPreferences,ties.method=breakVoterTies)]
    # Frequency Table of the Top Choices
    voteCount <- data.frame( table(topChoice) )
    voteCount$topChoice <- as.character(voteCount$topChoice)
    # Rename the columns
    names(voteCount) <- c("Alternative", "Count")
    # Find the top choice. 
    choiceSet <- dplyr::top_n(voteCount, 1, Count)
    
    if(nrow(choiceSet)>1 & forceUniqueChoice==TRUE){
        
        if(howToSelectUniqueChoice=="first"){
        choiceSet <- choiceSet[1, ]
        }

        if(howToSelectUniqueChoice=="random"){
        randomRowNumber <- sample( c(1:nrow(choiceSet)), size = 1 )
            choiceSet <- choiceSet[randomRowNumber, ]
        }

        if(howToSelectUniqueChoice=="last"){
        choiceSet <- choiceSet[nrow(choiceSet), ]
        }


    }

    pluralityOut <- list(winners = choiceSet, totals = voteCount)
    
    pluralityOut
}    
    
