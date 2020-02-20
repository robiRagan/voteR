#' useCondorcetMethod
#' Applies the COndorcet Method to a set of preferences. Ties broken at random.
#' 
#' Takes in a set of voter preferences, applies plurality rule to them and returns the choice set. 
#' @param voterPreferencesPluralityRunoff A matrix of voter preferences that is numVoters x numAlts.
#' @param breakVoterTiesPluralityRunoff Determines how a voter with two equal top choices cast their vote. Can be "random", "first" or "last".
#' @return A list where the first element is the group's choice set and the second element is the vote totals. 
#' @export
useCondorcetMethod <- function( voterPreferencesCondorcetMethod, 
                                    breakVoterTiesCondorcetMethod = "random"){
    # 
    # ### TEST ###
    # staticAltsDataFrame2 <- data.frame( pointType = rep(x = "alternative", 5),
    #     ID = c("Reeces", "Skittles", "Air Heads", "Milky Way", "Blow Pops"),
    #     xLocation=c(-1, -.0001, .0001, 0, -1),
    #     yLocation=c(1, -.0001, .0001, 0, -1), stringsAsFactors = FALSE
    # )
    # generatedVoterDataFrame2 <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 45, distributionTypeGenVoters = "unif", )
    # voterPreferencesCondorcetMethod<- calcMinkowskiUtilityVotersAlts(votersCalcMinkowskiDistanceVotersAlts = generatedVoterDataFrame2, alternativesCalcMinkowskiDistanceVotersAlts = staticAltsDataFrame2)
    # breakVoterTiesCondorcetMethod <- "random"
    # ### TEST ###
    
    # ### TEST ###
    # staticAltsDataFrame2 <- data.frame( pointType = rep(x = "alternative", 5),
    #     ID = c("Reeces", "Skittles", "Air Heads", "Milky Way", "Blow Pops"),
    #     xLocation=c(-1, -1, -1, -1, 0),
    #     yLocation=c(-1, -1, -1, -1, 0), stringsAsFactors = FALSE
    # )
    # generatedVoterDataFrame2 <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 45, distributionTypeGenVoters = "unif", )
    # voterPreferencesCondorcetMethod<- calcMinkowskiUtilityVotersAlts(votersCalcMinkowskiDistanceVotersAlts = generatedVoterDataFrame2, alternativesCalcMinkowskiDistanceVotersAlts = staticAltsDataFrame2)
    # breakVoterTiesCondorcetMethod <- "random"
    # ### TEST ###
    
    
    
    # Pull out the list of Alts
    theAlts <- colnames(voterPreferencesCondorcetMethod)
    
    # Create the train schedule of pairwise votes
        # # First find all combos of alts by row number in 
        # allCombos1 <- expand.grid(c(1:length(theAlts)),c(1:length(theAlts)))
        # # Now find only the unique combos by row number
        # allUniqueCombos1 <- subset(allCombos1,Var1!=Var2)
        
        # First find all combos of alts by name
        allCombos2 <- expand.grid(theAlts,theAlts, stringsAsFactors = FALSE)
        # Now find only the unique combos by name
        allUniqueCombos2 <- subset(allCombos2,Var1!=Var2)
    
    
    # eachPairWinner1 <- rep(NA,nrow(allUniqueCombos1))
    
    eachPairWinner2 <- rep(NA,nrow(allUniqueCombos2))
    
    for (i in 1:nrow(allUniqueCombos2)){
        thePairThisRound <- unlist(allUniqueCombos2[i, ])
        voterPreferencesForThisRound <- voterPreferencesCondorcetMethod[ , thePairThisRound]
        voterChoiceThisRound <- usePluralityRule(voterPreferencesForThisRound)$winners
        eachPairWinner2[i] <- voterChoiceThisRound$Alternative
    }
    allUniqueCombos2$Winner <- eachPairWinner2
    
    # Check to see if each alt always wins
    isThisACondorcetWinnerVector <- rep(NA,length(theAlts) )
    for(j in 1:length(theAlts) ){
        checkThisAlt <- theAlts[j]
        checkThesePairwiseVotes <- dplyr::filter(allUniqueCombos2, Var1==checkThisAlt | Var2==checkThisAlt)
        isThisACondorcetWinnerVector[j] <- all(checkThesePairwiseVotes$Winner==checkThisAlt)
        }
    
    condorcetWinnerIs <- theAlts[isThisACondorcetWinnerVector]
    
    if ( identical(condorcetWinnerIs, character(0))==FALSE ){
        condorcetMethodOut<- condorcetWinnerIs
    } else {
        condorcetMethodOut<- c("There is no Condorcet Winner")
        }

    condorcetMethodOut
    }    
    
