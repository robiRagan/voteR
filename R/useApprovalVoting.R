#' useApprovalVoting
#' Applies the COndorcet Method to a set of preferences. Ties broken at random.
#' 
#' Takes in a set of voter preferences, applies plurality rule to them and returns the choice set. 
#' @param voterPreferencesApprovalVoting A matrix of voter preferences that is numVoters x numAlts.
#' @param breakVoterTiesApprovalVoting Determines how a voter with two equal top choices cast their vote. Can be "random", "first" or "last".
#' @param forceUniqueChoiceApprovalVoting If there is more than one alternative in the choice set should only one be returned as the gorup choice. If set to TRUE then choose a method with the parameter \code{howToSelectUniqueChoice}.
#' @param howToSelectUniqueChoiceApprovalVoting If there is more than one alternative in the choice set, and \code{forceUniqueChoice == "TRUE"}, this determines which alternative is selected as the gorup choice. Can be "random", "first" or "last".
#' @return A list where the first element is the group's choice set and the second element is the vote totals. 
#' @export
useApprovalVoting <- function( voterPreferencesApprovalVoting, 
                                    breakVoterTiesApprovalVoting = "random", 
                                    forceUniqueChoiceApprovalVoting=TRUE,
                                    howToSelectUniqueChoiceApprovalVoting='random'){
    # ### TEST ###
    # staticAltsDataFrame1 <- data.frame( pointType = rep(x = "alternative", 5),
    #                                     ID = c("Reeces", "Skittles", "Air Heads", "Milky Way", "Blow Pops"),
    #                                     xLocation=c(-3/8, 1/8, 2/8, 7/8, 4/8),
    #                                     yLocation=c(-3/8, 1/8, 7/8, -4/8, -5/8),
    #                                     stringsAsFactors = FALSE
    #                                     )
    # generatedVoterDataFrame1 <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 45, distributionTypeGenVoters = "unif", )
    # voterPreferencesApprovalVoting <- calcMinkowskiUtilityVotersAlts(votersCalcMinkowskiDistanceVotersAlts = generatedVoterDataFrame1, alternativesCalcMinkowskiDistanceVotersAlts = staticAltsDataFrame1)
    # breakVoterTiesApprovalVoting <- "random"
    # forceUniqueChoiceApprovalVoting <- "TRUE"
    # howToSelectUniqueChoiceApprovalVoting <- "random"
    #     ### TEST ###
    
    # ### TEST ###
    # theAlts <- c("Reeces", "Skittles", "Air Heads", "Milky Way", "Blow Pops")
    # voterPreferencesApprovalVoting  <- matrix(data = sample( x = c(0:10), size = (45*length(theAlts) ), replace = TRUE ), nrow = 45, ncol = length(theAlts) )
    # voterPreferencesApprovalVoting <- data.frame(voterPreferencesApprovalVoting)
    # names(voterPreferencesApprovalVoting) <- theAlts
    # breakVoterTiesApprovalVoting  <- "random"
    # forceUniqueChoiceApprovalVoting <- FALSE
    # howToSelectUniqueChoiceApprovalVoting <- 'random'
    # ### TEST ###
    
    
    # ### TEST ###
    # voterPreferencesApprovalVoting <- rawUtility
    # breakVoterTiesApprovalVoting  <- "random"
    # forceUniqueChoiceApprovalVoting <- FALSE
    # howToSelectUniqueChoiceApprovalVoting <- 'random'
    # ### TEST ###
    
    
    # Convert the Vote Preferences into binary
    voterPreferencesApprovalVoting <- ifelse(voterPreferencesApprovalVoting==0, 0, 1)
    #voterPreferencesApprovalVoting <- data.frame(voterPreferencesApprovalVoting)
    # get the vote Counts
    
    voteTotalsApprovalVoting <- data.frame("Alternative"=rep(NA, ncol(voterPreferencesApprovalVoting) ), "Count"=rep(NA, ncol(voterPreferencesApprovalVoting) ) ) 
    voteTotalsApprovalVoting[ ,1] <- colnames(voterPreferencesApprovalVoting)
    
    voteTotalsApprovalVoting[ ,2] <- colSums(voterPreferencesApprovalVoting)
    
    # Find the top choice. 
    choiceSet <- dplyr::top_n(voteTotalsApprovalVoting, 1, Count)
    
    
    if(nrow(choiceSet)>1 & forceUniqueChoiceApprovalVoting==TRUE){
        
        if(howToSelectUniqueChoiceApprovalVoting=="first"){
            choiceSet <- choiceSet[1, ]
        }
        
        if(howToSelectUniqueChoiceApprovalVoting=="random"){
            randomRowNumber <- sample( c(1:nrow(choiceSet)), size = 1 )
            choiceSet <- choiceSet[randomRowNumber, ]
        }
        
        if(howToSelectUniqueChoiceApprovalVoting=="last"){
            choiceSet <- choiceSet[nrow(choiceSet), ]
        }
        
        
    }
    
    approvalMethodOut <- list(winners = choiceSet, totals = voteTotalsApprovalVoting)
    
    approvalMethodOut
    

    }    
    
