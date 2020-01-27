#' voteByUtilCumu
#' Allocate votes based utility values, cumulation is allowed. 
#'  
#' Voters allocate their votes based on their utillity for each of the alternatives being considered. Once removing any
#' alternatives that are below a voters abstension threshold the utility a voter has for each alternative is calculated. 
#' Then the ratio utilityForAnAlternative/sumOfAllUtilityForAlternatives is calculated for each alternative the voter allocates
#' their numVotes of votes accoording to those ratios. Rounding to whole votes is acomplished using the 
#' "last integer method" with the function \code{\link{roundPreserveSum}}. See ?roundPreserveSum for more details. 
#' This function is usually not called directly but is wrapped by  \code{\link{votersVote}}.
#' @param votersUtilsForAltsVoteByUtilCumu numVoters by numAlts matrix of utility values.
#' @param numVotesPerVoterVoteByUtilCumu the number of votes each voter is allowed to cast. This function does not allow cumulation of votes. 
#' @return outVotesVoteByUtilCumu A 1 x numberOfAlts matrix displaying the number of votes cast for each alternative.
#' @export
voteByUtilCumu <- function(votersUtilsForAltsVoteByUtilCumu, numVotesPerVoterVoteByUtilCumu){
    
    # ## TESTING 1 ##
    # testVotersDataFrame <- genVoters(numberOfDimensionsGenVoters = 2, dimOneBoundsGenVoters = c(0,1), dimTwoBoundsGenVoters = c(0,1), distributionTypeGenVoters = "unif")
    # 
    # testAltsDataFrame <- genCompetitors(numberOfDimensionsGenCompetitors = 2, dimOneBoundsGenCompetitors = c(0,1), dimTwoBoundsGenCompetitors = c(0,1), distributionTypeGenCompetitors = "unif", numberOfCompetitorsGenCompetitors = 7)
    # 
    # 
    # votersUtilsForAltsVoteByUtilCumu <- minkowskiUtilitySets(idealsMatrix = cbind(testVotersDataFrame$xLocation,testVotersDataFrame$yLocation), altsMatrix = cbind(testAltsDataFrame$xLocation, testAltsDataFrame$yLocation), minkoOrderVector = testVotersDataFrame$minkoOrder, lossOrderVector = testVotersDataFrame$lossOrder, salienceMatrix = cbind(testVotersDataFrame$xSalience, testVotersDataFrame$ySalience))
    # 
    # row.names(votersUtilsForAltsVoteByUtilCumu) <- testVotersDataFrame$voterID
    # testAbstentionThreshold <- 1
    # testAbstentionThresholdUtils <- -(testAbstentionThreshold^testVotersDataFrame$lossOrder[1])
    # 
    # votersUtilsForAltsVoteByUtilCumu <- ifelse(votersUtilsForAltsVoteByUtilCumu < testAbstentionThresholdUtils, -Inf, votersUtilsForAltsVoteByUtilCumu)
    # votersUtilsForAltsVoteByUtilCumu <- votersUtilsForAltsVoteByUtilCumu[apply(X=votersUtilsForAltsVoteByUtilCumu, MARGIN=1, FUN=max)!=-Inf, ]
    # numVotesPerVoterVoteByUtilCumu <- 15
    # # TESTING 1 ##
    
  # #  ## FOR TESTING with devVoteScript_SEA18
  # #  and votersVote() ##
  #   votersUtilsForAltsVoteByUtilCumu <- votersUtilsForAltsVotersVote
  #   numVotesPerVoterVoteByUtilCumu <- numVotesPerVoter
  #   #  ## FOR TESTING with devVoteScript_SEA18
    # 

    
    ######################################
    # Replace -Inf with NA
    ######################################
    
    votersUtilsForAltsVoteByUtilCumu <- ifelse(votersUtilsForAltsVoteByUtilCumu==-Inf, NA, votersUtilsForAltsVoteByUtilCumu)
    
    
    ###################################################################################################
    # For each voter, find the ratio of utility for an alternative, to their utility for
    # all alternaitves/competitors above thier abstention threshold. 
    ##################################################################################################
    
    # Before calculating the proportions convert the utility values from negative to positive.
        # I do this by normalizing the utility each voter has for the least preferred alternative
        # that they are still willing to vote for to 1. 
      
      # Find the lowest utility each voter experiences for an alternative they are still willing to vote for, 
     lowestVotingUtilityForEachVoter <- apply(X = votersUtilsForAltsVoteByUtilCumu, MARGIN = 1, FUN = min, na.rm = TRUE)
    
    # Determine the ammount to add to all the voter's utility ammounts in order to normalize the minimum voting utility to 1 
    normalizationValueForEachVoter <- 1 + abs(lowestVotingUtilityForEachVoter)
    
    normalizedUtilityForAltsByUtil <- votersUtilsForAltsVoteByUtilCumu + normalizationValueForEachVoter
    
    normalizedUtilityForAllAlts <- rowSums(normalizedUtilityForAltsByUtil, na.rm=TRUE)
    
    proportionOfUtilityByAlt <- normalizedUtilityForAltsByUtil/normalizedUtilityForAllAlts
    
    ###################################################################################################
    # Now mutiply the utility proportions by the total number of votes each voter has
    ##################################################################################################
    
    rawVotesPerAlt <- proportionOfUtilityByAlt*numVotesPerVoterVoteByUtilCumu
    
    #######################################################################
    # Use roundPreserveSum() to apply the largest integer method, to round
    # raw votes to integers
    ######################################################################
    
    votersVotesForAltsMatrixCumu <- t( apply(X = rawVotesPerAlt, MARGIN = 1, FUN = roundPreserveSum) )
    
    outVotesVoteByUtilCumu <- matrix( colSums(votersVotesForAltsMatrixCumu,na.rm = TRUE), nrow=1)
    
    colnames(outVotesVoteByUtilCumu) <- seq(1:ncol(votersUtilsForAltsVoteByUtilCumu))
    
    outVotesVoteByUtilCumu
    }   
