#' voteByUtilNoCumu
#' Allocate votes based utility values, no cumulation is allowed. 
#'  
#' Voters allocate their votes based on their preference prdering as determined by their utility function.
#' They are not allowd to vote for a competitor or alternative more than once. This function is usually not called directly 
#' but is wrapped by  \code{\link{votersVote}}.
#' @param votersUtilsForAltsVoteByUtilNoCumu numVoters by numAlts matrix of utility values.
#' @param numVotesPerVoterVoteByUtilNoCumu the number of votes each voter is allowed to cast. This function does not allow cumulation of votes. 
#' @return outVotesVoteByUtilNoCumu A 1 x numberOfAlts matrix displaying the number of votes cast for each alternative.
#' @export
voteByUtilNoCumu <- function(votersUtilsForAltsVoteByUtilNoCumu, numVotesPerVoterVoteByUtilNoCumu){
    
    ## TESTING ##
    # testVotersDataFrame <- voterIdeals
    # testAltsDataFrame <- currentCompetitorPositions
    # numVotesPerVoterVoteByUtilNoCumu <- numVotesPerVoter
    # testAbstentionThreshold <- 1
    # 
    # 
    # votersUtilsForAltsVoteByUtilNoCumu <- minkowskiUtilitySets(idealsMatrix = cbind(testVotersDataFrame$xIdeal,testVotersDataFrame$yIdeal), altsMatrix = cbind(testAltsDataFrame$xLocation, testAltsDataFrame$yLocation), minkoOrderVector = testVotersDataFrame$minkoOrder, lossOrderVector = testVotersDataFrame$lossOrder, salienceMatrix = cbind(testVotersDataFrame$xSalience, testVotersDataFrame$ySalience))
    # 
    # row.names(votersUtilsForAltsVoteByUtilNoCumu) <- testVotersDataFrame$voterID
    # 
    # testAbstentionThresholdUtils <- -(testAbstentionThreshold^testVotersDataFrame$lossOrder[1])
    # 
    # votersUtilsForAltsVoteByUtilNoCumu <- ifelse(votersUtilsForAltsVoteByUtilNoCumu < testAbstentionThresholdUtils, -Inf, votersUtilsForAltsVoteByUtilNoCumu)
    # votersUtilsForAltsVoteByUtilNoCumu <- votersUtilsForAltsVoteByUtilNoCumu[apply(X=votersUtilsForAltsVoteByUtilNoCumu, MARGIN=1, FUN=max)!=-Inf, ]

    
    ## TESTING ##
    
    ######################################
    # Replace -Inf with NA
    ######################################
    
    votersUtilsForAltsVoteByUtilNoCumu <- ifelse(votersUtilsForAltsVoteByUtilNoCumu==-Inf, NA, votersUtilsForAltsVoteByUtilNoCumu)
    
    
    ###############################################
    # Find large to small rank for each alternative
    ###############################################
    rankOrderOfAltsLargeToSmall <- t(apply(X = -votersUtilsForAltsVoteByUtilNoCumu, MARGIN = 1, FUN = rank, na.last="keep"))
    
    ############################################################################
    # Voter cast votes for all alts from 1 to numVotesPerVoterVoteByUtilNoCumu
    ############################################################################
    votersVotesForAltsMatrix <- ifelse(rankOrderOfAltsLargeToSmall <= numVotesPerVoterVoteByUtilNoCumu, 1, 0)
    
    outVotesVoteByUtilNoCumu <- matrix(colSums(votersVotesForAltsMatrix, na.rm = TRUE), nrow = 1)
    
    colnames(outVotesVoteByUtilNoCumu) <- seq(1:ncol(votersUtilsForAltsVoteByUtilNoCumu))
    
    outVotesVoteByUtilNoCumu
    
    }   
