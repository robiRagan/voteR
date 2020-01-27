#' votersVote
#' Alocates votes for a  set of competitors/alternatives according to a set of model parameters.
#'  
#' Voters cast their votes for the competitors or alternatives in the choice set based on a set of model parameters. 
#' @param voterDataFrameVotersVote a voterR voter dataFrame that contains the voterID, ideal point location, minkowski order, loss order, and salience weights for each voter. 
#' @param altsDataFrameVotersVote a voteR dataFrame that contains the competitorID or alternativeID, and current location of competitors or alternatives being voted on.
#' @param abstentionThresholdVotersVote A voter will not cast a vote for any competitor/alternative that is past this euclidian distance. 
#' @param numVotesPerVoterVotersVote The number of votes a voter may cast in the current electoral system. 
#' @param cumulationParameterVotersVote The degree to which voters want to cumulate votes. If = 0, cumulation is not allowed, For all values > 0 the loss parameter in the voters loss function is used. 
#' @return outVoteTotals A dataFrame containing the vote totals for each alternative or competitor. 
#' @export
votersVote <- function(voterDataFrameVotersVote = NA, altsDataFrameVotersVote = NA, abstentionThresholdVotersVote, numVotesPerVoterVotersVote, cumulationParameterVotersVote){

   #  ## FOR TESTING In ISOLATION ##
   #  voterDataFrameVotersVote <- genVoters(numberOfDimensionsGenVoters = 2, dimOneBoundsGenVoters = c(0,1), dimTwoBoundsGenVoters = c(0,1), distributionTypeGenVoters = "unif")
   #  altsDataFrameVotersVote <- genCompetitors(numberOfDimensionsGenCompetitors = 2, dimOneBoundsGenCompetitors = c(0,1), dimTwoBoundsGenCompetitors = c(0,1), distributionTypeGenCompetitors = "unif", numberOfCompetitorsGenCompetitors = 5)
   #  abstentionThresholdVotersVote <- 2
   #  numVotesPerVoterVotersVote <- 3
   # cumulationParameterVotersVote <- 2
   #
   #  ## FOR TESTING IN ISOLATION ##
    
  
  #
  # #  ## FOR TESTING with devVoteScript_SEA18 ##
  # voterDataFrameVotersVote <-  voterIdeals
  # altsDataFrameVotersVote <- currentCompetitorPositions
  # abstentionThresholdVotersVote <- abstentionThreshold
  # numVotesPerVoterVotersVote <- 3
  # cumulationParameterVotersVote <- cumulationParameter
  # #  ## FOR TESTING with devVoteScript_SEA18 ##
  
  
  # #  ## FOR TESTING with devVoteScript_ParetoProperties ##
  # voterDataFrameVotersVote <-  voterIdeals
  # altsDataFrameVotersVote <- altsForThisRound
  # abstentionThresholdVotersVote <- abstentionThreshold
  # numVotesPerVoterVotersVote <- numVotesPerVoter
  # cumulationParameterVotersVote <- cumulationParameter
  # #  ## FOR TESTING with devVoteScript_ParetoProperties ##
    
    
    
    # First determine the number of issue dimensions in the voterDataFrameVotersVote  and altsDataFrameVotersVote
    # If they are not the samme issue an error.
    
    voterDimensions <- ifelse("xLocation" %in% colnames(voterDataFrameVotersVote) & "yLocation" %in% colnames(voterDataFrameVotersVote), 2, 1)
    altDimensions <- ifelse("xLocation" %in% colnames(altsDataFrameVotersVote) & "yLocation" %in% colnames(altsDataFrameVotersVote), 2, 1)
    
    if(voterDimensions!= altDimensions){ stop('The number of dimensions supplied with voterDataFrameVotersVote is ',voterDimensions, '.\n The number of dimensions supplied with altsDataFrameVotersVote is ', altDimensions, '.\n Voter ideal points and the location of the competitors/alternatives under consideration must have the same dimensionality.')}
    
                ###################
                # COMPUTE UTILITY
                ####################
    # Compute the voter's utility for the competitors/alternatives

    votersUtilsForAltsVotersVote <- minkowskiUtilitySets(idealsMatrix = cbind(voterDataFrameVotersVote$xLocation, voterDataFrameVotersVote$yLocation), altsMatrix = cbind(altsDataFrameVotersVote$xLocation, altsDataFrameVotersVote$yLocation), minkoOrderVector = voterDataFrameVotersVote$minkoOrder, lossOrderVector = voterDataFrameVotersVote$lossOrder, salienceMatrix = cbind(voterDataFrameVotersVote$xSalience, voterDataFrameVotersVote$ySalience))
    
    #Add the VoterIDs as the rownames
    row.names(votersUtilsForAltsVotersVote) <- voterDataFrameVotersVote$ID
    
    ## TESTING
    #votersDistancesToCompetitorsVotersVote <- minkowskiDistanceSets(idealsMatrix = cbind(voterDataFrameVotersVote$xLocation,voterDataFrameVotersVote$yLocation), altsMatrix = cbind(altsDataFrameVotersVote$xLocation, altsDataFrameVotersVote$yLocation), minkoOrderVector = voterDataFrameVotersVote$minkoOrder, salienceMatrix = cbind(voterDataFrameVotersVote$xSalience, voterDataFrameVotersVote$ySalience)) 
    
    
            ##############################
            # APPLY ABSTENTION THRESHOLD
            #############################
    
    # Convert the abstention parameter into utility 
    abstentionThresholdUtilsVotersVote <- -(abstentionThresholdVotersVote^voterDataFrameVotersVote$lossOrder[1])
    
     # Give all voters a utility of negative infiity (-Inf) for alternatives/competitors who give them less utility than the abstention threshold (in utils)
        # This keeps them from casting any votes for these candidates.
    votersUtilsForAltsVotersVote <- ifelse(votersUtilsForAltsVotersVote < abstentionThresholdUtilsVotersVote, -Inf, votersUtilsForAltsVotersVote)
    
    # Remove any voters who want to fully abstain because all alternatives give them -Inf utility.
          # First determine if any of the rows contain all -Inf
          rowHasAllNegInfUtil <- matrix(apply(X=votersUtilsForAltsVotersVote, MARGIN=1, FUN=max)==-Inf, byrow = TRUE)
    
          votersUtilsForAltsVotersVote <- votersUtilsForAltsVotersVote[!rowHasAllNegInfUtil, ]


        ## If at least one voter wants to vote continue with the voting. 
        ## If no one wants to vote jump to the else{}
        if(nrow(votersUtilsForAltsVotersVote)>=1){    # STARTS THE VOTING IF AT LEAST ONE PERSON WANTS TO VOTE
    
    
    ####################################
    # ALLOCATE nu VOTES for EACH VOTER
    ###################################
              if(cumulationParameterVotersVote==0){  # First cases where cumulation is not allowed
        
                votersVoteFor <- voteByUtilNoCumu(votersUtilsForAltsVoteByUtilNoCumu = votersUtilsForAltsVotersVote, numVotesPerVoterVoteByUtilNoCumu = numVotesPerVoterVotersVote)

              } else {  # Now for cases where cumulation is allowed
        
                votersVoteFor <- voteByUtilCumu(votersUtilsForAltsVoteByUtilCumu = votersUtilsForAltsVotersVote, numVotesPerVoterVoteByUtilCumu = numVotesPerVoterVotersVote)  
        
              } # ends the else for cases where voters can cumulate
    
        
        } else { # What about cases where no one votes
        
        votersVoteFor <- matrix(0, nrow = 1, ncol = nrow(altsDataFrameVotersVote))
        
    } # end else for the cases where no one votes
    
    
    outVoteTotals <- data.frame(votersVoteFor)
    
    names(outVoteTotals) <- altsDataFrameVotersVote$ID
    
    outVoteTotals
    }   
