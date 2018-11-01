#####################################################
# devVoteRScript_SEA18.R
# 10.30.2018
# SEA 2018 version of Pareto Properties of Electoral Systems

# 
# This script was created as a demo for the 
# 2018 Public Choice Meeting 
# Charleston, SC 03.06.18
#####################################################
rm(list = ls(all = TRUE))



##############################################
## LOAD THE PACKAGES (Install if not loaded):
##############################################

# install.packages("devtools") # to install the any package from github you need 
                                # to install the devtools package. 
library(devtools) # this will give you the command install_github() command you need
                    # to install the voteR package from github.    


# install_github("robiRagan/voteR") # This installs the voteR package used to runs the simulations
# install.packages("ggplot2")
# install.packages("sf")

library(voteR)
library(ggplot2)
# library(sf)




##################
# Globals
##################
# The model will run each set of parameters for a number of "runs" of the model.
numRunsGlobal <- 5


# The model will run each set of parameters for number of time periods for each run.
numTimePeriodsGlobal <- 10


# The voteR package can run 1 or two dimensions. For this model we want two.
numDimsGlobal <- 2

# All models have two issue dimensions bounded on the unit square:
dimOneBounds <- c(0,1)
dimTwoBounds <- c(0,1)

# Number of initial status quos:
numInitialStatusQuos <- 100



#####################################
# LOOP PARAMETERS
#   - ROBUSTNESS SWEEP PARAMETERS
#   - ELECTORAL SYSTEM SWEEP PARAMETERS    
####################################

        
    ###############################################################################
    # ROBUSTNESS SWEEP PARAMETERS
    ###############################################################################
    # Robustness Sweep Parameters: These parameters are not the main parameters of 
    # interest with respect to examining how different electoral system properties 
    # the affect the centripidal and centrifucal incentives of competitors.
    #
    # Rather, they are sweeps of other model parameters that could affect the outcome, 
    # so I sweep them to ensure the results of changing the electoral system 
    ##############################################################################
    
    
        # voterDistribution: There are three different distributions from which voter ideal
        # points are drawn: 
            # Normal Narrow: ~N(.5,.1)
            # Normal Wide: ~N(.5,.5)
            # Uniform: ~U(0,1)
            # Bimodal: ~Beta(.1,.1)
        # voterDistributionsSweep <- c("uniform", "normalNarrow", "bimodal") 
# Current Options are "uniform", "narrowNormal", "wideNormal" and "bimodal"
 
voterDistributionsSweep <- c("normalWide") # FOR TESTING

    
    #############################################################################
    # ELECTORAL SYSTEM SWEEP PARAMETERS
    #############################################################################
    # Electoral System Sweep Parameters: These are the parameters of interest in the model
    # 
        # numVoters (N): Number of voters partisipating in the election.
            minNumVoters <- 100
            maxNumVoters <- 100
            stepsNumVoters <-10
            
            numVotersSweep <- seq(minNumVoters, maxNumVoters, by = stepsNumVoters)
        

                
        # abstentionThreshold (a): distance at which a voter will never vote for a candidate
            # Note this number is calculated in distance here, but when it is applied by a voting rule the a voter's loss function
            # converts. Set the second number higher than 0. 0 means that no one votes.
            numAbstentionThresholdSweepSteps <- 3 
            maxEuclDist <- sqrt( (dimOneBounds[1]-dimOneBounds[2])^2 +  (dimTwoBounds[1]-dimTwoBounds[2])^2 ) # This determines the max distance in the space.
            # abstentionThresholdSweep <- seq(maxEuclDist*1.01, maxEuclDist/10, length.out = numAbstentionThresholdSweepSteps) # # Creates the sweep vector.
            abstentionThresholdSweep <- c(.15)
        
        # numVotesPerVoter(nu): Number of votes each voter may cast. 
            minNumVotesPerVoter <- 1
            maxNumVotesPerVoter <- 1
            numVotesPerVoterSweep <- seq(minNumVotesPerVoter, maxNumVotesPerVoter, by = 1)
            
        # cumulationParameter (c): A number that determinies if a voter casts multiple
            # votes for a single competitor If c = 0 then a voter cannot cast multiple votes
            # for the same competitor. If c >= 1 then the voter casts their votes in proportion 
            # to their utility for each competitor's position. If c = 1 then the voter's loss function
        # (which turns euclidian distance into utility) is linear when evaluating the distance of competitors
        # position to their ideal point. If c = 2 then the voter's loss function
            # (which turns euclidian distance into utility) increases and they experence qudratic loss 
            # when evaluating a competitors distance to their ideal point. If c = 3 then the voter's loss 
            # function increases and they experence cubic loss 
            # when evaluating a competitors distance from their ideal point. 
            cumulationParameterSweep <- c(2)
            
        # fmla: The electoral formula that turns votes into seats.
            # plurality: orders the competitors (candidates) by their voteshare and 
            #   the first through the numSeats competitor gets a seat. 
            # proportional: Awards seats to competitors (parties) based on their voteshare.
            #    note that this my result in a competitor recieving more than one seat.    
            # electoralFormulaSweep <- c("plurality", "proportional")
             electoralFormulaSweep <- c("plurality")
  
            
            # Loop iterators for testing when R or RStudio crashes
            
            # dloop <- 1
            # Nloop <- 1
            # aloop <- 1
            # nuloop <- 1
            # cloop <- 1
            # floop <- 1
            # rloop <- 1
            # tloop <-1
            
            
            
# Loop iterators for resuming when R or RStudio crashes

dloopStart <- 1
    NloopStart <- 1
      aloopStart <- 1
        nuloopStart <- 1
          cloopStart <- 1
            floopStart <- 1
              rloopStart <- 1
                tloopStart <-1
  
                                    
                                    
dloop <- dloopStart # FPR TESTING                                     
#  for (dloop in dloopStart:length(voterDistributionsSweep)){      ##### BEGIN dloop Sweep Across Distribution Types ######
                               
                                
    # Set the parameters for genVoters based on the distribution of the current model run.
    if(voterDistributionsSweep[dloop]=="normalNarrow"){
        distributionTypeForRound <- "norm" 
        distributionParametersForRound <- c(.5,.1)
        }
        
    if(voterDistributionsSweep[dloop]=="normalWide"){
        distributionTypeForRound <- "norm" 
        distributionParametersForRound <- c(.5,.5)
        }
        
    if(voterDistributionsSweep[dloop]=="uniform"){
        distributionTypeForRound <- "unif" 
        distributionParametersForRound <- c(0,1)
        }
        
    if(voterDistributionsSweep[dloop]=="bimodal"){
        distributionTypeForRound <- "beta" 
        distributionParametersForRound <- c(.1,.1)
        }

        
                                
                                
    cat("**BEGIN SIMULATION**\n")                            
    
    Nloop <- NloopStart  # FOR TESTING
    #  for (Nloop in NloopStart:length(numVotersSweep)){      ###### BEGIN Nloop Sweep across Number of Voters ######
        numVoters <- numVotersSweep[Nloop]        
    #             
                 aloop <- aloopStart # FOR TESTING
    #              for (aloop in aloopStart:length(abstentionThresholdSweep)){   ###### BEGIN aloop Sweep across abstention threshold ######
                 abstentionThreshold <- abstentionThresholdSweep[aloop]
    #               
                       nuloop <- nuloopStart   # FOR TESTING
    #                  for (nuloop in nuloopStart:length(numVotesPerVoterSweep)){   ###### BEGIN nuloop Sweep across number of votes per voter ######
                     numVotesPerVoter <- numVotesPerVoterSweep[nuloop]
    #                     
                         cloop <- cloopStart # FOR TESTING
    #                     for (cloop in cloopStart:length(cumulationParameterSweep)){   ###### BEGIN cloop Sweep across cumulation parameter ######
                         cumulationParameter <- cumulationParameterSweep[cloop]
    # 
                                  floop <- floopStart  # FOR TESTING
    #                             for (floop in floopStart:length(electoralFormulaSweep)){   ###### BEGIN floop Sweep across cumulation parameter ######
                                 electoralFormula <- electoralFormulaSweep[floop]

 
                                    # Create the Folder to store all the output
                                    sysTimeStamp <- format(Sys.time(),"_%m_%d_%Y_at_%H_%M_%S")    
                                    dir.create(paste("Output/Disn-", distributionTypeForRound,
                                                     "_fmla-", electoralFormula,
                                                     "_N-",numVoters,
                                                     "_nu-",numVotesPerVoter,
                                                     "_c-",cumulationParameter,
                                                     "_a-",round(abstentionThreshold,digits = 2),
                                                     sysTimeStamp, sep=""), showWarnings=FALSE)
                                    
               
                                                                   
                                  rloop <- rloopStart ## FOR TESTING
                                 #       for(rloop in rloopStart:numRunsGlobal){        ###### BEGIN rloop number of total loops to run with a given set of parameters #####
                                        roundNumber <- rloop                                
                                                       
                                    

                                    
                                    
    ##################
    #1) Generate Voters
    ##################
    
 voterIdeals <- genVoters(numberOfDimensionsGenVoters = numDimsGlobal, numberOfVotersGenVoters = numVoters, distributionTypeGenVoters = distributionTypeForRound, distributionParametersGenVoters = distributionParametersForRound, dimOneBoundsGenVoters = dimOneBounds, dimTwoBoundsGenVoters = dimTwoBounds, lossOrderForAllGenVoters = cumulationParameter)
        
    marginalMedianOfIdeals <- data.frame( xMedian=median(voterIdeals$xIdeal), yMedian=median(voterIdeals$yIdeal))                                        
                                        
        write.csv(x = voterIdeals, file = paste("Output/Disn-", distributionTypeForRound,
                                                "_fmla-", electoralFormula,
                                                "_N-",numVoters,
                                                "_nu-",numVotesPerVoter,
                                                "_c-",cumulationParameter,
                                                "_a-",round(abstentionThreshold,digits = 2),
                                                sysTimeStamp,"/round_",roundNumber,"_voterIdeals.csv", sep=""))
        
        
   
        
    ###############################
    #2 Generate Initial Status Quos
    #############################
        
        # altPoints <- altsTest <- c(0,0)
        # 
        # altPoints <- altsTest <- genAlts(numberOfDimensionsGenAlts = 2, numberOfAltsGenAlts = 10, dimOneBoundsGenAlts = dimOneBounds, dimTwoBoundsGenAlts = dimTwoBounds)
        # 
        # plotteRvoteR(votersDataFrame = voterIdeals, altPoints = altsTest, plotIdeals = TRUE, plotAlts = TRUE, plotPareto = TRUE, xBounds = dimOneBounds, yBounds = dimTwoBounds)
    
    # Initialize Initial Status Quo Positions                                    
     initialStatusQuos <- genInitialStatusQuos(numberOfDimensionsGenInitialStatusQuos = numDimsGlobal, numberOfInitialStatusQuosGenInitialStatusQuos = numInitialStatusQuos, distributionTypeGenInitialStatusQuos = "unif", distributionParametersGenInitialStatusQuos = c(-1,1), dimOneBoundsGenInitialStatusQuos = c(0,1), dimTwoBoundsGenInitialStatusQuos = c(0,1) )
 
        
        
    
    ############################
    #3 Generate Random Proposals
    ############################
    # One version of the model looks at a case of random proposals. So here I will generate random proposals for each round to face the status quos
        
    
    # Generate Random Proposals                                 
    randomProposals <- genRandomProposals(numberOfDimensionsGenRandomProposals = numDimsGlobal, numberOfRandomProposalsGenRandomProposals =  numTimePeriodsGlobal, distributionTypeGenRandomProposals = "unif", distributionParametersGenRandomProposals = c(-1,1), dimOneBoundsGenRandomProposals = c(0,1), dimTwoBoundsGenRandomProposals = c(0,1) )
    
    
    
    # # Initialize the outputMatrix if soring each vooter's votes                                
    # outputForOneRun <- data.frame( matrix( NA, nrow = numTimePeriodsGlobal, ncol =  4+(2*numVoters) ) )
    # 
    # # Create two varaoble names for each voter. One where they vote for the SQ and the other where they vote for the proposal. 
    # voterVaraibles <- as.vector( t( outer(voterIdeals$voterID, c("SQ","Prop"), paste, sep="_") ) )
    # names(outputForOneRun) <- c("round","sqInPS", "sq","proposal",voterVaraibles)
    
    
    
    # Initialize the outputMatrix if storign only the total votes per round.                                 
    outputForOneRun <- data.frame( matrix( NA, nrow = numTimePeriodsGlobal, ncol =  4+(2*numVoters) ) )
    
    # Create two varaoble names for each voter. One where they vote for the SQ and the other where they vote for the proposal. 
    voterVaraibles <- as.vector( t( outer(voterIdeals$voterID, c("SQ","Prop"), paste, sep="_") ) )
    names(outputForOneRun) <- c("round","sqInPS", "sq","proposal",voterVaraibles)
    
    
    
    # Calculate the competitors allowed adaptation step size:
    scaledCompetitorAdaptStepSize <- maxEuclDist*competitorAdaptStepProportion
    
    
    
    
    
    ####################################################
    #3) THE TIME PERIOD LOOPS BEGIN HERE
    #####################################################
    
    
        distanceToMarginalMedians <- data.frame(matrix(NA, nrow=numTimePeriodsGlobal, nrow(newProposal)))
        names(distanceToMarginalMedians) <- newProposal$competitorID
    
         for(tloop in tloopStart:numTimePeriodsGlobal){        ###### BEGIN tloop number of total loops to run with a given set of parameters #####
             cat("********Distn:",distributionTypeForRound,"N:",numVoters,"M:",numCompetitors,"S:",numSeats,"a:",abstentionThreshold,"nu:",numVotesPerVoter,"c:",cumulationParameter,"f:",electoralFormula,"ROUND:",rloop,"of",numRunsGlobal,"PERIOD:",tloop,"of",numTimePeriodsGlobal,"\n") 
             
             
             # timePeriod <- tloop # is this needed?       
        
    ## Store the Competitor Positions, VoteTotals, and Seat Allocations found in the last time period 
    ## as the current values of those objects.
    currentCompetitorPositions <- newProposal  
    currentVoteTotals <- newVoteTotals        
    currentSeatAllocation <- newSeatAllocation
    distanceToMarginalMedians[tloop, ] <- distToMargMed(competitorDataFrameDistToMargMed = currentCompetitorPositions, marginalMedianDistToMargMedian = marginalMedianOfIdeals)[1, ]
    
              
    write.csv(x = newProposal, file = paste("Output/Disn-", distributionTypeForRound,
                                                       "_fmla-", electoralFormula,
                                                       "_S-", numSeats,
                                                       "_M-",numCompetitors,
                                                       "_N-",numVoters,
                                                       "_nu-",numVotesPerVoter,
                                                       "_c-",cumulationParameter,
                                                       "_a-",round(abstentionThreshold,digits = 2),
                                                       sysTimeStamp,"/round_",roundNumber,"_time_",tloop,"_CompetitorPositions.csv", sep=""))                    
                    
        
        ####################################################
        #4) Voter's Vote according to their utility function, abstension threshold and cumulation parameter
        #####################################################
            
        newVoteTotals <- votersVote(voterDataFrameVotersVote = voterIdeals, 
                   altsDataFrameVotersVote = currentCompetitorPositions, 
                   abstentionThresholdVotersVote = abstentionThreshold, 
                   numVotesPerVoterVotersVote = numVotesPerVoter,
                   cumulationParameterVotersVote = cumulationParameter)
        
    write.csv(x = newVoteTotals, file = paste("Output/Disn-", distributionTypeForRound,
                                              "_fmla-", electoralFormula,
                                              "_S-", numSeats,
                                              "_M-",numCompetitors,
                                              "_N-",numVoters,
                                              "_nu-",numVotesPerVoter,
                                              "_c-",cumulationParameter,
                                              "_a-",round(abstentionThreshold,digits = 2),
                                              sysTimeStamp,"/round_",roundNumber,"_time_",tloop,"_VoteTotals.csv", sep=""))  
        
    voterUtility <- minkowskiUtilitySets(cbind(voterIdeals$xIdeal, voterIdeals$yIdeal), altsMatrix = cbind(currentCompetitorPositions$xLocation, currentCompetitorPositions$yLocation), minkoOrderVector = voterIdeals$minkoOrder, lossOrderVector = voterIdeals$lossOrder, salienceMatrix = cbind(voterIdeals$xSalience, voterIdeals$ySalience))
    
    write.csv(x = voterUtility, file = paste("Output/Disn-", distributionTypeForRound,
                                              "_fmla-", electoralFormula,
                                              "_S-", numSeats,
                                              "_M-",numCompetitors,
                                              "_N-",numVoters,
                                              "_nu-",numVotesPerVoter,
                                              "_c-",cumulationParameter,
                                              "_a-",round(abstentionThreshold,digits = 2),
                                              sysTimeStamp,"/round_",roundNumber,"_time_",tloop,"_VoterUtility.csv", sep="")) 
    
    
    
    
    
    
         ####################################################
        #5) Seats are allocated according to the Electoral Rule
        #####################################################
        
        newSeatAllocation <- allocateSeats(voteTotalsAllocateSeats = newVoteTotals, altsDataFrameAllocateSeats = currentCompetitorPositions, numSeatsAllocateSeats = numSeats, electoralFormulaAllocateSeats = electoralFormula)
        
    write.csv(x = newSeatAllocation, file = paste("Output/Disn-", distributionTypeForRound,
                                                  "_fmla-", electoralFormula,
                                                  "_S-", numSeats,
                                                  "_M-",numCompetitors,
                                                  "_N-",numVoters,
                                                  "_nu-",numVotesPerVoter,
                                                  "_c-",cumulationParameter,
                                                  "_a-",round(abstentionThreshold,digits = 2),
                                              sysTimeStamp,"/round_",roundNumber,"_time_",tloop,"_SeatAllocations.csv", sep="")) 
        
        outPlot <- plotteRvoteR(votersDataFrame = voterIdeals, competitorPoints = currentCompetitorPositions, plotIdeals = TRUE, plotCompetitors = TRUE, plotVoronoi = TRUE, xBounds = dimOneBounds, yBounds = dimTwoBounds, plotMarginalMedian = TRUE)
        
        suppressMessages( ggsave(plot=outPlot, filename = paste("Output/Disn-", distributionTypeForRound,
                                                                "_fmla-", electoralFormula,
                                                                "_S-", numSeats,
                                                                "_M-",numCompetitors,
                                                                "_N-",numVoters,
                                                                "_nu-",numVotesPerVoter,
                                                                "_c-",cumulationParameter,
                                                                "_a-",round(abstentionThreshold,digits = 2),
                                               sysTimeStamp,"/round_",roundNumber,"_time_",tloop,"_outPlot.pdf", sep=""), width = 15, height = 11, units = "in") )
        
        suppressMessages( ggsave(plot=outPlot, filename = paste("Output/Disn-", distributionTypeForRound,
                                                                "_fmla-", electoralFormula,
                                                                "_S-", numSeats,
                                                                "_M-",numCompetitors,
                                                                "_N-",numVoters,
                                                                "_nu-",numVotesPerVoter,
                                                                "_c-",cumulationParameter,
                                                                "_a-",round(abstentionThreshold,digits = 2),
                                              sysTimeStamp,"/round_",roundNumber,"_time_",tloop,"_outPlot.png", sep=""), width = 15, height = 11, units = "in") )
        
        ################################################
        # 6) Competitors adapt according to their type:
        ###################################################
        
        newProposal <- competitorsAdapt(competitorDataFrameCompetitorAdapts = currentCompetitorPositions,
                                                   voteTotalsForCompetitorsOldCompetitorAdapts = currentVoteTotals,
                                                   voteTotalsForCompetitorsNewCompetitorAdapts = newVoteTotals,
                                                   seatAllocationForCompetitorsNewCompetitorAdapts = currentSeatAllocation,
                                                   seatAllocationForCompetitorsOldCompetitorAdapts = newSeatAllocation,
                                                   electoralFormulaCompetitorAdapts = electoralFormula,
                                                   scaledCompetitorAdaptStepSizeCompetitorAdapts = scaledCompetitorAdaptStepSize,
                                                   dimOneBoundsCompetitorsAdapt = dimOneBounds,
                                                   dimTwoBoundsCompetitorsAdapt = dimTwoBounds
                                                       )
        

                                    } #End the tloop. Time periods within a run of the model. 
                                    
                                    tloop <- 1 # Neet to reset the tloop here for cases where we start or restart the tloop at something other than 1   
        
        # Grab the plot for the final time period
        outPlot2 <- plotteRvoteR(votersDataFrame = voterIdeals, competitorPoints = currentCompetitorPositions, plotIdeals = TRUE, plotCompetitors = TRUE, plotVoronoi = TRUE, xBounds = dimOneBounds, yBounds = dimTwoBounds, plotMarginalMedian = TRUE)
        
        suppressMessages( ggsave(plot=outPlot2, filename = paste("Output/Disn-", distributionTypeForRound,
                                                                 "_fmla-", electoralFormula,
                                                                 "_S-", numSeats,
                                                                 "_M-",numCompetitors,
                                                                 "_N-",numVoters,
                                                                 "_nu-",numVotesPerVoter,
                                                                 "_c-",cumulationParameter,
                                                                 "_a-",round(abstentionThreshold,digits = 2),
                                              sysTimeStamp,"/round_",roundNumber,"_time_",tloop,"_outPlot.pdf", sep=""), width = 15, height = 11, units = "in") )
        
        suppressMessages( ggsave(plot=outPlot2, filename = paste("Output/Disn-", distributionTypeForRound,
                                                                 "_fmla-", electoralFormula,
                                                                 "_S-", numSeats,
                                                                 "_M-",numCompetitors,
                                                                 "_N-",numVoters,
                                                                 "_nu-",numVotesPerVoter,
                                                                 "_c-",cumulationParameter,
                                                                 "_a-",round(abstentionThreshold,digits = 2),
                                              sysTimeStamp,"/round_",roundNumber,"_time_",tloop,"_outPlot.png", sep=""), width = 15, height = 11, units = "in") )
        
            write.csv(x = distanceToMarginalMedians, file = paste("Output/Disn-", distributionTypeForRound,
                                                                  "_fmla-", electoralFormula,
                                                                  "_S-", numSeats,
                                                                  "_M-",numCompetitors,
                                                                  "_N-",numVoters,
                                                                  "_nu-",numVotesPerVoter,
                                                                  "_c-",cumulationParameter,
                                                                  "_a-",round(abstentionThreshold,digits = 2),
                                                      sysTimeStamp,"/round_",roundNumber,"_distancesToMarginalMedian.csv", sep="")) 
        
        
                                } # End the rloop. The numer of rounds each model runs.         
                                rloop <- 1 # We need to restart the rloop here for cases where we start/restart at an rloop other than 1
                
                
                
                
                
                
                            } #End the floop. Sweep across electoral formula parameter.
                            floopStart <- 1 # We need to restart the floop here for cases where we start/restart at an rloop other than 1
                        
                            
                        } #End the cloop. Sweep across cumulation parameter. 
                        cloopStart <- 1 # We need to restart the cloop here for cases where we start/restart at an rloop other than 1
                    
                    } #End the nuloop. Seep across number of voters per voter.
                    nloopStart <- 1 # We need to restart the nloop here for cases where we start/restart at an rloop other than 1
                
                } #End the alooop. Sweep across abstenstion threshold. 
                aloopStart <- 1 # We need to restart the aloop here for cases where we start/restart at an rloop other than 1
    
    } # End the Nloop. Sweep across number of voters. 
    NloopStart <- 1 # We need to restart the Nloop here for cases where we start/restart at an rloop other than 1   
     
}  #End the dloop. Sweep across voter distributions.
dloopStart <- 1 # We need to restart the rloop here for cases where we start/restart at an rloop other than 1               
                                    
            cat("**END SIMULATION**\n")  
        
  
