#####################################################
# This is an R script I use when doing development work on the package. The end user should not have a need to use this script.
#
# Used to write and test code for looking at different voting Rules
#
#####################################################
rm(list = ls(all = TRUE))


#################
# R OPTIONS
#################
options(scipen=2) #scientific notation off
options(digits=10)



##############################################
## LOAD THE PACKAGES (Install if not loaded):
##############################################
library(voteR)
  

####################################################################
# 
####################################################################

set.seed(123) # for development purposes




# 1) Create three sets of alternatives for testing purposes
    
    staticAltsDataFrame1 <- data.frame(pointType = rep(x = "alternative", 5), ID = c("Reeces", "Skittles", "Air Heads", "Milky Way", "Blow Pops"), xLocation=c(-3/8, 1/8, 2/8, 7/8, 4/8), yLocation=c(-3/8, 1/8, 7/8, -4/8, -5/8) )

# 2) Create three voter data frames to test with:

generatedVoterDataFrame1 <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 45, distributionTypeGenVoters = "unif")

# 3) Calculate Minkowsi Utility of each voter from each alt:

rawUtility <- calcMinkowskiUtilityVotersAlts(votersCalcMinkowskiDistanceVotersAlts = generatedVoterDataFrame1, alternativesCalcMinkowskiDistanceVotersAlts = staticAltsDataFrame1)

rawUtility

# 4) Generate an approval threshold for testing. 

votersApprovalThreshold <- rowMeans(rawUtility)

# 5) Apply Plurality Rule

pluralityChoice <- usePluralityRule(rawUtility)
pluralityChoice

# 6) Apply Plurality Runnoff

pluralityRunnoffChoice <- usePluralityRunoffRule(rawUtility)
pluralityRunnoffChoice

# 7) Sequential Runnoff

sequentialRunnoffChoice <- useSequentialRunoffRule(rawUtility)
sequentialRunnoffChoice

# 8) Borda Count
bordaCountChoice <- useBordaCount(rawUtility)
bordaCountChoice

# 9) Condorcet Method
condorcetMethodChoice <- useCondorcetMethod(rawUtility)
condorcetMethodChoice

# 10) Approval Method
  # To test I need a case where there are zeros
  theAlts <- c("Reeces", "Skittles", "Air Heads", "Milky Way", "Blow Pops")
  voterPreferencesApprovalVoting  <- matrix(data = sample( x = c(0:10), size = (45*length(theAlts) ), replace = TRUE ), nrow = 45, ncol = length(theAlts) )
  voterPreferencesApprovalVoting <- data.frame(voterPreferencesApprovalVoting)

approvalMethodChoice <- useApprovalVoting(voterPreferencesApprovalVoting, forceUniqueChoiceApprovalVoting = FALSE)
approvalMethodChoice

