#####################################################
# Business Scholarship Challenge Example
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


# 1) Import the GOogle Sheets

voterDataFrame <- read.csv("data/Untitled form (Responses) - Form Responses 1.csv") 


# 2) Create three voter data frames to test with:

names(voterDataFrame) <- c("time", "Reeses", "Skittles", "AirHeads", "MilkyWay", "BlowPop")

# 3) Remove the timestamp

voterPreferences <- voterDataFrame[ ,2:ncol(voterDataFrame)]


# 4) Apply Plurality Rule
pluralityChoice <- usePluralityRule(voterPreferences)
pluralityChoice$winners

# 6) Apply Plurality Runnoff

pluralityRunnoffChoice <- usePluralityRunoffRule(voterPreferences)
pluralityRunnoffChoice$winners

# 7) Sequential Runnoff

sequentialRunnoffChoice <- useSequentialRunoffRule(voterPreferences)
sequentialRunnoffChoice

# 8) Borda Count
bordaCountChoice <- useBordaCount(voterPreferences)
bordaCountChoice

# 9) Condorcet Method
condorcetMethodChoice <- useCondorcetMethod(voterPreferences)
condorcetMethodChoice

# 10) Approval Method
approvalMethodChoice <- useApprovalVoting(voterPreferences, forceUniqueChoiceApprovalVoting = FALSE)
approvalMethodChoice$winners

