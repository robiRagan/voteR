#####################################################
# Voting Rules Exmaples with Shiny Server
#
#####################################################
rm(list = ls(all = TRUE))


# install.packages("shiny")

# library(shiny)

# Generate a preference matrix for testing

prefOrder <- matrix(NA,5,21)
for (i in 1:ncol(prefOrder)){
  prefOrder[,i] <- sample(c(1:5),5,replace=FALSE)
}
#Generate Approval Voting Threshold
approveAbove <- sample(c(1:nrow(prefOrder)),ncol(prefOrder),replace=TRUE)

# Functions
topChoice <- function(inOrderMatrix){
  mostPrefOfRem <- rep(NA,ncol(inOrderMatrix))
  for (i in 1:ncol(inOrderMatrix)){
    mostPrefOfRem[i] <- inOrderMatrix[,i][is.na(inOrderMatrix[,i])==FALSE][1]
  }
  mostPrefOfRem
}

pairwiseVote <- function(inOrderMatrix,indexOfAlt1=NA,indexOfAlt2=NA){
  topTwoAlts <- c(indexOfAlt1,indexOfAlt2)
  pairwiseOrder <- rbind(inOrderMatrix[indexOfAlt1,],inOrderMatrix[indexOfAlt2,])
  topChoice <- apply(pairwiseOrder,2,which.max)
  indexOfTopChoice <- as.numeric(which.max(table(topChoice)))
  topTwoAlts[indexOfTopChoice]
}

# pluralityRule based on top choice. Ties broken at random.
singlePlurality <- function(inOrderMatrix){
    topChoice <- as.data.frame(table(inOrderMatrix[1,]))
#     topChoiceSorted <- topChoice[with(topChoice, order(-Freq)),]
    allTopVoteGetters <- subset(topChoice,topChoice$Freq==max(topChoice$Freq),Var1)
    pickOneAtRandom <- sample(as.vector(allTopVoteGetters$Var1),1)
   as.numeric(pickOneAtRandom)
}

lowestVotes <- function(inOrderMatrix){
  topChoice <- as.data.frame(table(inOrderMatrix[1,]))
  allBottomVoteGetters <- subset(topChoice,topChoice$Freq==min(topChoice$Freq),Var1)
  pickOneAtRandom <- sample(as.vector(allBottomVoteGetters$Var1),1)
  as.numeric(pickOneAtRandom)
}



# pluralityRule w/ top 2 runnoff based on top choice. Ties broken at random.
pluralityRunoff <- function(inOrderMatrix){
  orderMatrix <- inOrderMatrix
  firstPlaceWinner <- singlePlurality(orderMatrix)
  matchWinner <- match(orderMatrix, firstPlaceWinner, 0)
  orderMatrix[matchWinner > 0] <- NA
  secondPlaceWinner <- singlePlurality(orderMatrix)
  pairwiseVote(inOrderMatrix,firstPlaceWinner,secondPlaceWinner)
}

sequentialRunoff <- function(inOrderMatrix){
  orderMatrix <- inOrderMatrix
  for (i in 1:(nrow(inOrderMatrix)-1)){
    mostPrefered <- topChoice(orderMatrix)
    leastNumVotes <- lowestVotes(orderMatrix)
    matchLoser <- match(orderMatrix, leastNumVotes, 0)
    orderMatrix[matchLoser > 0] <- NA
  }
unique(topChoice(orderMatrix))
}

bordaCount <- function(inOrderMatrix){
  totals <- rowSums(inOrderMatrix)
  as.numeric(which.max(totals))
}



condorcetProcedure <- function(inOrderMatrix){
  allCombos <- expand.grid(c(1:nrow(inOrderMatrix)),c(1:nrow(inOrderMatrix)))
  allUniqueCombos <- subset(allCombos,Var1!=Var2)
  eachPairWinner <- rep(NA,nrow(allUniqueCombos))
  for (i in 1:nrow(allUniqueCombos)){
    eachPairWinner[i] <- pairwiseVote(inOrderMatrix,allUniqueCombos[i,1],allUniqueCombos[i,2])
  }
  if (length(unique(eachPairWinner))==1){
   unique(eachPairWinner)
  } else {cat("There is no Condorcet Winner")}
}

inOrderMatrix <- prefOrder 
inApproveAbove <- approveAbove
approvalVoting <- function(inOrderMatrix,inApproveAbove){
  approvalMatrix <- matrix(NA,nrow(inOrderMatrix),ncol(inOrderMatrix))
  for (i in 1:ncol(inOrderMatrix)){
    approvalMatrix[,i] <- c(rep(1,inApproveAbove[i]),rep(0,nrow(inOrderMatrix)-inApproveAbove[i]))
  }
  totals <- rowSums(approvalMatrix)
  as.numeric(which.max(totals))
}
