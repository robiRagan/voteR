#' GenSincereProposals
#' Generates a set of sincere proposals to face the status quo policy.
#' 
#' Generates a set of sincere proposals to face the status quo policy. 
#'  For the number of porposals requested, this fucntion will at random select that many voter's
#'  ideal points as the proposals to face the status quo. 
#' @param voterIdealsGenSincereProposals The dataframe of voter ideals usually created with \code{\link{genVoters}}. 
#'    If not creaeted with \code{\link{genVoters}} then the user must make sure the coordinates for the ideal points
#'    in the sumbitted dataframe are named xLocation and yLocation. 
#' @param numberOfSincereProposalsGenSincereProposals Number of sincere proposals to generate. Default is 1.
#' 
#' @return outSincereProposalsDataFrame data.frame The RandomProposals data frame will have the following format.
#'  proposalID: A numeric identifier unique to the voter.
#'  xLocation: The x coordinate of the voter's ideal point.
#'  yLocation: The y coordinate of the voter's ideal point.
#' @examples
#'    someVoters <- genVoters(numberOfDimensionsGenVoters = 2) 
#'   genSincereProposals(voterIdealsGenSincereProposals = someVoters, numberOfSincereProposalsGenSincereProposals = 2) 
#'   
#'   genSincereProposals(voterIdealsGenSincereProposals = someVoters) 
#'   
#' @export
# voterIdealsGenSincereProposals <- genVoters(numberOfDimensionsGenVoters = 2)     # For TESTING 
# numberOfSincereProposalsGenSincereProposals <- 3  # For TESTING
genSincereProposals <- function(voterIdealsGenSincereProposals, numberOfSincereProposalsGenSincereProposals=1){

    # 1) Randomly Select Voters to make the proposal(s)
    proposingVotersRowNumb <- sample(x = c(1:nrow(voterIdealsGenSincereProposals) ), size = numberOfSincereProposalsGenSincereProposals)
    
    
    #2) Subset the voterIdeals dataframe to only the rows for the voters who were selected to propose
    theProposals <- voterIdealsGenSincereProposals[c(proposingVotersRowNumb), c("voterID", "xLocation", "yLocation")]
    
    #3) Create proposalIDs
    
    proposalID = paste( "P",seq(from = 1,to = numberOfSincereProposalsGenSincereProposals), sep="-" )
    
    
    #3) Create a vector of the pointType called "competitor"
    
    pointType <- rep("proposal", numberOfSincereProposalsGenSincereProposals)
    
    
    
    #4) Store Everything in a Data Frame 
    
    outSincereProposalsDataFrame <- data.frame(pointType, proposalID, theProposals )    
    
    # Rename the x and y coordinates to xLocation and yLocation
    # names(outCompetitorDataFrame)[names(outCompetitorDataFrame) == 'xCoord'] <- 'xCompetitor'
    # names(outCompetitorDataFrame)[names(outCompetitorDataFrame) == 'yCoord'] <- 'yCompetitor'
    
    outSincereProposalsDataFrame

}   
