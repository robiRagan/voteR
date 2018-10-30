#' allocateSeats
#' Allocates seats based on the district magnitude (number of seats) and the electoral formula. 
#' 
#' Takes the vote totals calculated by \code{\link{votersVote}} and allocates seats 
#' based on the district magnitude (number of seats) and the electoral formula. 
#' 
#' @param voteTotalsAllocateSeats A 1 x numberOfCompetitors data frame with the vote totals recieved by each competitor. 
#' @param altsDataFrameAllocateSeats a voteR dataFrame that contains the competitorID or alternativeID, and current location of competitors or alternatives being voted on.
#' @param numSeatsAllocateSeats A scalar. The district magnitude (number of seats available).
#' @param electoralFormulaAllocateSeats A string indicating the electoral 
#' @return outSeatAllocation 
#' @export
allocateSeats <- function(voteTotalsAllocateSeats, altsDataFrameAllocateSeats, numSeatsAllocateSeats, electoralFormulaAllocateSeats){

    # TEST ##
    # voteTotalsAllocateSeats <- voteTotals
    # numSeatsAllocateSeats <- numSeats
    # electoralFormulaAllocateSeats <- electoralFormula
    # TEST ##
    

    
        if(electoralFormulaAllocateSeats=="plurality"){
        
            ###############################################
            # Find large to small rank for each competitor based on their
            # number of votes
            ###############################################
            rankOrderOfCompLargeToSmall <- t(apply(X = -voteTotalsAllocateSeats, MARGIN = 1, FUN = rank, na.last="keep", ties.method="random"))
            
            ############################################################################
            # Allocate seats by rank order to all competitors from 1 to numberOfSeats
            ############################################################################
            outSeatAllocation <- ifelse(rankOrderOfCompLargeToSmall <= numSeatsAllocateSeats, 1, 0)
            
            
            outSeatAllocation
        
        }
    
    
        if(electoralFormulaAllocateSeats=="proportional"){
        
            ###################################################################################################
            # For each competitor, find their vote share
            ##################################################################################################
            totalVotesCast <- sum(voteTotalsAllocateSeats, na.rm=TRUE)
        
            voteSharePerCompetitor <- voteTotalsAllocateSeats/totalVotesCast
        
            ###################################################################################################
            # Now mutiply the voteshares by the total number of seats
            ##################################################################################################
        
            rawSeatsPerCompetitor <- voteSharePerCompetitor*numSeatsAllocateSeats
        
            #######################################################################
            # Use roundPreserveSum() to apply the largest remainder method, to round
            # raw seat totals to integers
            ######################################################################
        
            outSeatAllocation <- t( apply(X = rawSeatsPerCompetitor, MARGIN = 1, FUN = roundPreserveSum) )
        
        }
    
    
    outSeatAllocation <- data.frame(outSeatAllocation)
    
    names(outSeatAllocation) <- altsDataFrameAllocateSeats$competitorID
    
    outSeatAllocation

    }   
