#' huntersAdapt
#' Hunters Adaptation rule
#' 
#' Hunters compare their vote share or seat share in an election at t, to their vote share or seat share in the election at t-1.
#' If the vote share/ seat share improved they continue moving in the same direction.
#' If the vote share/ seat share did not improve they randomly chooses a new heading between 90 to 270 degrees away from its previous heading.
#' 
#' @param competitorDataFrameHunterAdapts a voteR dataFrame that contains the competitorID, current location, and comeptitor type for each competitor.
#' @param voteTotalsForCompetitorsOldHunterAdapts a numCOmpetitors x 2 matrix containing each competitor's competitorID and their vote share from the previous period.  
#' @param voteTotalsForCompetitorsNewHunterAdapts a numCOmpetitors x 2 matrix containing each competitor's competitorID and their vote share from the current period.
#' @param seatAllocationForCompetitorsNewHunterAdapts a numCOmpetitors x 2 matrix containing each competitor's competitorID and their vote share from the previous period.  
#' @param seatAllocationForCompetitorsOldHunterAdapts a numCOmpetitors x 2 matrix containing each competitor's competitorID and their vote share from the current period.
#' @param electoralFormulaHunterAdapts The electoral formula being used in the model. In Plurality competitors maximize vote share, in PR systems they maximize seats.
#' @param scaledCompetitorAdaptStepSizeHunterAdapts Size step the party can take. Should be in cartesian distance units.
#' @param dimOneBoundsHuntersAdapt The bounds of the first dimension.
#' @param dimTwoBoundsHuntersAdapt The bounds of the second dimension.
#' @return outCompetitorDataFrameCompetitorAdapts
#'   
#' @export
huntersAdapt <- function(competitorDataFrameHunterAdapts, voteTotalsForCompetitorsOldHunterAdapts, voteTotalsForCompetitorsNewHunterAdapts, seatAllocationForCompetitorsNewHunterAdapts, seatAllocationForCompetitorsOldHunterAdapts, electoralFormulaHunterAdapts, scaledCompetitorAdaptStepSizeHunterAdapts, dimOneBoundsHuntersAdapt, dimTwoBoundsHuntersAdapt){
    ## TEST ##
    # competitorDataFrameHunterAdapts <- currentCompetitorPositions
    # voteTotalsForCompetitorsOldHunterAdapts <- newVoteTotals
    # voteTotalsForCompetitorsNewHunterAdapts <- voteTotalsForCompetitorsOldHunterAdapts + sample(x=c(-1,0,1), size = ncol(voteTotalsForCompetitorsOldHunterAdapts), replace = TRUE)
    # seatAllocationForCompetitorsOldHunterAdapts <- currentSeatAllocation
    # seatAllocationForCompetitorsNewHunterAdapts <- newSeatAllocation
    # electoralFormulaHunterAdapts <- electoralFormula
    # scaledCompetitorAdaptStepSizeHunterAdapts <- scaledCompetitorAdaptStepSize
    # dimOneBoundsHuntersAdapt <- dimOneBounds
    # dimTwoBoundsHuntersAdapt <- dimTwoBounds
    ## TEST ##
        
    ## Subset the competitorDataFrame to only competitors of type "hunter"
   
     
    onlyHunters <- subset(x = competitorDataFrameHunterAdapts, competitorDataFrameHunterAdapts$competitorType=="Hunter")
    
    
    if(electoralFormulaHunterAdapts=="plurality"){
        ## Determine if vote share improved
        voteShareImproved <- (voteTotalsForCompetitorsNewHunterAdapts-voteTotalsForCompetitorsOldHunterAdapts > 0)
        
        ## For competitors whose voteshare did not improve they should draw a new headding
        
        competitorDataFrameHunterAdapts$headingRadians[voteShareImproved!=TRUE] <- hunterNewHeading(competitorDataFrameHunterAdapts$headingRadians[voteShareImproved!=TRUE])
        
    }
    
    if(electoralFormulaHunterAdapts=="proportional"){
        seatShareImproved <- (seatAllocationForCompetitorsNewHunterAdapts-seatAllocationForCompetitorsOldHunterAdapts > 0)
        
        ## For competitors whose voteshare did not improve they should draw a new headding
        competitorDataFrameHunterAdapts$headingRadians[seatShareImproved!=TRUE] <- hunterNewHeading(competitorDataFrameHunterAdapts$headingRadians[seatShareImproved!=TRUE])
        
    }
    # plotteRvoteR(competitorPoints = competitorDataFrameHunterAdapts, plotCompetitors = TRUE, xBounds = c(0,1), yBounds = c(0,1), plotVoronoi = TRUE)
    ### Hunters now take one step
    
   tempNewLocMatrix <- matrix(NA, nrow = nrow(competitorDataFrameHunterAdapts), ncol = 2)
    for (i in 1:nrow(competitorDataFrameHunterAdapts)){
    tempNewLocMatrix[i, ] <- moveDistDAtAngleT(
                                origPointMoveDistDAtAngleT = cbind(competitorDataFrameHunterAdapts$xLocation[i], competitorDataFrameHunterAdapts$yLocation[i]), 
                                distanceDMoveDistDAtAngleT = scaledCompetitorAdaptStepSizeHunterAdapts, 
                                angleTMoveDistDAtAngleT = competitorDataFrameHunterAdapts$headingRadians[i], 
                                dimOneBoundsMoveDistDAtAngleT = dimOneBoundsHuntersAdapt, 
                                dimTwoBoundsMoveDistDAtAngleT = dimTwoBoundsHuntersAdapt
                                            )
        } # end loop acoss making steps
    
   outCompetitorDataFrameCompetitorAdapts <- competitorDataFrameHunterAdapts
   
   outCompetitorDataFrameCompetitorAdapts$xLocation <- tempNewLocMatrix[ ,1]
   outCompetitorDataFrameCompetitorAdapts$yLocation <- tempNewLocMatrix[ ,2]
   
   # plotteRvoteR(competitorPoints = outCompetitorDataFrameCompetitorAdapts, plotCompetitors = TRUE, xBounds = c(0,1), yBounds = c(0,1), plotVoronoi = TRUE)
    

    
    outCompetitorDataFrameCompetitorAdapts

    }   

