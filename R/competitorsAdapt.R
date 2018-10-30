#' competitorsAdapt
#' Competitors Adapt According to their type/
#' 
#' Takes a voter data frame, a competitor position data frame, the voters utility values for all competitors over the past two time periods and the voteTotals of all competitors from the past 2 time periods and applies the appropriate adaptation rule for each competitor based on their competitor type.  
#' 
#' @param competitorDataFrameCompetitorAdapts a voteR dataFrame that contains the competitorID, current location, and comeptitor type for each competitor.
#' @param voteTotalsForCompetitorsOldCompetitorAdapts a numCOmpetitors x 2 matrix containing each competitor's competitorID and their vote share from the previous period.  
#' @param voteTotalsForCompetitorsNewCompetitorAdapts a numCOmpetitors x 2 matrix containing each competitor's competitorID and their vote share from the current period.
#' @param seatAllocationForCompetitorsNewCompetitorAdapts a numCOmpetitors x 2 matrix containing each competitor's competitorID and their vote share from the previous period.  
#' @param seatAllocationForCompetitorsOldCompetitorAdapts a numCOmpetitors x 2 matrix containing each competitor's competitorID and their vote share from the current period.
#' @param electoralFormulaCompetitorAdapts The electoral formula being used in the model. In Plurality competitors maximize vote share, in PR systems they maximize seats. 
#' @param scaledCompetitorAdaptStepSizeCompetitorAdapts Size step the party can take. Should be in cartesian distance units.
#' @param dimOneBoundsCompetitorsAdapt The bounds of the first dimension.
#' @param dimTwoBoundsCompetitorsAdapt The bounds of the second dimension.
#' @return outCompetitorDataFrameCompetitorAdapts
#' @export
competitorsAdapt <- function(competitorDataFrameCompetitorAdapts, voteTotalsForCompetitorsOldCompetitorAdapts, voteTotalsForCompetitorsNewCompetitorAdapts, seatAllocationForCompetitorsNewCompetitorAdapts, seatAllocationForCompetitorsOldCompetitorAdapts, electoralFormulaCompetitorAdapts, scaledCompetitorAdaptStepSizeCompetitorAdapts, dimOneBoundsCompetitorsAdapt, dimTwoBoundsCompetitorsAdapt){
    ## TEST ##
    # competitorDataFrameCompetitorAdapts <- currentCompetitorPositions
    # voteTotalsForCompetitorsOldCompetitorAdapts <- currentVoteTotals
    # voteTotalsForCompetitorsNewCompetitorAdapts <- newVoteTotals
    # seatAllocationForCompetitorsOld <- currentSeatAllocation
    # seatAllocationForCompetitorsNew <- newSeatAllocation
    # electoralFormulaCompetitorAdapts <- electoralFormula
    # scaledCompetitorAdaptStepSizeCompetitorAdapts <- scaledCompetitorAdaptStepSize
    # dimOneBoundsCompetitorsAdapt <- dimOneBounds
    # dimTwoBoundsCompetitorsAdapt <- dimTwoBounds
    ## TEST ##
    
    
    if(any(competitorDataFrameCompetitorAdapts$competitorType=="Hunter")){
        
        newHunters <- huntersAdapt(
            competitorDataFrameHunterAdapts = competitorDataFrameCompetitorAdapts,
            voteTotalsForCompetitorsOldHunterAdapts = voteTotalsForCompetitorsOldCompetitorAdapts,
            voteTotalsForCompetitorsNewHunterAdapts = voteTotalsForCompetitorsNewCompetitorAdapts,
            seatAllocationForCompetitorsNewHunterAdapts = seatAllocationForCompetitorsOldCompetitorAdapts,
            seatAllocationForCompetitorsOldHunterAdapts = seatAllocationForCompetitorsNewCompetitorAdapts,
            electoralFormulaHunterAdapts = electoralFormulaCompetitorAdapts,
            scaledCompetitorAdaptStepSizeHunterAdapts = scaledCompetitorAdaptStepSizeCompetitorAdapts,
            dimOneBoundsHuntersAdapt = dimOneBoundsCompetitorsAdapt,
            dimTwoBoundsHuntersAdapt = dimTwoBoundsCompetitorsAdapt
                )
    }
    
    
    
    if(any(competitorDataFrameCompetitorAdapts$competitorType!="Hunter")){
        
    stop("The competitor type was wither not sepcified in competitorDataFrameCompetitorAdapts, or the type listed has not been implemented yet. As of right now the types implemented are: Hunter.")
        
    }
    
    
    
    outCompetitorDataFrameCompetitorAdapts <- newHunters
    
    outCompetitorDataFrameCompetitorAdapts

    }   
