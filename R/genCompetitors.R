#' genCompetitors
#' Generates a set of Competitors (candidates or parties) that can adjust their location in the space.
#' 
#' Generates a set of competitors, each with a competitorID number, an initial location using \code{\link{genIdeals}}, 
#' a vote seekign strategy using \code{\link{genStrategy}}.
#'  
#' @param numberOfDimensionsGenCompetitors The number of policy dimensions.
#' @param numberOfCompetitorsGenCompetitors Number of competitors to generate.
#' @param distributionTypeGenCompetitors A string identifying the base R discribution to draw
#'   the ideal points from. Uses the base R random number generation family of
#'   commands rxxxx (see ?distributions). The user should specify the
#'   distribution as a string using the standard R abreviation for the
#'   distribution (see ?distributions for a list). Currently supported are: "norm",
#'   "unif", "binom", "cauchy", "chisq", "weibull"
#' @param distributionParametersGenCompetitors A vector that contains the additional
#'   parameters needed by the particular rxxxx function for a distribtuion. (see
#'   ?rxxxx where xxxx is a function listed under ?distribution). Example for a
#'   Normal(0,1), use: c(0,1).
#' @param numDimsGenCompetitors scalar Number of policy dimensions. Can be 1 or 2. If this is set to 1, then the salience of that one dimension will be 1 for all voters. 
#' @param dimOneBoundsGenCompetitors A vector that contains the starting and ending poitns of t
#' he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf) if no boundary is provided. 
#' @param dimTwoBoundsGenCompetitors A vector that contains the starting and ending poitns of t
#' he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf)  if no boundary is provided. 
#' @param allHunterGenCompetitors logical If If FALSE (the default) then the distribution of competitor types is determined by the other parameters in this function. If TRUE then all agents will be of the Humter type and will seek votes by first miving at random. If the move increaes vote share then they continue to move in that directon. If the vote share stays the same or decreases then the competitor randomly selects a move that is between 90 and 270 degrees from the direction of their previous move. 
#' @param probabilityHunterGenCompetitors scalar [0,1] The probability that any given competitor will have the Hunter type. 
#' @param probabilityAggregatorGenCompetitors scalar [0,1] The probability that any given competitor will have the Aggregator type.
#' @param probabilityPredatorGenCompetitors scalar [0,1] The probability that any given competitor will have the Predator type.
#' 
#' @return outCompetitorsDataFrame data.frame The competitors data frame will have the following format.
#' 
#'  voterID: A numeric identifier unique to the voter.
#'  xIdeal: The x coordinate of the voter's ideal point.
#'  yIdeal: The y coordinate of the voter's ideal point.
#'  minkoOrder: The Minkowski order of the voters MInkowski metric based utility function. = 1, is City Block. = 2 is Euclidian and 100 = is  See ?Minkowski. 
#'  xSalience: The salience of the x dimension for the voter. The dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#'  ySalience: The salience of the y dimension for the voter. he dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#' @examples
#'   genCompetitors(numberOfDimensionsGenCompetitors=1, numberOfCompetitorsGenCompetitors=10, distributionTypeGenCompetitors ="unif", distributionParametersGenCompetitors = c(-1,1), dimOneBoundsGenCompetitors = c(-Inf,Inf), dimTwoBoundsGenCompetitors = c(-Inf,Inf), allHunterGenCompetitors = TRUE, probabilityHunterGenCompetitors = 0, probabilityAggregatorGenCompetitors=0, probabilityPredatorGenCompetitors=0)
#'  
#' @export
# numberOfDimensionsGenCompetitors=2
# numberOfCompetitorsGenCompetitors=10
# distributionTypeGenCompetitors ="unif"
# distributionParametersGenCompetitors = c(-1,1)
# dimOneBoundsGenCompetitors = c(-Inf,Inf)
# dimTwoBoundsGenCompetitors = c(-Inf,Inf)
# allHunterGenCompetitors = TRUE
# probabilityHunterGenCompetitors = 0
# probabilityAggregatorGenCompetitors=0
# probabilityPredatorGenCompetitors=0
genCompetitors <- function(numberOfDimensionsGenCompetitors=1, numberOfCompetitorsGenCompetitors=3, distributionTypeGenCompetitors ="unif", distributionParametersGenCompetitors = c(-1,1), dimOneBoundsGenCompetitors = c(-Inf,Inf), dimTwoBoundsGenCompetitors = c(-Inf,Inf), allHunterGenCompetitors = TRUE, probabilityHunterGenCompetitors = 0, probabilityAggregatorGenCompetitors=0, probabilityPredatorGenCompetitors=0){

    # 1) Generate Ideals
    tempIdeals <- genIdeals(numberOfDimensionsGenIdeals = numberOfDimensionsGenCompetitors, numberOfIdealsGenIdeals = numberOfCompetitorsGenCompetitors, distributionTypeGenIdeals = distributionTypeGenCompetitors, distributionParametersGenIdeals = distributionParametersGenCompetitors, dimOneBoundsGenIdeals = dimOneBoundsGenCompetitors, dimTwoBoundsGenIdeals = dimTwoBoundsGenCompetitors)
    
    # 2) Generate a Competitor Type
    
    competitorType <- genCompetitorType(numCompetitorsGenCompType = numberOfCompetitorsGenCompetitors, allHunterGenCompType = allHunterGenCompetitors, probabilityHunterGenCompType = probabilityHunterGenCompetitors, probabilityAggregatorGenCompType = probabilityAggregatorGenCompetitors, probabilityPredatorGenCompType = probabilityPredatorGenCompetitors)

    #3) Create a vector of the pointType called "competitor"
    
    pointType <- rep("competitor", numberOfCompetitorsGenCompetitors)
    
    #4) Create CompetitorIDs
    
    ID = paste( "C",seq(from = 1,to = numberOfCompetitorsGenCompetitors), sep="-" )
    
    #5) Create the competitors direction. This is not used by all party types but is generated for all.
    
    headingRadians <- genCompetitorHeading(numCompetitorsGenCompHeading = numberOfCompetitorsGenCompetitors)
    
    #6) Store Everything in a Data Frame 
    
    if(numberOfDimensionsGenCompetitors==1){
        outCompetitorDataFrame <- data.frame(pointType, ID, xLocation=tempIdeals[ ,1], competitorType, headingRadians)    
    }
        
    if(numberOfDimensionsGenCompetitors==2){
    outCompetitorDataFrame <- data.frame(pointType, ID, xLocation=tempIdeals[ ,1], yLocation=tempIdeals[ ,2], competitorType, headingRadians)    
    }
    
    
    outCompetitorDataFrame

}   
