#' genCompetitorType
#' Assigns each competitor a type based upon parameters provided by the user. 
#' 
#' Randomly type all competitors in the supoplied dataframe. \code{\link{genCompetitorType}} is used in conjuction 
#' with \code{\link{genIdeals}} by \code{\link{genCompetitors}} to generate
#' the characteristics of eac competitor. 
#' @param numCompetitorsGenCompType scalar Number of voters
#' @param allHunterGenCompType logical If If FALSE (the default) then the distribution of competitor types is determined by the other parameters in this function. If TRUE then all agents will be of the Humter type and will seek votes by first miving at random. If the move increaes vote share then they continue to move in that directon. If the vote share stays the same or decreases then the competitor randomly selects a move that is between 90 and 270 degrees from the direction of their previous move. 
#' @param probabilityHunterGenCompType scalar [0,1] The probability that any given competitor will have the Hunter type. 
#' @param probabilityAggregatorGenCompType scalar [0,1] The probability that any given competitor will have the Aggregator type.
#' @param probabilityPredatorGenCompType scalar [0,1] The probability that any given competitor will have the Predator type.
#' 
#' @return outCompetitorType numCompetitors x 1 A vector of competitor types, one for each competitor.
 # numCompetitorsGenCompType <- 100 ###
 # allHunterGenCompType <- TRUE ###
 # probabilityHunterGenCompType <-1 ###
 # probabilityAggregatorGenCompType <- 0 ###
 # probabilityPredatorGenCompType <- 0 ###
#' @export
genCompetitorType <- function(numCompetitorsGenCompType, allHunterGenCompType=FALSE, probabilityHunterGenCompType=1, probabilityAggregatorGenCompType=0, probabilityPredatorGenCompType=0){
  
    if(allHunterGenCompType==TRUE){probabilityHunterGenCompType <- 1}
      
    if(probabilityHunterGenCompType+probabilityAggregatorGenCompType+probabilityPredatorGenCompType!=1){stop('Something is not quite right here. If you want all the competitors to be of the hunter type then set allHunterGenCompType=TRUE, and leave off the three probability parameters. If instead you want to specify the probability of each type then the parameters probabilityHunterGenCompType, probabilityAggregatorGenCompType, and probabilityPredatorGenCompType must sum to 1.')}
      
    outCompetitorType <- sample(x = c("Hunter","Aggregator","Predator"), size = numCompetitorsGenCompType, replace = TRUE, prob = c(probabilityHunterGenCompType,probabilityAggregatorGenCompType,probabilityPredatorGenCompType))
     
    outCompetitorType
  }