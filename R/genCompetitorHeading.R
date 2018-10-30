#' genCompetitorHeading
#' Assigns each competitor a heading in radians.
#' 
#' Gives an angle in radians for each competitor. For competitors with types that move this is their inital heading. 
#' \code{\link{genCompetitorHeading}} is used in conjuction 
#' with \code{\link{genCompetitorHeading}} and\code{\link{genIdeals}} by \code{\link{genCompetitors}} to generate
#' the characteristics of each competitor. 
#' @param numCompetitorsGenCompHeading scalar Number of voters
#' 
#' @return outCompetitorType numCompetitors x 1 A vector of competitor types, one for each competitor.
 # numCompetitorsGenCompHeading <- 100 ###
#' @export
genCompetitorHeading <- function(numCompetitorsGenCompHeading){
 
    outCompetitorHeading  <- runif(numCompetitorsGenCompHeading, 0 , 2*pi)
     
    outCompetitorHeading
  }