#' hunterNewHeading.R
#' For the Hunter type competitor. Takes a heading in radians and randomly draws a new heading between pi/2 to 3pi/2 (90 to 270 degrees).
#' 
#' @param origHeadingInRadians The inital point in 2D space. 
#' @return outNewHeading 
#' @export
hunterNewHeading <- function(origHeadingInRadians){
    
    # # ## TEST ###
    #  origHeadingInRadians <- pi
    # # ## TEST ###
    
    outNewHeading <- (origHeadingInRadians + runif(1, pi*1/2, pi*3/2) )%%(2*pi)
    
        outNewHeading
        
    }   
