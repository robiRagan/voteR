#' moveDistDAtAngleT
#' Takes a two dimensional coordinate and moves it to another coordinate based on a distance and angle. 
#' 
#' @param origPointMoveDistDAtAngleT The inital point in 2D space. 
#' @param distanceDMoveDistDAtAngleT The distance you want to move from the original point. 
#' @param angleTMoveDistDAtAngleT The angle in radians you want to take when moving away from the original point.
#' @param dimOneBoundsMoveDistDAtAngleT Boundry points of the x dimension.
#' @param dimTwoBoundsMoveDistDAtAngleT Boundry poitns of the y dimension. 
#' @return outNewPoint 
#' @export
moveDistDAtAngleT <- function(origPointMoveDistDAtAngleT, distanceDMoveDistDAtAngleT, angleTMoveDistDAtAngleT, dimOneBoundsMoveDistDAtAngleT, dimTwoBoundsMoveDistDAtAngleT){
    
    # ## TEST ###
    # compNum <- 3
    # origPointMoveDistDAtAngleT <- c(competitorDataFrameHunterAdapts$xLocation[compNum], competitorDataFrameHunterAdapts$yLocation[compNum])
    # distanceDMoveDistDAtAngleT <- scaledCompetitorAdaptStepSizeHunterAdapts
    # angleTMoveDistDAtAngleT <- competitorDataFrameHunterAdapts$headingRadians[compNum]
    # dimOneBoundsMoveDistDAtAngleT <- c(0,1)
    # dimTwoBoundsMoveDistDAtAngleT <- c(0,1)
    # ## TEST ###
    
    newXCoord = origPointMoveDistDAtAngleT[1] + round( distanceDMoveDistDAtAngleT * cos(angleTMoveDistDAtAngleT), digits =  8)
    newYCoord = origPointMoveDistDAtAngleT[2] + round( distanceDMoveDistDAtAngleT * sin(angleTMoveDistDAtAngleT), digits =  8)
    
    tempNewPoint <- c(newXCoord, newYCoord)

    ## Check Bounds ## 
    if(tempNewPoint[1] >= dimOneBoundsMoveDistDAtAngleT[1] & 
       tempNewPoint[1] <= dimOneBoundsMoveDistDAtAngleT[2] & 
       tempNewPoint[2] >= dimTwoBoundsMoveDistDAtAngleT[1] & 
       tempNewPoint[2] <= dimTwoBoundsMoveDistDAtAngleT[2]){
        outNewPoint <- tempNewPoint
    } else {
        outNewPoint <- origPointMoveDistDAtAngleT
        }
    
        outNewPoint
        
    }   
