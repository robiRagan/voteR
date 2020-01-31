#' calcMinkowskiUtillityVotersAlts
#' Calulate utility using minkowskiDistanceSets() which is an Rcpp generated function. 
#' 
#' Takes a voters dataframe and an alternatives dataframe and strips out the relevant parts and feeds them to minkowskiDistanceSets() which is an Rcpp generated function. 
#' @param votersCalcMinkowskiDistanceVotersAlts The voters data frame must have a specific format, and it must be an R data.frame object. There must be at least these 6 variables and they must have the following names. The order of the variables in the data.frame is not important as long as the variables have the proper names.
#' 
#'  ID: A numeric identifier unique to the voter.
#'  xLocation: The x coordinate of the voter's ideal point.
#'  yLocation: The y coordinate of the voter's ideal point.
#'  minkoOrder: The Minkowski order of the voters MInkowski metric based utility function. = 1, is City Block. = 2 is Euclidian and 100 = is  See ?Minkowski. 
#'  xSalience: The salience of the x dimension for the voter. The dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#'  ySalience: The salience of the y dimension for the voter. he dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#'  lossOrder: The loss order for the agents utility function. See the parameter lossOrderVector in ?minkowskiUtilitySets().
#'  
#' @param alternativesCalcMinkowskiDistanceVotersAlts alternatives data frame generated from genAlternatives() or in the same format.
#' @return A numVoters by numAlternitives matrix containing the utility each voter has for each alternitive.  
#' @export
calcMinkowskiUtilityVotersAlts <- function(votersCalcMinkowskiDistanceVotersAlts, alternativesCalcMinkowskiDistanceVotersAlts){
    
    # ## FOR TESTING ###
    # votersCalcMinkowskiDistanceVotersAlts <- data.frame(pointType = rep(x = "voter", 3), ID = c("V-1", "V-2", "V-3"), xLocation=c(-1/8, 7/8, 4/8), yLocation=c(3/8, 4/8, -3/8), minkoOrder=c(1, 2, 100), xSalience = c(1, 1, 1), ySalience = c(1, 1, 1), lossOrder = c(2, 2, 2) )
    # 
    # alternativesCalcMinkowskiDistanceVotersAlts <- data.frame(pointType = rep(x = "alternative", 3), ID = c("A-1", "A-2", "A-3"), xLocation=c(-3/8, 1/8, 2/8), yLocation=c(-3/8, 1/8, 7/8) )
    # ## FOR TESTING ##

    
    minkoUtilOut <-  minkowskiUtilitySets( idealsMatrix = as.matrix (dplyr::select(votersCalcMinkowskiDistanceVotersAlts, xLocation, yLocation) ), 
                                            altsMatrix = as.matrix( dplyr::select(alternativesCalcMinkowskiDistanceVotersAlts, xLocation, yLocation) ), 
                                            minkoOrderVector = as.matrix( dplyr::select(votersCalcMinkowskiDistanceVotersAlts, minkoOrder) ), 
                                            lossOrderVector = as.matrix( dplyr::select(votersCalcMinkowskiDistanceVotersAlts, lossOrder) ), 
                                            salienceMatrix = as.matrix( dplyr::select(votersCalcMinkowskiDistanceVotersAlts, xSalience, ySalience) ) 
                                            )
    
    
    rownames(minkoUtilOut) <- votersCalcMinkowskiDistanceVotersAlts$ID
     
    colnames(minkoUtilOut) <- as.vector(alternativesCalcMinkowskiDistanceVotersAlts$ID)

    minkoUtilOut
    }   
