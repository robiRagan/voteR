#' plotteRvoteR
#' Main plotting command for the voteR package. 
#' 
#' @param votersDataFrame The voters data frame must have a specific format, and it must be an R data.frame object. There must be at least these 6 variables and they must have the following names. The order of the variables in the data.frame is not important as long as the variables have the proper names.
#' 
#'  ID: A numeric identifier unique to the voter.
#'  xIdeal: The x coordinate of the voter's ideal point.
#'  yIdeal: The y coordinate of the voter's ideal point.
#'  minkoOrder: The Minkowski order of the voters MInkowski metric based utility function. = 1, is City Block. = 2 is Euclidian and 100 = is  See ?Minkowski. 
#'  xSalience: The salience of the x dimension for the voter. The dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#'  ySalience: The salience of the y dimension for the voter. he dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
#'  
#' @param altPoints A numberOfAlts x numberOfDimensions matrix. This is the matrix that contains the alternatives that the voter's are evaluating. Default: altPoints = NA.
#' 
#' @param competitorPoints A numberOfCompetitors x numberOfDimensions matrix. This is the matrix that contains the competitor locations that the voter's are evaluating.
#' 
#' @param plotIdeals If set to TRUE the ideal points of the voters are plotted. Default: plotIdeals = FALSE
#' 
#' @param plotPareto If set to TRUE the Pareto set is plotted. Default: plotPareto = FALSE
#' 
#' @param plotICs If set to TRUE the voters' indifference curves are plotted. Default: plotICs = FALSE
#' 
#' @param locationForICs The location in the space that the indifference curves are drawn through. This is usually the status quo. plotteRvoteR() assumes that the first row in the altPoints or competitorPoints dataFrame is the point to draw ICs through if points are not supplied via this parameter. You can supply one of the other rows of altPoints or competitorPoints by using something like: locationForICs = c(altPoints$xLocation[2], altPoints$yLocation[2]). Default is locationForICs = NA.
#' 
#' @param plotAlts If set to TRUE the alternatives in the choice set are plotted. Default: plotAlts = FALSE
#' 
#' @param plotCompetitors If set to TRUE the competitors in the choice set are plotted. Default: plotCompetitors = FALSE
#' 
#' @param plotVoronoi If set to TRUE the alternatives/competitors are used as the generatign points for a Voronoi Tessellation. Default: plotVoronoi = FALSE
#' 
#' @param plotMarginalMedian If set to TRUE the marginal median (the vector of medians for each dimension) is plotted.
#' 
#' @param percision How precisely you want to trace out the voters' indifference curves. The lower the number the more accurate and smooth the indifference curve will be, but for large numbers of voters larger numbers will slow down plotting. Default: precision = .01
#' 
#' @param yToXRatio Fixes the ratio of the two axis for plotting purposes. The number of units on the policy space represented on the y-axis that is equivalent to one unit on policy space represented on the the x-axis. Default: yToXRatio = 1. For more see ?coord_fixed().
#'
#' @param showLegend Should the legend for the plot be shown. If TRUE then ggplot() uses defaults to plot Default: showLegend = FALSE
#'
#' @param showLegend Show the legend by one of the following: "pointTypes" Default: "pointTypes"
#'
#' @param xBounds The bounds for the x dimension provided as a vector. Ex: c(-1,1). If they are not provided then ggplot() will use its defaults. Default = FALSE
#'
#' @param yBounds The bounds for the x dimension. Ex: c(-1,1). If they are not provided then ggplot() will use its defaults. Default = FALSE
#'
#'
#' @return Plots the requested attributes of a spatial voting model. 
#' 
#' @export
# TODO I will need to write a simple function for plotting things that takes care of most cases and plots well. And a version where the user can control all of the detials like opacity, colors, layering etc.
plotteRvoteR <- function( votersDataFrame=NA,
                          altPoints=as.matrix(NA),
                          competitorPoints = as.matrix(NA),
                          plotIdeals = FALSE,
                          plotPareto = FALSE,
                          plotICs = FALSE,
                          locationForICs=NA,
                          plotAlts = FALSE,
                          plotCompetitors = FALSE,
                          plotVoronoi = FALSE,
                          plotMarginalMedian = FALSE,
                          precision=.01,
                          yToXRatio = 1,
                          showLegend = TRUE,
                          showLegendBy = "pointTypes",
                          xBounds = FALSE,
                          yBounds = FALSE){
    

  
#   ## TEST ##
#   rm(list = ls(all = TRUE))
#     votersDataFrame<-data.frame(pointType= rep("voter", 5), ID = c("V-1", "V-2", "V-3", "V-4", "V-5"), xLocation=c(15, 20, 35, 40, 55), yLocation=c(45, 15, 50, 35, 30), minkoOrder=c(2, 2, 2, 2, 2), xSalience = c(1, 1, 1, 1, 1), ySalience = c(1, 1, 1, 1, 1) )
#     altPoints = c(30,40)
#     competitorPoints <- as.matrix(NA)
#     plotIdeals <- TRUE
#     plotPareto <- TRUE
#     plotICs <- TRUE
#     locationForICs=NA
#     plotAlts <- TRUE
#     plotCompetitors <- FALSE
#     plotVoronoi <- FALSE
#     plotMarginalMedian <- TRUE
#     precision<-.01
#     yToXRatio <- 1
#     showLegend <- TRUE
#     xBounds <- c(0,100)
#     yBounds <- c(0,100)
# ## TEST ##

    
  # ## TEST Using devVoteRScript_SEA18.R ###
  # votersDataFrame <- voterIdeals
  # altPoints=as.matrix(NA)
  # competitorPoints <- currentCompetitorPositions 
  # plotIdeals <- TRUE
  # plotPareto <- FALSE
  # plotICs <- FALSE
  # locationForICs <- NA
  # plotAlts <- FALSE
  # plotCompetitors <- TRUE 
  # plotVoronoi <- TRUE 
  # plotMarginalMedian <- TRUE
  # precision <- .01 
  # yToXRatio <-  1 
  # showLegend <- TRUE 
  # showLegendBy <- "pointTypes"
  # xBounds <- dimOneBounds 
  # yBounds <- dimTwoBounds 
  # ## TEST Using devVoteRScript_SEA18.R ###

  # # ## TEST  ###
  # rm(list = ls(all = TRUE))
  # votersDataFrame = data.frame( pointType= rep("voter", 5), 
  #                               ID = c("V-1", "V-2", "V-3", "V-4", "V-5"), 
  #                               xLocation=c(15, 20, 35, 40, 55), 
  #                               yLocation=c(45, 15, 50, 35, 30), 
  #                               minkoOrder=c(2, 2, 2, 2, 2), 
  #                               xSalience = c(1, 1, 1, 1, 1), 
  #                               ySalience = c(1, 1, 1, 1, 1) ) 
  # altPoints = c(30,40)
  # plotIdeals = TRUE 
  # plotICs = TRUE 
  # plotPareto = TRUE 
  # plotAlts = TRUE 
  # yToXRatio = 1
  # 
  # competitorPoints = as.matrix(NA)
  # locationForICs=NA
  # plotCompetitors = FALSE
  # plotVoronoi = FALSE
  # plotMarginalMedian = FALSE
  # precision=.01
  # showLegend = TRUE
  # showLegendBy = "pointTypes"
  # xBounds = FALSE
  # yBounds = FALSE
  # # ## TEST ###
  
  
    
    # If altPoints is supplied to the function, we must convert it to a dataframe before calling the plot functions.
    
    if(is.na(altPoints[1]) == TRUE){
      altPointsDF <- data.frame(NA)
    } else if(is.vector(altPoints) == TRUE) {
      altPointsDF <- data.frame(xLocation=altPoints[1], yLocation=altPoints[2])
    }else if(is.matrix(altPoints) == TRUE) {
      altPointsDF <- data.frame(xLocation=altPoints[ ,1], yLocation=altPoints[ ,2])
    }
    if(is.na(altPointsDF[1])==FALSE){
      altPointsDF$alternativeID <- paste( "A",seq(from = 1,to = nrow(altPointsDF)), sep="-" )
    }
    
    # If competitorPoints is supplied to the function, we must convert it to a dataframe before calling the plot functions.
    ## THIS CHANGE HAS NOT BEEN CHECKED YET 01.30.2020 ###
    if(is.na(competitorPoints[1]) == TRUE){
      competitorPointsDF <- data.frame(NA)
    } else if(is.vector(competitorPoints) == TRUE) {
      competitorPointsDF <- data.frame(xLocation=competitorPoints[1], yLocation=competitorPoints[2])
    }else if(is.matrix(competitorPoints) == TRUE) {
      competitorPointsDF <- data.frame(xLocation=competitorPoints[ ,1], yLocation=competitorPoints[ ,2])
    }
    
    
    
    
    # First a set of checks is run to ensure that the information that is needed to plot the components of the graph via the plotXXXX = TRUE parameters have been supplied to the plotteRvoteR().
    
    if(is.na(altPointsDF[1]) & plotAlts == TRUE){ stop('There was a request to plot the alternatives via (plotAlts=TRUE), However, no altPoints were supplied to plotteRvoteR(). See ?plotteRvoteR for help.')}
    if(is.na(competitorPointsDF[1]) & plotCompetitors == TRUE){ stop('There was a request to plot the competitors via (plotCompetitors=TRUE), However, no competitorPoints were supplied to plotteRvoteR(). See ?plotteRvoteR for help.')}
    if(any(is.na(votersDataFrame[1])) & plotIdeals == TRUE){ stop('There was a request to plot the ideal points via plotIdeals=TRUE. However, no votersDataFrame was supplied to plotteRvoteR(). See ?plotteRvoteR for help.')}
    if(any(is.na(votersDataFrame[1])) & plotPareto == TRUE){ stop('There was a request to plot the Pareto set via plotPareto=TRUE. However, no votersDataFrame was supplied to plotteRvoteR(). See ?plotteRvoteR for help.')}
    if(any(is.na(votersDataFrame[1])) & plotICs == TRUE){ stop('There was a request to plot the indifference curves via plotICs=TRUE. However, no votersDataFrame was supplied to plotteRvoteR(). See ?plotteRvoteR for help.')}
    if(any(is.na(altPoints[1])) & any(is.na(competitorPoints[1])) & plotICs == TRUE){ stop('There was a request to plot the indifference curves via plotICs=TRUE. However, no alternatives (altPoints) or competitors (competitorPoints) were supplied to plotteRvoteR(). See ?plotteRvoteR for help.')}
    if(any(is.na(altPointsDF[1])) & any(is.na(competitorPointsDF[1])) & plotVoronoi == TRUE){ stop('There was a request to plot the cutlines/Voronoi Tessellation via plotVoronoi=TRUE. However, no alternatives (altPoints) or competitors (competitorPoints) were supplied to plotteRvoteR(). See ?plotteRvoteR for help.')}
    if( nrow(altPointsDF) <= 2  & nrow(competitorPointsDF)<=2  & plotVoronoi == TRUE){ stop('There was a request to plot the cutlines/Voronoi Tessellation via plotVoronoi=TRUE. However, only one alternative (altPoints) or competitor (competitorPoints) was supplied to plotteRvoteR(). See ?plotteRvoteR for help.')}

    

    # Next a set of checks is run to ensure that the alts, competitor points and/or ideals that the user has requested to plot are all within the boundary parameters have been supplied to the plotteRvoteR().
    
    # If voters were supplied find the max and min on each dimension
    if(any(is.na(votersDataFrame[1]))==FALSE){
    
      minXIdeals <- min(votersDataFrame$xLocation)
      maxXIdeals <- max(votersDataFrame$xLocation)
      minYIdeals <- min(votersDataFrame$yLocation)
      maxYIdeals <- max(votersDataFrame$yLocation)
    
      if (xBounds != FALSE){
      # check the bounds for the ideals
        
        if(xBounds[1] > minXIdeals & plotIdeals == TRUE){ stop('There was a request to plot the ideal points via (plotIdeals=TRUE), However, the the X-coordinate of one of the ideal points is below the minimum X in the xBounds. See ?plotteRvoteR for help.')}
        if(xBounds[2] < maxXIdeals  & plotIdeals == TRUE){ stop('There was a request to plot the ideal points via (plotIdeals=TRUE), However, the the X-coordinate of one of the ideal points is above the maximum X in the xBounds. See ?plotteRvoteR for help.')}
        }
        
      if (yBounds != FALSE){
        if(yBounds[1] > minYIdeals & plotIdeals == TRUE){ stop('There was a request to plot the ideal points via (plotIdeals=TRUE), However, the the Y-coordinate of one of the ideal points is below the minimum Y in the yBounds. See ?plotteRvoteR for help.')}
        if(yBounds[2] < maxYIdeals  & plotIdeals == TRUE){ stop('There was a request to plot the ideal points via (plotIdeals=TRUE), However, the the Y-coordinate of one of the ideal points is above the maximum Y in the yBounds. See ?plotteRvoteR for help.')}
        }
        
      }
    
    
    
    # If alts were supplied find the max and min on each dimension
    if(is.na(altPointsDF[1])==FALSE){
    
      minXAlts <- min(altPointsDF$xLocation)
      maxXAlts <- max(altPointsDF$xLocation)
      minYAlts <- min(altPointsDF$yLocation)
      maxYAlts <- max(altPointsDF$yLocation)
    
      if (xBounds != FALSE){
      # Check the bounds for the alts
        if(xBounds[1] > minXAlts & plotAlts == TRUE){ stop('There was a request to plot the alternatives via (plotAlts=TRUE), However, the the X-coordinate of one of the alternatives is below the minimum X in the xBounds. See ?plotteRvoteR for help.')}
        if(xBounds[2] < maxXAlts  & plotAlts == TRUE){ stop('There was a request to plot the alternatives via (plotAlts=TRUE), However, the the X-coordinate of one of the alternatives is above the maximum X in the xBounds. See ?plotteRvoteR for help.')}
      }
        
      if (yBounds != FALSE){
        if(yBounds[1] > minYAlts & plotAlts == TRUE){ stop('There was a request to plot the alternatives via (plotAlts=TRUE), However, the the Y-coordinate of one of the alternatives is below the minimum Y in the yBounds. See ?plotteRvoteR for help.')}
        if(yBounds[2] < maxYAlts  & plotAlts == TRUE){ stop('There was a request to plot the alternatives via (plotAlts=TRUE), However, the the Y-coordinate of one of the alternatives is above the maximum Y in the yBounds. See ?plotteRvoteR for help.')}
      }
    }
    
    
    
    # If competitors were supplied find the max and min on each dimension
    if(is.na(competitorPointsDF[1])==FALSE){
    
      minXCompetitor <- min(competitorPointsDF$xLocation)
      maxXCompetitor <- max(competitorPointsDF$xLocation)
      minYCompetitor <- min(competitorPointsDF$yLocation)
      maxYCompetitor <- max(competitorPointsDF$yLocation)
    
      
      if (xBounds != FALSE){
      # Check the bounds for the competitors
        if(xBounds[1] > minXCompetitor & plotAlts == TRUE){ stop('There was a request to plot the competitors via (plotAlts=TRUE), However, the the X-coordinate of one of the competitors is below the minimum X in the xBounds. See ?plotteRvoteR for help.')}
        if(xBounds[2] < maxXCompetitor  & plotAlts == TRUE){ stop('There was a request to plot the competitors via (plotAlts=TRUE), However, the the X-coordinate of one of the competitors is above the maximum X in the xBounds. See ?plotteRvoteR for help.')}
      }
        
      if (yBounds != FALSE){  
        if(yBounds[1] > minYCompetitor & plotAlts == TRUE){ stop('There was a request to plot the competitors via (plotAlts=TRUE), However, the the Y-coordinate of one of the competitors is below the minimum Y in the yBounds. See ?plotteRvoteR for help.')}
        if(yBounds[2] < maxYCompetitor  & plotAlts == TRUE){ stop('There was a request to plot the competitors via (plotAlts=TRUE), However, the the Y-coordinate of one of the competitors is above the maximum Y in the yBounds. See ?plotteRvoteR for help.')}
      }
    }
    

    


    
    

    
    
    
    ## The code that actually generates the plots is here.     
    voteplot <- ggplot2::ggplot() +  ggplot2::coord_fixed()
    
    
    
            ################################################
            # SHADED AREAS
            ###############################################
    
    
    ## Plot the ICs first so points will appear on top  of them ##
    if ( plotICs == TRUE){
        
        if(is.na(locationForICs[1]) & (is.na(competitorPoints[1,1])==FALSE)){locationForICs <- c(competitorPoints$xLocation[1], competitorPoints$yLocation[1])}
        
        if(is.na(locationForICs[1]) & (is.na(altPointsDF[1,1])==FALSE)){locationForICs <- c(altPointsDF$xLocation[1], altPointsDF$yLocation[1])}
        
        # creates a blank list to place the Preferred to sets of the voters in
        allVotersICPointsList <- list()
        
        for (j in 1:nrow(votersDataFrame)){
            allVotersICPointsList[[j]] <- findICPoints(voterID = votersDataFrame$ID[j], idealPoint = c(votersDataFrame$xLocation[j], votersDataFrame$yLocation[j]), orderScalar = votersDataFrame$minkoOrder[j], salienceVector = c(votersDataFrame$xSalience[j], votersDataFrame$ySalience[j]), altPointVector = locationForICs, precision = .01)
        }
        
        # Collapse the list to a data frame. 
        allVotersPrefToSetsDF <- do.call(rbind.data.frame, allVotersICPointsList)
        
        # Name the dataframe variables. I swear to god this "colnames() <-"  thing is witchcraft. 
        colnames(allVotersPrefToSetsDF) <- c('ID', 'xCoords', 'yCoords')
        
        allVotersPrefToSetsDF$ID <- as.factor(allVotersPrefToSetsDF$ID)
        
        voteplot <- voteplot + ggplot2::geom_polygon(data = allVotersPrefToSetsDF, mapping = ggplot2::aes(color = ID, fill = ID, x = xCoords, y = yCoords), alpha = 1/(2*nrow(competitorPoints)))
        
    }
    
    ### Pareto Set is another filled region and need to plot below the points ###
    
    if ( plotPareto == TRUE){
        
        justIdealPoints = cbind(votersDataFrame$xLocation, votersDataFrame$yLocation)
        
        paretoSetDF <- findParetoSet(justIdealPoints)
        
        voteplot <- voteplot + ggplot2::geom_polygon(data = paretoSetDF, mapping = ggplot2::aes(x = V1, y = V2, alpha=1), fill="grey", color="grey50") + ggplot2::scale_alpha(labels = "")
    }
    
    ################################################
    # LINES
    ###############################################
    
    if ( plotVoronoi == TRUE){
        
        if((is.na(competitorPoints[1,1])==FALSE)){dataFrameForVoronoi <- competitorPoints}
        
        if((is.na(altPointsDF[1,1])==FALSE)){dataFrameForVoronoi <- altPointsDF}
        
            if(as.character(xBounds[1])==FALSE | as.character(yBounds[1])==FALSE){
                calcVoronoi <- suppressMessages( deldir::deldir(dataFrameForVoronoi$xLocation, dataFrameForVoronoi$yLocation) )
            } else {
                calcVoronoi <- suppressMessages( deldir::deldir(dataFrameForVoronoi$xLocation, dataFrameForVoronoi$yLocation, rw=c(xBounds[1], xBounds[2], yBounds[1], yBounds[2])) )
            }
        #Now we can make a plot
        voteplot <- voteplot + ggplot2::geom_segment(data = calcVoronoi$dirsgs, mapping = ggplot2::aes( x = x1, y = y1, xend = x2, yend = y2, linetype=""), color="gray", size=1)
        
    }
    
    
        ################################################
        # POINTS
        ###############################################
    
    
    ## Now plot the ideal points for the voters ##
    
     if ( plotIdeals == TRUE ){
         
         votersDataFrame$ID <- as.factor(votersDataFrame$ID)
         
         voteplot <- voteplot + ggplot2::geom_point(data = votersDataFrame, mapping = ggplot2::aes(shape=pointType, color = ID, x = xLocation, y = yLocation), size = 3) # + scale_shape(labels = "A Voter")
     }
     
    
    if ( plotMarginalMedian == TRUE){
        
        marginalMedian <- data.frame( xMedian=median(votersDataFrame$xLocation), yMedian=median(votersDataFrame$yLocation), pointType="marginal median" )
        
        voteplot <- voteplot + ggplot2::geom_point(data = marginalMedian, mapping = ggplot2::aes(shape=pointType, x = xMedian, y = yMedian), size = 3) # + scale_shape(labels = "A Voter")
    }
    
    
    ## Now plot the alternatives ##
    
    if ( plotAlts == TRUE){
      altPointsDF$pointType <- "alternative"
      
      voteplot <- voteplot + ggplot2::geom_point(data = altPointsDF, mapping = ggplot2::aes(shape=pointType, x = xLocation, y = yLocation), size = 4) + ggplot2::geom_text(data = altPointsDF, mapping = ggplot2::aes(x = xLocation, y = yLocation, label=alternativeID), hjust=0, vjust=1.75)# + scale_shape(labels = "An Alternative")
    }   
    
    
    ## Now plot the competitors ##
    
    if ( plotCompetitors == TRUE){
        voteplot <- voteplot + ggplot2::geom_point(data = competitorPoints, mapping = ggplot2::aes(shape=pointType, x = xLocation, y = yLocation), size = 4) + geom_text(data = competitorPoints, mapping = aes(x = xLocation, y = yLocation, label=ID), hjust=0, vjust=1.75)# + scale_shape(labels = "A Competitor")
    }
    
    
    ## Adjust the labels on axis and legend#
    voteplot <- voteplot + ggplot2::labs(x = "Issue Dimension 1", y = "Issue Dimension 2", alpha = "Pareto Set", shape = "Point Type", linetype="Voronoi Cutlines", color="Voter ID", fill="Voter ID")
    
    ## Fix the shapes so everything in the plot and legend line up ##
    ## This will have to be generalized for the fact that not every plot will have the same number of different points ##
    voteplot <- voteplot  + ggplot2::scale_shape_manual( values = c(18, 8, 16) )
                                 
    
    ## Make the plot fit the user provided yToXRatio or make square:
    voteplot <- suppressMessages(voteplot + ggplot2::coord_equal(ratio = yToXRatio))
    
    
    ###################################
    # Set the bounds of the plot.
    # Note this uses coord_cartesian() which keeps all underlying data in the plot.
    # Think of it as zooming in on the plot based on the bounds
    # ## THIS NEEDS TO BE GENERALIZED SO ICs do not go off the graph ###
    ###################################
    
  
    if (as.character(xBounds[1]) != FALSE & as.character(yBounds[1]) == FALSE){
        voteplot <- suppressMessages( voteplot + ggplot2::coord_cartesian(xlim = c(xBounds[1], xBounds[2])) )
    }  
    
    if (as.character(yBounds[1]) != FALSE & as.character(xBounds[1]) == FALSE){
        voteplot <- suppressMessages( voteplot + ggplot2::coord_cartesian(ylim = c(yBounds[1], yBounds[2])) )
    }  
    
    if (as.character(xBounds[1]) != FALSE & as.character(yBounds[1]) != FALSE){
        voteplot <- suppressMessages( voteplot + ggplot2::coord_cartesian(xlim = c(xBounds[1], xBounds[2]), ylim = c(yBounds[1], yBounds[2])) )
    }  
    
    # if (showLegend == TRUE){
    # 
    #     if (showLegendBy == "pointTypes"){
    #         voteplot <- voteplot + geom_polygon(group=pointType)
    #     }
    # }

    
    
    if (showLegend == FALSE){
        voteplot <- voteplot + ggplot2::theme(legend.position="none")
    
        }
    

    
    voteplot
} 
