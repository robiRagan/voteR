#' voronoiTessellation
#' Creates a voroni tessilation of the space.
#' 
#' Used mostly for testign purposes. This fucntion allows you to quickly check the cutlines
#' for 2 alterantive cases and the analogous boundaries for more than 2 alterantives. Only works for 
#' casses with purely 'circular' euclidian preferences. 
#' @param xLocationsVoronoiTessellation The x coordinates for the generator points. Usually this will be the alternatives/competitors in the choice set. 
#' @param yLocationsVoronoiTessellation The y coordinates for the generator points. Usually this will be the aternatives/competitors in the choice set.
#' @return outAlternativeDataFrame 
#' @export
voronoiTessellation <- function(xLocationsVoronoiTessellation, yLocationsVoronoiTessellation){
    ## TEST ##
    xLocationsVoronoiTessellation <- competitorPoints$xLocation
    yLocationsVoronoiTessellation <- competitorPoints$yLocation
    ## TEST ##
    
    dataFrameForVoronoi <- data.frame(xLocationsVoronoiTessellation, yLocationsVoronoiTessellation)
    
    voronoi <- deldir(xLocationsVoronoiTessellation, yLocationsVoronoiTessellation)
    
    #Now we can make a plot
    ggplot(data=dataFrameForVoronoi, aes(x=xLocationsVoronoiTessellation, y=yLocationsVoronoiTessellation) ) +
        #Plot the voronoi lines
        geom_segment(
            aes(x = x1, y = y1, xend = x2, yend = y2),
            size = 2,
            data = voronoi$dirsgs,
            linetype = 1,
            color= "#FFB958") + 
        #Plot the points
        geom_point(
            fill=rgb(70,130,180,255,maxColorValue=255),
            pch=21,
            size = 4,
            color="#333333")

    }   
