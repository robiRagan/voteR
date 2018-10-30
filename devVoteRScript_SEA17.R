#####################################################
# devVoteRScript_SEA17.R
# 11.17.17
# SEA17 Demo Sript for: voteR
# 
# This script was created as a demo for the 
# 2017 Southern Economics Association Meeting 
# Tampa, FL 11.17.17 
# It demonstrates the following 
#   - genVoters
#
#####################################################
rm(list = ls(all = TRUE))



##################
# Globals
##################
# numRunsGlobal <- 4
# numDimsGlobal <- 2
# numVotersGlobal <- 100
# numAltsGlobal <- 2

##############################################
## LOAD THE PACKAGES (Install if not loaded):
##############################################
# install.packages("Rcpp")
# install.packages("BH")
# install.packages("ggplot2")
# install.packages("sf")
library(ggplot2)
library(voteR)
library(sf)
  

     ##########################
    #1) Generate Ideal Points
    ###########################
    
        # 1a) [Norm(0, 1), Norm(0,1)]

        idealsNormNorm <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 101, distributionTypeGenVoters = "norm", distributionParametersGenVoters = c(0,1) )
        
        
        plotteRvoteR(votersDataFrame = idealsNormNorm, plotIdeals = TRUE)

        
        # 1b) [Norm(0, .5), Norm(0,.5)]
        
        idealsNormNormLowerStd <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 101, distributionTypeGenVoters = "norm", distributionParametersGenVoters = c(0,.1) )
        
        
        plotteRvoteR(votersDataFrame = idealsNormNormLowerStd, plotIdeals = TRUE)
        
        
        
        # 1c) [Unif(-1,1), Unif(-1,1)]
        
        idealsUnifUnif <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 101, distributionTypeGenVoters = "unif", distributionParametersGenVoters = c(-1,1) )
        
        plotteRvoteR(votersDataFrame = idealsUnifUnif, plotIdeals = TRUE)
        
        
        
        # 1d) [ChiSquared(-1), ChiSquared(1)]
        
        idealsChiSquared <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 101, distributionTypeGenVoters = "chisq", distributionParametersGenVoters = c(1,0) )
        
        plotteRvoteR(votersDataFrame = idealsChiSquared, plotIdeals = TRUE)
        
 
        
        
        
        
        ####################################################
        #2) Utility and Indifference Curves
        #####################################################
        
        ## ALL CIRCLES
        unifCircularEqualSalVoters <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 7, distributionTypeGenVoters = "unif", distributionParametersGenVoters = c(-1,1), salienceHeterogeneityGenVoters = 1, maxRelativeSalienceGenVoters = 1, allEllipticalGenVoters = TRUE, probabilityElipticalGenVoters = 1, probabilityDiamondGenVoters = 0, probabilitySquareGenVoters = 0)
        
        #ALT at (0,0)
        theAlt1 <- setAlts( matrix(c(0,0), nrow = 1,ncol = 2) )
        plotteRvoteR(votersDataFrame = unifCircularEqualSalVoters, altPoints=theAlt1, plotIdeals = TRUE, plotICs = TRUE, showLegend = TRUE)
       
        #ALT at (0,.5)
        theAlt2 <- setAlts( matrix(c(0,0), nrow = 1,ncol = 2) )
        plotteRvoteR(votersDataFrame = unifCircularEqualSalVoters, altPoints=theAlt2, plotIdeals = TRUE, plotICs = TRUE, showLegend = TRUE)
        
        
        ## ALL ELIPTICAL
        unifElipticalDiffSalVoters <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 7, distributionTypeGenVoters = "unif", distributionParametersGenVoters = c(-1,1), salienceHeterogeneityGenVoters = .25, maxRelativeSalienceGenVoters = 4, allEllipticalGenVoters = TRUE, probabilityElipticalGenVoters = 1, probabilityDiamondGenVoters = 0, probabilitySquareGenVoters = 0)
        
        #ALT at (0,0)
        plotteRvoteR(votersDataFrame = unifElipticalDiffSalVoters, altPoints=theAlt1, plotIdeals = TRUE, plotICs = TRUE, showLegend = TRUE)
        
        #ALT at (0,.5)
        plotteRvoteR(votersDataFrame = unifElipticalDiffSalVoters, altPoints=theAlt2, plotIdeals = TRUE, plotICs = TRUE, showLegend = TRUE)
        
         
        
        ## Minkowski Heterogeneity
        unifMinkoDiffSalVoters <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 7, distributionTypeGenVoters = "unif", distributionParametersGenVoters = c(-1,1), salienceHeterogeneityGenVoters = .25, maxRelativeSalienceGenVoters = 2, allEllipticalGenVoters = FALSE, probabilityElipticalGenVoters = .34, probabilityDiamondGenVoters = .33, probabilitySquareGenVoters = .33)
        
        #ALT at (0,0)
        plotteRvoteR(votersDataFrame = unifMinkoDiffSalVoters, altPoints=theAlt1, plotIdeals = TRUE, plotICs = TRUE, showLegend = TRUE)
        
        #ALT at (0,.5)
        plotteRvoteR(votersDataFrame = unifMinkoDiffSalVoters, altPoints=theAlt2, plotIdeals = TRUE, plotICs = TRUE, showLegend = TRUE, plotPareto = TRUE, plotAlts = TRUE)
        
        
        
        ## More of voters Minkowski Heterogeneity
        unifMinkoDiffSalVotersMore <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 15, distributionTypeGenVoters = "unif", distributionParametersGenVoters = c(-1,1), salienceHeterogeneityGenVoters = .25, maxRelativeSalienceGenVoters = 3, allEllipticalGenVoters = FALSE, probabilityElipticalGenVoters = .34, probabilityDiamondGenVoters = .33, probabilitySquareGenVoters = .33)
        
        #ALT at (0,0)
        plotteRvoteR(votersDataFrame = unifMinkoDiffSalVotersMore, altPoints=theAlt1, plotIdeals = TRUE, plotICs = TRUE, showLegend = FALSE)
        
        #ALT at (0,.9)
        plotteRvoteR(votersDataFrame = unifMinkoDiffSalVotersMore, altPoints=theAlt2, plotIdeals = TRUE, plotICs = TRUE, showLegend = FALSE)
        
        
        

#####################################
# PARETO SET
#####################################

# Plot the voters and Pareto Set

plotParetoSet(unifMinkoDiffSalVotersMore, idealsOn  = TRUE)

        
       
#####################################
# WIN SET
#####################################
        
# Plot the voters and Win Set

        
        
  