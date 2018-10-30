#####################################################
# Laver and Fowler's tourney.R
#  
#
# http://fowler.ucsd.edu/tournament_appendix.pdf
#
#
# The Tournament of Party Strategies
# version 1.0
# james fowler
# uc davis
# 1/11/2006
#
# vn = number of voters
# sims = number of simulations
# efreq = frequency of elections
# pthresh = party minimum threshold
# pthreshperiods = number of periods a party must be below threshold to die
# starttype = starting party type -- if 0, random, otherwise the given type
# pstepsize = step size for party strategies
# plot = T/F variable to turn on/off plot of party strategies
# writeall = T/F variable to record T = all poll and election data or F = just election data
# file = name of file to write results to -- two files are recorded one with the name given and one with
#        an "a" as a prefix which records aggregate information
# 
# party types
# 1 = sticker     black
# 2 = aggregator  red
# 3 = hunter      green
# 4 = predator    blue
#

party.tournament <- function( vn=1000, sims=1000, efreq=20,
                              pthresh=0.1, pthreshperiods=2, starttype=0,
                              pstepsize=0.05 , plot=T, writeall=F, file=NULL) {
    
    # initialize voter ideal points, poll/election indicator
    videal<-rbind(rnorm(vn),rnorm(vn))
    pestr<-c("poll","election")
    epsilon<-1/10^10
    if(plot) par(mfrow=c(1,3))
    
    # initialize party location, name, and type vectors
    ototals<-angle<-oploc<-pbt<-ploc<-pmloc<-pname<-ptype<-NULL
    
    # initialize proportional vote counts
    pcount<-mcount<-c(0.25,0.25,0.25,0.25)
    
    # intialize datafiles if there is one
    if(length(file)>0) write.table(t(c("Election","PartyID","Type","VoteRecd",
                                       "X","Y","Angle")), file=file, 
                                   row.names=F,col.names=F,quote=F,sep=",")
    
    for(period in 1:sims) {
        
        #
        # party birth
        #
        
        if(period%%efreq==1) {
            
            # party name, failure counter, previous total counter, and angle
            pname<-c(pname,1+(period-1)/efreq)
            pbt<-c(pbt,0)
            ototals<-c(ototals,0)
            angle<-c(angle,runif(1,0,2*pi))
            
            # draw party type 1=sticker, 2=aggregator, 3=hunter, 4=predator
            if(period==1&starttype!=0) {
                ptype<-starttype
            } else {
                ptype<-c(ptype,sample(1:4,1))
            }
            
            # count parties
            pn<-length(ptype)
            
            # party initial position
            ploc<-cbind(ploc,rbind(rnorm(1),rnorm(1)))
            pmloc<-cbind(pmloc,ploc[,pn])
            oploc<-cbind(oploc,ploc[,pn])
            
        }
        
        #
        # take poll / hold election
        #
        
        # find distances between parties and voters / respondents
        dist<-matrix(0,nrow=pn,ncol=vn)
        for(i in 1:pn) {
            
            dist[i,]<-colSums((videal-ploc[,i])^2)+ # note sqrt not taken to save operation
                runif(vn,-epsilon,epsilon)    # and epsilon added to break ties
        }
        
        # voters choose closest party (epsilon added to break ties)
        vchoice<-apply(dist,2,which.min)
        
        # election / poll results
        ptotals<-hist(vchoice,breaks=0:pn,plot=F)$counts
        
        if(period%%efreq==0) {
            
            # record counts and then randomly 
            # choose party type for next birth
            for(i in 1:4) {
                div<-sum(ptype==i)
                pcount[i]<-div
                if(div==0) div<-1
                mcount[i]<-sum(ptotals[ptype==i])/vn/div
            }
        }
        
        #
        # display information
        #
        
        # print party info
        if(writeall||period%%efreq==0) {
            if(length(file)==0) {
                write.table(paste("Period",period,pestr[(period%%efreq==0)+1]),
                            row.names=F,col.names=F,quote=F)
                write.table(format(rbind(c("Election","Party ID","Type","Vote Recd",
                                           "X","Y","Angle"),
                                         cbind(rep(period/efreq,pn),pname,ptype,round(ptotals/vn,3),
                                               round(t(ploc),3),round(angle,3)))),
                            row.names=F,col.names=F,quote=F)
            } else {
                write.table(cbind(rep(period/efreq,pn), pname, ptype, round(ptotals/vn,3),
                                  round(t(ploc),3), round(angle,3)), file=file, row.names=F, col.names=F, 
                            append=T, sep=",")
                write.table(t(c(mcount,pcount)), file=paste("a",file,sep=""),
                            row.names=F, col.names=F, append=T, sep=",")
            }
        }
        
        # plot parties
        if(plot) {
            plot(0,0,type="n",xlim=c(-3,3),ylim=c(-3,3),xlab="",ylab="",main="Location")
            text(t(ploc),labels=as.character(pname),col=ptype)
            barplot(mcount,names.arg=c("sticker","aggregator","hunter","predator"),col=1:4,
                    main="Votes per Party")
            barplot(mcount*pcount,names.arg=c("sticker","aggregator","hunter","predator"),
                    col=1:4,main="Total Votes")
        }  
        
        #
        # calculate available information
        #
        
        # mean party members' location
        for(i in 1:pn) {
            pslist<-which(vchoice==i)
            if(length(pslist)==0) pmloc[,i]<-ploc[,i]
            if(length(pslist)==1) pmloc[,i]<-videal[,pslist]
            if(length(pslist)>1) pmloc[,i]<-rowMeans(videal[,pslist])
        }
        
        
        #
        # party death
        #
        
        if(period%%efreq==0) { 
            
            # which parties fell below threshold?
            pweak<-as.numeric(ptotals<pthresh*vn)
            
            # update number of times party falls below threshold
            pbt<-(pbt+pweak)*pweak
            
            
            # kill parties that have been below threshold for too long
            pdead<-which(pbt==pthreshperiods)
            if(length(pdead)>0) {
                pn<-pn-length(pdead)
                pname<-pname[-pdead]
                ptype<-ptype[-pdead]
                ploc<-ploc[,-pdead]
                pmloc<-pmloc[,-pdead]
                oploc<-oploc[,-pdead]
                angle<-angle[-pdead]
                pbt<-pbt[-pdead]
                ptotals<-ptotals[-pdead]
                ototals<-ototals[-pdead]
            }
            
        }
        
        
        #
        # calculate more available information
        #
        
        # angle and distance to top party
        prank<-rank(ptotals,ties.method="random")
        top<-which(prank==pn)
        angletop<-atan((ploc[2,top]-ploc[2,])/(ploc[1,top]-ploc[1,]))+
            pi*(ploc[1,top]<ploc[1,])%%(2*pi)
        angletop[is.nan(angletop)]<-0
        disttop<-sqrt(colSums((ploc[,top]-ploc)^2))
        steptop<-apply(matrix(c(disttop,rep(pstepsize,pn)),ncol=2),1,min)
        
        
        #
        # parties adapt
        #
        
        # stickers stick
        
        # aggregators move to party mean
        ploc[,ptype==2]<-pmloc[,ptype==2]
        
        # hunters win or tie - stay the course, lose - shift direction
        hunters<-which(ptype==3)
        if(length(hunters)>0) {
            angle[hunters]<-(angle[hunters]+(ototals[hunters]>=ptotals[hunters])*
                                 runif(length(hunters),pi/2,3*pi/2))%%(2*pi)
            ploc[1,hunters]<-ploc[1,hunters]+pstepsize*cos(angle[hunters])
            ploc[2,hunters]<-ploc[2,hunters]+pstepsize*sin(angle[hunters])
        }
        
        # predators move to largest party (unless they are the largest)
        predators<-which(ptype==4&prank!=pn)
        if(length(predators)>0) {
            ploc[1,predators]<-ploc[1,predators]+
                steptop[predators]*cos(angletop[predators])
            ploc[2,predators]<-ploc[2,predators]+
                steptop[predators]*sin(angletop[predators])
        }
        
        #
        # store information
        #
        
        # store previous election totals and locations
        ototals<-ptotals
        oploc<-ploc
        
    }
    
}


party.tournament(starttype=1)
# 
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
        plotteRvoteR(votersDataFrame = unifCircularEqualSalVoters, altPoints=c(0,0), plotIdeals = TRUE, plotICs = TRUE, showLegend = TRUE)
       
        #ALT at (0,.5)
        plotteRvoteR(votersDataFrame = unifCircularEqualSalVoters, altPoints=c(0,.5), plotIdeals = TRUE, plotICs = TRUE, showLegend = TRUE)
        
        
        ## ALL ELIPTICAL
        unifElipticalDiffSalVoters <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 7, distributionTypeGenVoters = "unif", distributionParametersGenVoters = c(-1,1), salienceHeterogeneityGenVoters = .25, maxRelativeSalienceGenVoters = 4, allEllipticalGenVoters = TRUE, probabilityElipticalGenVoters = 1, probabilityDiamondGenVoters = 0, probabilitySquareGenVoters = 0)
        
        #ALT at (0,0)
        plotteRvoteR(votersDataFrame = unifElipticalDiffSalVoters, altPoints=c(0,0), plotIdeals = TRUE, plotICs = TRUE, showLegend = TRUE)
        
        #ALT at (0,.5)
        plotteRvoteR(votersDataFrame = unifElipticalDiffSalVoters, altPoints=c(0,.5), plotIdeals = TRUE, plotICs = TRUE, showLegend = TRUE)
        
         
        
        ## Minkowski Heterogeneity
        unifMinkoDiffSalVoters <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 7, distributionTypeGenVoters = "unif", distributionParametersGenVoters = c(-1,1), salienceHeterogeneityGenVoters = .25, maxRelativeSalienceGenVoters = 3, allEllipticalGenVoters = FALSE, probabilityElipticalGenVoters = .34, probabilityDiamondGenVoters = .33, probabilitySquareGenVoters = .33)
        
        #ALT at (0,0)
        plotteRvoteR(votersDataFrame = unifMinkoDiffSalVoters, altPoints=c(0,0), plotIdeals = TRUE, plotICs = TRUE, showLegend = TRUE)
        
        #ALT at (0,.5)
        plotteRvoteR(votersDataFrame = unifMinkoDiffSalVoters, altPoints=c(0,.1), plotIdeals = TRUE, plotICs = TRUE, showLegend = TRUE)
        
        
        
        ## More of voters Minkowski Heterogeneity
        unifMinkoDiffSalVotersMore <- genVoters(numberOfDimensionsGenVoters = 2, numberOfVotersGenVoters = 15, distributionTypeGenVoters = "unif", distributionParametersGenVoters = c(-1,1), salienceHeterogeneityGenVoters = .25, maxRelativeSalienceGenVoters = 3, allEllipticalGenVoters = FALSE, probabilityElipticalGenVoters = .34, probabilityDiamondGenVoters = .33, probabilitySquareGenVoters = .33)
        
        #ALT at (0,0)
        plotteRvoteR(votersDataFrame = unifMinkoDiffSalVotersMore, altPoints=c(0,0), plotIdeals = TRUE, plotICs = TRUE, showLegend = FALSE)
        
        #ALT at (0,.9)
        plotteRvoteR(votersDataFrame = unifMinkoDiffSalVotersMore, altPoints=c(0,.5), plotIdeals = TRUE, plotICs = TRUE, showLegend = FALSE)
        
        
        

#####################################
# PARETO SET
#####################################

# Plot the voters and Pareto Set

plotParetoSet(unifMinkoDiffSalVotersMore, idealsOn  = TRUE)

        
       
#####################################
# WIN SET
#####################################
        
# Plot the voters and Win Set

        
        
  