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
# numVoters = number of voters
# numSims = number of simulations
# electFreq = frequency of elections
# minVoteShareThresh = party minimum threshold
# minVoteShareThreshPeriods = number of periods a party must be below threshold to die
# startingPartyType = starting party type -- if 0, random, otherwise the given type
# partyStepSize = step size for party strategies
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

party.tournament <- function( numVoters=1000, numSims=500, electFreq=10,
                              minVoteShareThresh=0.1, minVoteShareThreshPeriods=2, startingPartyType=0,
                              partyStepSize=0.05, plot=T, writeall=T, file="tourneyOut") {
    
    ## TEST ##
    numVoters<-1000
    numSims<-500
    electFreq<-10
    minVoteShareThresh<-0.1
    minVoteShareThreshPeriods<-2
    startingPartyType<-0
    partyStepSize<-0.05
    plot<-T
    writeall<-T
    file<-"tourneyOut"
    ## TEST ##
    
    
    # initialize voter ideal points, poll/election indicator
    voterIdeals<-rbind(rnorm(numVoters),rnorm(numVoters))
    pollOrElection<-c("poll","election")
    epsilon<-1/10^10
    
    if(plot) par( mfrow=c(1,3) ) # sets up three columned graphs
    
    # initialize party location, name, and type vectors
    ototals<-angle<-oploc<-pbt<-ploc<-pmloc<-pname<-ptype<-NULL
    
    # initialize proportional vote counts
    pcount<-mcount<-c(0.25,0.25,0.25,0.25)
    
    # intialize datafiles if there is one
    if(length(file)>0) write.table(t(c("Election","PartyID","Type","VoteRecd",
                                       "X","Y","Angle")), file=file, 
                                   row.names=F,col.names=F,quote=F,sep=",")
    
    
    for(period in 1:100) {
        
        #
        # party birth
        #
        
        if(period%%electFreq==1) {
            
            # party name, failure counter, previous total counter, and angle
            pname<-c(pname,1+(period-1)/electFreq)
            pbt<-c(pbt,0)
            ototals<-c(ototals,0)
            angle<-c(angle,runif(1,0,2*pi))
            
            # draw party type 1=sticker, 2=aggregator, 3=hunter, 4=predator
            if(period==1&startingPartyType!=0) {
                ptype<-startingPartyType
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
        dist<-matrix(0,nrow=pn,ncol=numVoters)
        for(i in 1:pn) {
            
            dist[i,]<-colSums((voterIdeals-ploc[,i])^2)+ # note sqrt not taken to save operation
                runif(numVoters,-epsilon,epsilon)    # and epsilon added to break ties
        }
        
        # voters choose closest party (epsilon added to break ties)
        vchoice<-apply(dist,2,which.min)
        
        # election / poll results
        ptotals<-hist(vchoice,breaks=0:pn,plot=F)$counts
        
        if(period%%electFreq==0) {
            
            # record counts and then randomly 
            # choose party type for next birth
            for(i in 1:4) {
                div<-sum(ptype==i)
                pcount[i]<-div
                if(div==0) div<-1
                mcount[i]<-sum(ptotals[ptype==i])/numVoters/div
            }
        }
        
        #
        # display information
        #
        
        # print party info
        if(writeall||period%%electFreq==0) {
            if(length(file)==0) {
                write.table(paste("Period",period,pollOrElection[(period%%electFreq==0)+1]),
                            row.names=F,col.names=F,quote=F)
                write.table(format(rbind(c("Election","Party ID","Type","Vote Recd",
                                           "X","Y","Angle"),
                                         cbind(rep(period/electFreq,pn),pname,ptype,round(ptotals/numVoters,3),
                                               round(t(ploc),3),round(angle,3)))),
                            row.names=F,col.names=F,quote=F)
            } else {
                write.table(cbind(rep(period/electFreq,pn), pname, ptype, round(ptotals/numVoters,3),
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
            if(length(pslist)==1) pmloc[,i]<-voterIdeals[,pslist]
            if(length(pslist)>1) pmloc[,i]<-rowMeans(voterIdeals[,pslist])
        }
        
        
        #
        # party death
        #
        
        if(period%%electFreq==0) { 
            
            # which parties fell below threshold?
            pweak<-as.numeric(ptotals<minVoteShareThresh*numVoters)
            
            # update number of times party falls below threshold
            pbt<-(pbt+pweak)*pweak
            
            
            # kill parties that have been below threshold for too long
            pdead<-which(pbt==minVoteShareThreshPeriods)
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
        steptop<-apply(matrix(c(disttop,rep(partyStepSize,pn)),ncol=2),1,min)
        
        
        #
        # parties adapt
        #
        
        # stickers stick
        
        # aggregators move to party mean
        ploc[,ptype==2]<-pmloc[,ptype==2]
        
        # hunters win or tie - stay the course, lose - shift direction
        hunters<-which(ptype==3)
        if(length(hunters)>0) {
            
            angle[hunters] <- ( angle[hunters] + ( ototals[hunters] >= ptotals[hunters] ) * runif( length(hunters), pi/2, 3*pi/2) )%%(2*pi)
            
            ploc[1,hunters]<-ploc[1,hunters]+partyStepSize*cos(angle[hunters])
            ploc[2,hunters]<-ploc[2,hunters]+partyStepSize*sin(angle[hunters])
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

party.tournament(startingPartyType=1)
