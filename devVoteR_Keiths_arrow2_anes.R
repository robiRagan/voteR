####################################################################################################################
#				THE PROBABILITY OF VIOLATING ARROW'S CONDITIONS
# DESCRIPTION
#   This program calculates the probability that various preference aggregation procedures (PARs) violate the joint conditions of Arrow's
#   Impossibility Thoerem.  Namely, the probability they will violate Pareto, Transitivity, or IIA over a pre-specified distribution
#   of individual preferences.  This is only valid for PARs that do not violate non-dictatorship and produce complete rankings.
#
#   The program utilizes Rcpp for most of it's computations to increase speed.  All of the Rcpp functions are listed in iterate.cpp until we 
#   can properly package them.
#
#   VERSION.  This is the primary version for making tables.  Version 2b makes figures.  Variations are handled with different versions of iterate.cpp 
#	      below.
#
# NOTES
# 1-You use -9 for missing in this program.
# 2-The program works for PAR's that produce socially strict preference or indifference over pairs of alternatives 
#   (R, weak preference, is not allowed)
# 3- make sure the PAR's assign indifference to both (i,j) and (j,i)
# -I'm pretty sure plurality will violate IIA for all preference profiles (because you can always go from ordered to second and third tied)
#   but out procedure may not capture that.
####################################################################################################################

##### CASES #####
# N=5, 30, 100, 10,000
# dt=0,1
#################

rm(list=ls())
library(devtools)
library(Rcpp)
library(gtools)

# declare some primitives (if you want to do one case at a time; otherwise use input file)
set.seed(021114)
N<- 10000	# the number of individuals
A<- 3		# the number of alternatives.			
dt<-1		# distribution type: =0 if IC, =1 if IAC.
T<-500000	# the number of trials in the simulation.

# Preference Weights from ANES distributions (only used if dt==2)
# V<-c(0,0,0,0,0,0)						# default (use this for dt=0 and dt=1 so we don't get confused)
# V<-c(0.4106,0.0438,0.3491,0.0875,0.0289,0.0801)		# 1968 
# V<-c(0.2018,0.2910,0.1655,0.1618,0.1043,0.0756)		# 1992 
# V<-c(0.1345,0.2769,0.1547,0.2612,0.1200,0.0527)		# 2000 

# read input file
# input<-read.delim('C:/Users/dougherk/Dropbox/arrow_probability_paper/programs/inputj.txt', header = TRUE, sep = "\t")
# input b is a continuum of N from 1 to 200.
# input<-read.delim('C:/Users/dougherk/Dropbox/arrow_probability_paper/programs/inputb.txt', header = TRUE, sep = "\t")
# inputs_anes.txt includes both parameters and ANES weights
# input<-read.delim('C:/Users/dougherk/Dropbox/arrow_probability_paper/programs/inputs_anes.txt', header = TRUE, sep = "\t")
 input<-read.delim('C:/Users/dougherk/Dropbox/arrow_probability_paper/programs/inputs2b.txt', header = TRUE, sep = "\t")


# set the following to the directory from which C++ functions are called
setwd('C:/Users/dougherk/Dropbox/arrow_probability_paper/programs/c_functions/')

# load cpp functions
# sourceCpp("iterate.cpp")			# iterate is faster (it cancels a trial as soon as it finds a contradiction in any criterion; totals accurate)
 sourceCpp("iterate2.cpp")			# iterate2 is slower (because it goes through each criterion until contradiction in that criterion 
						#   - gives more accurate prob for Pareto, trans, and IIA; totals inaccurate)
# sourceCpp("iterate3.cpp")			# iterate3 is used only to count the number of cases where a Pareto preferred alternative exists.  All other
 						#  results should be considered bogus (use version 2 instead). 
# sourceCpp("iteratex.cpp")			# top-ranked pareto and slow IIA evaluation


for(tt in 1:nrow(input)){			# for each set of inputs in the input files (tt is a row in the matrix of inputs)
  N<-input[tt,"N"]
  A<-input[tt,"A"]
  dt<-input[tt,"dt"]
  T<-input[tt,"T"]
  # only use following for <inputs_anes.txt> 
  yr<- input[tt,"yr"]
  V<-c(input[tt,"DRI"],input[tt,"DIR"],input[tt,"RDI"],input[tt,"RID"],input[tt,"IDR"],input[tt,"IRD"])

  # Generate all possible permutations of an n-tuple using gtools (all possible strict linear preference orders)
  # 	n -> size of source vector (i.e., number of Alternatives)
  # 	r -> size of target vector (note: r == n for our purposes, but can also be r < n)
  # 	v -> source vector, defaults to 1:n (we want 0:A-1)
  perm <-permutations(n = A, r = A, v = 0:(A-1))	

  # Run the simulations for T trials
  out<-iterate(perm, N, T, dt, V)			
  
  # calculate the proportion of violations in the T trials (and combine the output in matix form)
  out2<-rbind(out$count_prr,out$count_aprr,out$count_borda,out$count_nanson,out$count_hare,out$count_pmr,out$count_copeland,out$count_schulze)
  tot<-rowSums(out2)
  out2<-cbind(out2,tot)/T

  # PRINT OUTPUT
  # reset the default directory
    setwd('C:/Users/dougherk/Dropbox/arrow_probability_paper/programs/output/ANES_distr/')
  # print a header to the output file
    file1<-sprintf("out_yr%s_N%s_A%s_dt%s_T%s.txt", as.character(yr), as.character(N), as.character(A), as.character(dt), as.character(T))
    header1<-"The Joint Probability of violating Arrow's Conditions (corrected IIA with blocking alternatives), from arrow2.R."
    header2<-sprintf("     Assuming: N=%s, A=%s, dt=%s, Trials=%s.", as.character(N), as.character(A), as.character(dt), as.character(T))
    header3<-sprintf("     Year = %s, Pref Weights=(%s,%s,%s,%s,%s,%s).\n", as.character(yr), as.character(V[1]), as.character(V[2]), as.character(V[3]), as.character(V[4]),  as.character(V[5]), as.character(V[6]))
    if(dt==2){
      write.table(rbind(header1,header2,header3), file1, quote = FALSE, row.names = FALSE, col.names=FALSE)
    } 
    if(dt<2){
      write.table(rbind(header1,header2), file1, quote = FALSE, row.names = FALSE, col.names=FALSE)
    } 
  # append the numeric output to the output file
    colnames(out2)<-c("Pareto","Trans","IIA","TOTAL")
    rownames(out2)<-c("Plurality Rank","Anti-PRR","Borda Count","Nanson Rule","Hare Rule","Pairwise Maj","Copeland","Schulze")  # comment this out for iterate 3
    write.table(out2, file=file1, append = TRUE, sep="\t", quote = FALSE, row.names = TRUE, col.names=TRUE)

}

# ==========================================================================================
# USEFUL R FUNCTION: READ DATA FROM CLIPBOARD, THAT IS TAB DELIMINATED COPIED FROM SAY EXCEL
# read.clip <- function(header=FALSE) {
#   read.table("clipboard",sep="\t",header=header)
# }