% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/genVoters.R
\name{genVoters}
\alias{genVoters}
\title{genVoters
Generates a set of voters with ideal points and the pararmeters for their utility functions.
 
Generates a set of voters, each with a voterID number, an ideal point using \code{\link{genIdeals}}, 
a salience weighting matrix using \code{\link{genSalience}}, a Minkowsi Order number using 
\code{\link{genMinkoOrder}}, and a loss function order using \code{\link{genLossOrder}}.
These ideal point, salience weights, minkowski distance order and loss function order can be used
by \code{\link{minkoUtilitySets}} to calulate utility and find indifference curves.}
\usage{
genVoters(
  numberOfDimensionsGenVoters = 1,
  numberOfVotersGenVoters = 15,
  distributionTypeGenVoters = "unif",
  distributionParametersGenVoters = c(-1, 1),
  dimOneBoundsGenVoters = c(-Inf, Inf),
  dimTwoBoundsGenVoters = c(-Inf, Inf),
  salienceHeterogeneityGenVoters = 1,
  maxRelativeSalienceGenVoters = 2,
  allEllipticalGenVoters = TRUE,
  probabilityElipticalGenVoters = 1,
  probabilityDiamondGenVoters = 0,
  probabilitySquareGenVoters = 0,
  lossOrderForAllGenVoters = FALSE,
  probabilityLinearGenVoters = 1,
  probabilityQuadraticGenVoters = 0,
  probabilityCubicGenVoters = 0
)
}
\arguments{
\item{numberOfDimensionsGenVoters}{The number of policy dimensions.}

\item{numberOfVotersGenVoters}{Number of voters to generate.}

\item{distributionTypeGenVoters}{A string identifying the base R discribution to draw
the ideal points from. Uses the base R random number generation family of
commands rxxxx (see ?distributions). The user should specify the
distribution as a string using the standard R abreviation for the
distribution (see ?distributions for a list). Currently supported are: "norm",
"unif", "binom", "cauchy", "chisq", "weibull"}

\item{distributionParametersGenVoters}{A vector that contains the additional
parameters needed by the particular rxxxx function for a distribtuion. (see
?rxxxx where xxxx is a function listed under ?distribution). Example for a
Normal(0,1), use: c(0,1).}

\item{dimOneBoundsGenVoters}{A vector that contains the starting and ending poitns of t
he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf) if no boundary is provided.}

\item{dimTwoBoundsGenVoters}{A vector that contains the starting and ending poitns of t
he first dimension. Example: c(0,1). Defaults to c(-Inf, Inf)  if no boundary is provided.}

\item{salienceHeterogeneityGenVoters}{scalar [0,1] The probability that any given dimension will also have a salience of 1 for a voter. If this is set to 1 then all dimensions will have the same salience.}

\item{maxRelativeSalienceGenVoters}{scalar The maximum relative salience for a dimension you want to allow your voters to have.}

\item{allEllipticalGenVoters}{logical If If FALSE (the default) then the agent's Minkowski order for their utility function is determined by the other parameters in this function. If TRUE then all agents will have eliptical indifference curves (Minkowski Order = 2). If their salience's across all dimesnions is 1 then the indifference curves will be circles.}

\item{probabilityElipticalGenVoters}{scalar [0,1] The probability that any given voter will have elliptical (Minkowski Order of 2) indifference curves.}

\item{probabilityDiamondGenVoters}{scalar [0,1] The probability that any given voter will have Manhattan/Diamond (Minkowski Order = 1) indifference curves.}

\item{probabilitySquareGenVoters}{scalar [0,1] The probability that any given voter will have Chebyshev/Square (Minkowski Order = 100) indifference curves.}

\item{lossOrderForAllGenVoters}{logical or scalar If If FALSE (the default) then the agent's loss order for their utility function is determined by the other parameters in this function. If a number is provided this will be the order of the loss function for all voters.}

\item{probabilityLinearGenVoters}{scalar [0,1] The probability that any given voter will have elliptical (Minkowski Order of 2) indifference curves.}

\item{probabilityQuadraticGenVoters}{scalar [0,1] The probability that any given voter will have Manhattan/Diamond (Minkowski Order = 1) indifference curves.}

\item{probabilityCubicGenVoters}{scalar [0,1] The probability that any given voter will have Chebyshev/Square (Minkowski Order = 100) indifference curves.}
}
\value{
outVotersDataFrame data.frame The voters data frame will have the following format.

 voterID: A numeric identifier unique to the voter.
 xIdeal: The x coordinate of the voter's ideal point.
 yIdeal: The y coordinate of the voter's ideal point.
 minkoOrder: The Minkowski order of the voters MInkowski metric based utility function. = 1, is City Block. = 2 is Euclidian and 100 = is  See ?Minkowski. 
 xSalience: The salience of the x dimension for the voter. The dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire. 
 ySalience: The salience of the y dimension for the voter. he dimension with the lowest salience is normalized to 1 and it is the numerarier, the salience of other dimension is measured in units of the numerarire.
}
\description{
genVoters
Generates a set of voters with ideal points and the pararmeters for their utility functions.
 
Generates a set of voters, each with a voterID number, an ideal point using \code{\link{genIdeals}}, 
a salience weighting matrix using \code{\link{genSalience}}, a Minkowsi Order number using 
\code{\link{genMinkoOrder}}, and a loss function order using \code{\link{genLossOrder}}.
These ideal point, salience weights, minkowski distance order and loss function order can be used
by \code{\link{minkoUtilitySets}} to calulate utility and find indifference curves.
}
\section{TODO}{

Add calibration functionality and move this into C++.
}

\examples{
  genVoters(numberOfDimensionsGenVoters=2, numberOfVotersGenVoters=100, distributionTypeGenVoters ="norm", distributionParametersGenVoters = c(0,1), dimOneBoundsGenVoters = c(-Inf,0), dimTwoBoundsGenVoters = c(0,Inf), salienceHeterogeneityGenVoters=1, maxRelativeSalienceGenVoters=2)
 
  genVoters(numberOfDimensionsGenVoters=1, numberOfVotersGenVoters=100, distributionTypeGenVoters ="unif", distributionParametersGenVoters = c(0,1), salienceHeterogeneityGenVoters=1, maxRelativeSalienceGenVoters=2)
}
