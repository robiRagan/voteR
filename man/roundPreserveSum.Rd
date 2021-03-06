% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roundPreserveSum.R
\name{roundPreserveSum}
\alias{roundPreserveSum}
\title{roundPreserveSum
Rounds a set of fractions and decimals to whole numbers while preserving their sum. Uses the "largest remainder method".}
\usage{
roundPreserveSum(vectorOfNumbersRoundPreserveSum, numDecimals = 0)
}
\arguments{
\item{vectorOfNumbersRoundPreserveSum}{The vector of numbers to round to integers while keep ing the sum the same.}
}
\value{
outVectorOfRoundedNumbers
}
\description{
Takes a vector of fractions or integers and rounds them so they are all integers, but the sum of the vector remains the same. 
There are many names for the method implemented here: see  \url{http://www.ams.org/publicoutreach/feature-column/fcarc-apportionii1}.
This code was adapted from \url{https://biostatmatt.com/archives/2902}.
}
\examples{
roundPreserveSum(c(0.333, 0.333, 0.334), 2)

sum( roundPreserveSum(c(0.333, 0.333, 0.334), 2) )

roundPreserveSum(c(0.333, 0.333, 0.334), 0)

}
