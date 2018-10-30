#' roundPreserveSum
#' Rounds a set of fractions and decimals to whole numbers while preserving their sum. Uses the "largest remainder method".
#' 
#' Takes a vector of fractions or integers and rounds them so they are all integers, but the sum of the vector remains the same. 
#' There are many names for the method implemented here: see  \url{http://www.ams.org/publicoutreach/feature-column/fcarc-apportionii1}.
#' This code was adapted from \url{https://biostatmatt.com/archives/2902}. 
#' @param vectorOfNumbersRoundPreserveSum The vector of numbers to round to integers while keep ing the sum the same. 
#' @return outVectorOfRoundedNumbers 
#' @examples
#' roundPreserveSum(c(0.333, 0.333, 0.334), 2)
#' 
#' sum( roundPreserveSum(c(0.333, 0.333, 0.334), 2) )
#' 
#' roundPreserveSum(c(0.333, 0.333, 0.334), 0)
#' 
#' @export
roundPreserveSum <- function(vectorOfNumbersRoundPreserveSum, numDecimals = 0) {
    ## TEST ##
    # vectorOfNumbersRoundPreserveSum <- rawVotesPerAlt[10, ]
    ## TEST ##
    scaleUpBy <- 10 ^ numDecimals
    vectorOfNumbersRoundPreserveSum <- vectorOfNumbersRoundPreserveSum * scaleUpBy
    vectorOfNumbersRounded <- floor(vectorOfNumbersRoundPreserveSum)
    remainingSumToBeAllocated <- sum(vectorOfNumbersRoundPreserveSum, na.rm=TRUE) - sum(vectorOfNumbersRounded, na.rm=TRUE)
    remaindersAfterRounding <- vectorOfNumbersRoundPreserveSum-vectorOfNumbersRounded
    orderOfRemainders <- order(-remaindersAfterRounding, na.last = NA)
    indices <- head(orderOfRemainders, round(remainingSumToBeAllocated) )
    vectorOfNumbersRounded[indices] <- vectorOfNumbersRounded[indices] + 1
    vectorOfNumbersRounded / scaleUpBy
}
