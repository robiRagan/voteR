#include <Rcpp.h>
#include "../inst/include/voteR.h"
using namespace Rcpp;
//' isInParetoSetFromPointAndPS
//' 
//' This function takes an alternative and a pareto set and checks to see if the
//' point is in the pareto set. The more general function
//' \code{isInParetoSetFromPointAndIdeals()} will take an alternative and a set
//' of ideals as its arguments.
//' 
//' @param aSexpPoint An R vector (SEXP) that contains the corrdinates of the
//'   alternative that you want to check.
//' @param aSexpMatrix An R matrix (SEXP) that contains all the points that
//'   define a pareto set. The set of points should progress clockwise or
//'   counter clockwise as you go down the rows of the matrix and the first and
//'   last rows should of the matrix should be the same. The function
//'   \code{findParetoSet()} generates a matrix that meets all of these
//'   requrements.
//' @return Logical that is TRUE if the alt is in the pareto set and FALSE of
//'   the alt is not in the pareto set.
//' @export
// [[Rcpp::export]]
LogicalVector isInParetoSetFromPointAndPS(SEXP aSexpPoint ,SEXP aSexpMatrix){

    // Conversion of pointSEXP here to boost::tuple
     boostTuple aBoostTuple = as<boostTuple>(aSexpPoint);

    // Conversion of pointsMatrixSEXP here to boost::polygon
     boostPolygon aBoostPolygon = as<boostPolygon>(aSexpMatrix);
     
    // Compute whether the point "aBoostTuple" is in the pareto set "aBoostPolygon"
    return boost::geometry::covered_by(aBoostTuple, aBoostPolygon);

}
