#include <Rcpp.h>
#include "../inst/include/voteR.h"
using namespace Rcpp;
//' isInICFromPointAndIC
//' 
//' This function takes an alternative and an indifference curve and checks to see if the
//' point is in the indifference curve. The more general function
//' \code{isInICFromPointAndIdeals()} will take an alternative and a set
//' of ideal points as its arguments.
//' 
//' @param aSexpPoint An R vector (SEXP) that contains the corrdinates of the
//'   alternative that you want to check.
//' @param aSexpMatrix An R matrix (SEXP) that contains all the points that
//'   define an indifference curve. The set of points should progress clockwise or
//'   counter clockwise as you go down the rows of the matrix and the first and
//'   last rows should of the matrix should be the same. The function
//'   \code{findICPoints()} generates a matrix that meets all of these
//'   requrements.
//' @return Logical that is TRUE if the alt is inside the agents indifference curve and FALSE if
//'   the alt is not inside the agents indifference curve. NOTE: Being inside the indifference curve 
//'   is equivalent to being in the agents preferred-to-set.
//' @export
// [[Rcpp::export]]
LogicalVector isInICFromPointAndIC(SEXP aSexpPoint ,SEXP aSexpMatrix){

    // Conversion of pointSEXP here to boost::tuple
     boostTuple aBoostTuple = as<boostTuple>(aSexpPoint);

    // Conversion of pointsMatrixSEXP here to boost::polygon
     boostPolygon aBoostPolygon = as<boostPolygon>(aSexpMatrix);
     
    // Compute whether the point "aBoostTuple" is in the pareto set "aBoostPolygon"
    return boost::geometry::covered_by(aBoostTuple, aBoostPolygon);

}
