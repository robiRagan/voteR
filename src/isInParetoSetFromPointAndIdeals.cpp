#include <Rcpp.h>
#include "../inst/include/voteR.h"
using namespace Rcpp;
//' isInParetoSetFromPointAndIdeals
//' 
//' This function takes an alternative and a pareto set and checks to see if the
//' point is in the pareto set. The function \code{isInParetoSetFromPointAndPS()} 
//' will take an alternative and the set of points that define the pareto set and 
//' check if the alternative is in the pareto set.
//' 
//' @param aSexpPoint An R vector (SEXP) that contains the corrdinates of the
//'   alternative that you want to check.
//' @param aSexpMatrix An R matrix (SEXP) that contains the ideal points of the
//'   voters.
//' @return Logical that is TRUE if the alt is in the pareto set and FALSE of
//'   the alt is not in the pareto set.
//' @export
// [[Rcpp::export]]
LogicalVector isInParetoSetFromPointAndIdeals(SEXP aSexpPoint ,SEXP aSexpMatrix){


    // Conversion of pointSEXP here to boost::tuple
     boostTuple aBoostTuple = as<boostTuple>(aSexpPoint);

    // Conversion of pointsMatrixSEXP here to boost::polygon
     boostPolygon aBoostPolygon = as<boostPolygon>(aSexpMatrix);

    // Creates a blank container called hull that is of the type polygon to store the pareto set.
    boostPolygon paretoSet; 
    
    // Compute the convex hull of the converted input, which is now claled "poly" and output it into "hull".
    boost::geometry::convex_hull(aBoostPolygon, paretoSet);
    
    // Compute whether the point "aBoostTuple" is in the pareto set "aBoostPolygon"
    return boost::geometry::covered_by(aBoostTuple, paretoSet);
    
}
