// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include "../inst/include/voteR.h"
using namespace Rcpp;


//' findParetoSet
//' 
//' Will find the pareto set in a two dimensional spatial voting model given a set of ideal points.
//' 
//' The pareto set of a set of ideal points is the convex hull of that set of
//' alternatives. This function will take a set of ideal points and returns
//' their convex hull as a matrix of points. The points are listed down the rows in clockwise
//' or counterclockwise order with the first and last element in the matrix being the same ideal point.
//' 
//' @param idealPoints A numberOfVoters x numberOfDimensions matrix of ideal points.
//' 
//' @return The set of points that constitutes the pareto set of the set of alternatives. The points are listed down the rows in clockwise
//' or counterclockwise order with the first and last element in the matrix being the same ideal point.
//' 
//' @export
// [[Rcpp::export]]
DataFrame findParetoSet(SEXP idealPoints){

    // Conversion of idealPoints here to boost::geometry polygon
    boostPolygon aBoostPolygon = as<boostPolygon>(idealPoints);

    boostPolygon hull; // Creates a blank container called hull that is of the type polygon
    
    // Compute the convex hull of the converted input, which is now called "poly" and output it into "hull".
    boost::geometry::convex_hull(aBoostPolygon, hull);

    // Convert hull into a NumericMatrix which is something that Rcpp can hand back to R
    return wrap(hull);
}
