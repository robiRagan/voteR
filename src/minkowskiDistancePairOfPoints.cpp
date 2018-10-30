// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include "../inst/include/voteR.h"
using namespace Rcpp;
//' minkowskiDistancePairOfPoints
//' 
//' Calculates the sailence weighted Minkowski distance between a pair of points.
//' 
//' This function will find the salience weighted Minkowski Distance between a
//' pair of points. For example an ideal point and an alternative. 
//' 
//' @param idealsPoint A 1 x numberDimensions vector of numerics. Each column is a dimension in the policy space,
//'   <d1,d2,...dn>.
//'   
//' @param altPoint A 1 x numberDimensions vector of numerics. Each column is a dimension in the policy space,
//'   <d1,d2,...dn>. 
//'   
//' @param orderScalar The ``order" of the Minkowski Distance being used. In
//'   this packge it should be an element of [1,100]. Examples for cases where
//'   the salience on all dimensions is equal: 1 = Manhattan Distance.  diamond
//'   shaped indifference curves, perfect substitutes. 2 = Euclidian Distance.
//'   If salience on all dimensions is equal, circular indifference curves. 100
//'   = Aproximates Chebyshev Distance. If salience on all dimensions is equal
//'   square indifference curves, perfect compliments.
//'   
//' @param salienceVector A vector of doubles that is numberOfDimensions long: 
//'   <sd1, sd2, sd3, ... sdk>. Each element of the vector represents the 
//'   *relative* saliance of each dimension for the voter. The dimension with
//'   the lowest salience serves as the "numeraire" dimension and should recieve
//'   a salience of 1. All the other saliences are expressed in units of this 
//'   "numeraire". So if a dimension is twice as important to a voter as the 
//'   numeraire dimension it recieves a salience of 2. Example:  1 3 1. The
//'   salience of dimension 1 (or 3), is the numeraire, dimension 2 is three
//'   times as saient as dimension 1 and dimesnion 3 is equally as salient as
//'   dimension one.
//'   
//' @return distVector A 1 x numAlts matrix that contains the Minkowski
//'   Distance between the ideal point and alternative, weighted by a voter's salience
//'   and Minkowski order. The sailence and the Minkowski order together
//'   determine a voter's utility function.
//' @family minkowski
//' @family utility functions
//' @export
//' 
// [[Rcpp::export]]
long double minkowskiDistancePairOfPoints(NumericVector idealVector, NumericVector altVector, double orderScalar, NumericVector salienceVector){
  
long double minkDist; // The container to put the distance in

        
              // Calculates the Minkowski for two points
              long double sum=0;
              long double weighted=0;
              for ( int i=0; i<idealVector.size(); i++ ){
                  weighted = salienceVector(i) * fabs(idealVector(i) - altVector(i) );
                  sum += pow ( weighted, orderScalar );
              }
              minkDist = pow ( sum, ( 1/orderScalar) ) ;
            // Store the distance for the two points in the apropriate location in the distMatrix
                  
return minkDist;
}
