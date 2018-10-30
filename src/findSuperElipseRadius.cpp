// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include "../inst/include/voteR.h"
using namespace Rcpp;
//' superElipseRadius
//' 
//' Returns the distance between the center of a super elipse and a point on the
//' elipse. The distance is mesured in the "order" of the superelipse. This
//' order is the same as the minkowski order for distance. 
//' 
//' @param idealPoint An ideal point for a voter.
//'   
//' @param altPoint An alternative in the choice set.
//'   
//' @param orderScalar Order for the voter's distance metric. 1 is the Manhattan
//'   Distance, 2 is the Cartesian Distance and 100000 is the Chebechev
//'   Distance. You can choose any integer inbetween.
//'   
//' @param salienceVector The salience the voter has for each issue dimension. The element that
//' coresponds to the least salient matrix is a 1 and all the other dimensions have their salience measured
//' relative to that dimension. 
//'   
//' @return indifferencePoints A set of points that are on the indifference curve.
//' @export
//' 
// [[Rcpp::export]]
long double findSuperElipseRadius(NumericVector idealPoint, NumericVector altPoint, double orderScalar, NumericVector salienceVector){

 // First we need to find the distance from the ideal point to the alternative using the given metric. 

// First calculate the "distance" between the two points using the voter's distance metric (minkowski order) and salience
 
// double minkDistance = voteR::minkowskiUtilityPairOfPoints(idealPoint, altPoint, orderScalar, salienceVector);

// Now use the SuperElipse formula to find the distance raised to the power associated with the minkowski order.


    // First the salience vector has to be inversed because salience in all of the
    // voteR function assumes a numeriare dimension and then all the other
    // dimensions are expressed as multiples. So if dimension 2 is twice as 
    // salient as dimension 1, then the salience vector is <1,2>. 
    // In the superelipse formula the salience is based on the ratio of the 
    // denominators in each of the sub-expressions (that is the "a" and the "b" terms.). 
    // The difference in the distance on a dimension is divided by the corespondeing term 
    // (for example x-x_0 is divided by "a"). So to find "a" and "b" we need the inverse
    // of the provided salience vector. In the case of the salience vector <1,2> above we 
    // need "a" = 1 and "b" = 1/2" so the salience of each dimension is represented correctly. 

    NumericVector convertedSalience = pow(salienceVector, -1); 
    
    
    // Now find the difference on every dimension between the ideal point and the alternative

    NumericVector differenceEachDimension = idealPoint - altPoint;

    // Divide the difference on each dimension by that dimensions converted salience number
    
    NumericVector salienceWeightedDifference = differenceEachDimension/convertedSalience;
    
    // Take each absolute value of the salience weighted difference
    
    NumericVector absoluteSalienceWeightedDifference = abs(salienceWeightedDifference);
    
    // Take each element in the vector (which represents each sub expression in
    // the eventual summation) to the power associated with the order of the
    // minkowsi distance for this voter.

    NumericVector raisedToTheMinkowsiOrder = pow(absoluteSalienceWeightedDifference, orderScalar);
    
    // Sum the vector
    
    long double sumOfTheLHS = sum(raisedToTheMinkowsiOrder);
    
    // Now take the n-root of the sum where n is the minkowsi order.
    
    long double minkowskiRadiusOfSuperElipse = pow( sumOfTheLHS, (1/orderScalar) );
    
    

return minkowskiRadiusOfSuperElipse;

// exTerm = abs( (altPoint(1)-idealPoint(1))/

 
 
 
 // NumericMatrix distMatrix(idealsMatrix.nrow(),altsMatrix.nrow()); // The container to put the distances in


 // for(int k =0; k< altsMatrix.nrow(); ++k){ // This loops over the second set of points, usually the smaller set.
  
   //   for(int j = 0; j < idealsMatrix.nrow(); ++j) {// This loops over the first set of points, usually the larger set.
        
              // Calculates the Minkowski for two points
            //  double sum=0;
             // double weighted=0;
              // for ( int i=0; i<idealsMatrix.ncol(); i++ ){
                 // weighted = salienceMatrix(j,i) * fabs(idealsMatrix(j,i) - altsMatrix(k,i) );
                 // sum += pow ( weighted, orderVector(j) );
              //}
              //distMatrix(j,k) = pow ( sum, ( 1/(orderVector(j)) ) );
            // Store the distance for the two points in the apropriate location in the distMatrix
                  
     // }// end j
 // }// end k

// return convertedSalience;
}

//// THIS HAS BEEN TESTED AGAINST OTHER SOURCES AND WORKS!