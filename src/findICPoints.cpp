// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins("cpp11")]]
#include <Rcpp.h>
#include <vector>
#include "../inst/include/voteR.h"
using namespace Rcpp;
//' findICPoints
//' 
//' Takes an ideal point and and alternative and returns the requested number of
//' points that are on the indifference curve for the alternative. The function
//' can take into account different distance metrics, and the
//' salience voters have over the different issue dimensions.
//' 
//' Possible TODO: Could add a simplify from <boost/geometry/algorithms/simplify.hpp> to possibaly decrease the number of points in each IC.
//' 
//' @param idealPoint An ideal point for a voter.
//'   
//' @param altPointVector An alternative in the choice set.
//'   
//' @param orderScalar Order for the voter's distance metric. 1 is the Manhattan
//'   Distance, 2 is the Cartesian Distance and 100000 is the Chebechev
//'   Distance. You can choose any integer inbetween.
//'   
//' @param salienceVector The salience the voter has for each issue dimension. The element that
//'     coresponds to the least salient matrix is a 1 and all the other dimensions have their salience measured
//'     relative to that dimension. 
//' 
//' @param precision How precise you want the IC to be. The lower the precision the higher the number
//'     points on the IC that are found.
//'   
//' @return indifferencePoints A set of points that are on the indifference curve.
//' 
//' @export
//'
// [[Rcpp::export]]
DataFrame findICPoints(NumericVector voterID, NumericVector idealPoint, NumericVector altPointVector, double orderScalar, NumericVector salienceVector, double precision=.01){
    
    // INVERT SALIENCE VECTOR
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
    

    // Find the Radius of the SuperElipse.
    
    double theSuperElipseRadius = voteR::superElipseRadius(idealPoint, altPointVector, orderScalar, salienceVector);
    
    
    std::vector<double> xCoords;
    std::vector<double> yCoords;
    
    double expN = 2.0 / orderScalar;
    double expM = 2.0 / orderScalar;
    // precision *= PI + 1 does this: precision = precision * (PI + 1);
    precision *= PI;
    
    for (double theta = -PI; theta < PI; theta += precision) {
        double cosTheta = cos ( theta );
        double sinTheta = sin ( theta );
        double absCosTheta = fabs ( cosTheta );
        double absSinTheta = fabs ( sinTheta );
        double powAbsCosTheta = pow ( absCosTheta, expM );
        double powAbsSinTheta = pow ( absSinTheta, expN );
        double x = theSuperElipseRadius * convertedSalience(0) * powAbsCosTheta * voteR::sgn ( cos ( theta ) );
        double y = theSuperElipseRadius * convertedSalience(1) * powAbsSinTheta * voteR::sgn ( sin ( theta ) );
        xCoords.emplace_back ( x + idealPoint(0) );
        yCoords.emplace_back ( y + idealPoint(1) );
    }
    
    // Convert std::vector<double> to Rcpp::NumericVector
    NumericVector xCoordsRcpp = wrap(xCoords);
    NumericVector yCoordsRcpp = wrap(yCoords);
    
    // Create the output matrix
    // NumericMatrix ICout(xCoords.size(), 3);
    
    // Create voter ID as a vector to be used for grouping purposes when plotting
    
    NumericVector voterIDVector = rep( voterID, xCoords.size() );
    
    // Store the output in a list before sending it back to R
    return DataFrame::create(_["voterID"]= voterIDVector, _["xCoords"]= xCoordsRcpp, _["yCoords"]= yCoordsRcpp);
    
    // ICout(_,0) = voterIDVector;
    // ICout(_,1) = xCoordsRcpp;
    // ICout(_,2) = yCoordsRcpp;
    // 
    // 
    // return wrap(ICout);
    
}
