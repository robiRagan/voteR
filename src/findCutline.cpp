// // [[Rcpp::interfaces(r, cpp)]]
// // [[Rcpp::plugins("cpp11")]]
// #include <Rcpp.h>
// #include <vector>
// #include "../inst/include/voteR.h"
// using namespace Rcpp;
// //' findCutline (I'm not sure if I want this b/c its not really generaliziable. Instead I should just check distance based on each voters Utility function when counting votes.)
// //' 
// //' Takes two alternatives (Alt1 and Alt2) in a 2 dimensional space and generates a cutline that seperates the issue space into two sets. 
// //' One set is the set of alternatives closer to Alt1 and one set is the set of alternatives closer to Alt2. Note that the cutline method
// //' is only appropriate for cases where indifference curves are circular (minkowski order = 2, and salience on all dimensions in equal.)
// //' 
// //' 
// //' @param altsMatrix A numberOfAlts x numberOfDimensions matrix that contains the two alternatives the cutline is based on
// //'   
// //' @param altPointVector An alternative in the choice set.
// //'   
// //'   
// //' @param salienceVector The salience the voter has for each issue dimension. The element that
// //' coresponds to the least salient matrix is a 1 and all the other dimensions have their salience measured
// //' relative to that dimension. 
// //' 
// //' @param numberOfPoints The number of points on the indifference curve the
// //'   function should report back. Half are found by scanning the x-range and
// //'   half are found by scanning the y-range.
// //'   
// //' @return indifferencePoints A set of points that are on the indifference
// //'   curve.
// //' @export
// //'
// // [[Rcpp::export]]
// DataFrame findCutline(NumericMatrix altPointMatrix){
//     
//     
//     std::vector<double> xCoords;
//     std::vector<double> yCoords;
//     
//     double expN = 2.0 / orderScalar;
//     double expM = 2.0 / orderScalar;
//     // precision *= PI + 1 does this: precision = precision * (PI + 1);
//     precision *= PI;
//     
//     for (double theta = -PI; theta < PI; theta += precision) {
//         double cosTheta = cos ( theta );
//         double sinTheta = sin ( theta );
//         double absCosTheta = fabs ( cosTheta );
//         double absSinTheta = fabs ( sinTheta );
//         double powAbsCosTheta = pow ( absCosTheta, expM );
//         double powAbsSinTheta = pow ( absSinTheta, expN );
//         double x = theSuperElipseRadius * convertedSalience(0) * powAbsCosTheta * voteR::sgn ( cos ( theta ) );
//         double y = theSuperElipseRadius * convertedSalience(1) * powAbsSinTheta * voteR::sgn ( sin ( theta ) );
//         xCoords.emplace_back ( x + idealPoint(0) );
//         yCoords.emplace_back ( y + idealPoint(1) );
//     }
//     
//     // Convert std::vector<double> to Rcpp::NumericVector
//     NumericVector xCoordsRcpp = wrap(xCoords);
//     NumericVector yCoordsRcpp = wrap(yCoords);
//     
//     // Create the output matrix
//     NumericMatrix ICout(xCoords.size(), 3);
//     
//     // Create voter ID as a vector to be used for grouping purposes when plotting
//     
//     NumericVector voterIDVector = rep( voterID, xCoords.size() );
//     
//     // Place the voter ID as the first column
//     ICout(_,0) = voterIDVector;
//     ICout(_,1) = xCoordsRcpp;
//     ICout(_,2) = yCoordsRcpp;
//     
//     
//     return wrap(ICout);
//     
//     // List prefToSetCheck; 
//     // prefToSetCheck["theXCords"] = xCoords; 
//     // prefToSetCheck["theYCords"] = yCoords; 
//     // return(prefToSetCheck);
// }
