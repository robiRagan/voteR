#include <Rcpp.h> 
#include "ranker.h"
using namespace Rcpp;
 
//' Preferece Ording Using Minkowski Distance
//' @param idealsMatrix A numberVoters x numberDimensions matrix of numerics.
//'   Each row is a voter and each column is a dimension in the policy space,
//'   <d1,d2,...dn>. The values in a given row give the location of one voters
//'   ideal point, the point that is the argMax of their utility
//'   function.Example: In a two dimensional space the if entry in the matrix at
//'   location (v=2,d1=2,d=2) is <.5,.5> then the peak of voter 2's utility
//'   function in the two dimensional issue space is at .5,.5.
//'   
//' @param altsMatrix A numberAlts x numberDimensions matrix of numerics. Each
//'   row is an alternative and each column is a dimension in the policy space,
//'   <d1,d2,...dn>. The values in a given row give the location of that
//'   alternative in the policy space. Example: In a two dimensional space the
//'   if entry in the matrix at location (d=2,d1=2,d=2) is <.25,.75> then the
//'   location of that alternative in the two dimensional issue space is at
//'   .25,.75.
//'   
//' @param minkoOrderVector A numVoters lengh vector of doubles. It is the ``order" 
//'   of the Minkowski Distance being used. In this packge it should be an 
//'   element of [1,100]. Examples for cases where the salience on all 
//'   dimensions is equal: 1 = Manhattan Distance.  diamond shaped indifference
//'   curves, perfect substitutes. 2 = Euclidian Distance. If salience on all
//'   dimensions is equal, circular indifference curves. 100 = Aproximates
//'   Chebyshev Distance. If salience on all dimensions is equal square
//'   indifference curves, perfect compliments.
//'   
//' @param salienceMatrix A matrix of doubles that is numberOfDimensions long: 
//'   <sd1, sd2, sd3, ... sdk>. Each element of the vector represents the 
//'   *relative* saliance of each dimension for a voter. The dimension with the 
//'   lowest salience serves as the "numeraire" dimension and should recieve a 
//'   salience of 1. All the other saliences are expressed in units of this 
//'   "numeraire". So if a dimension is twice as important to a voter as the 
//'   numeraire dimension it recieves a salience of 2. Example: The second row
//'   is 1 2 3. This means that for voter two (second row) the salience of dimension 1 is the
//'   numeraire, dimension 2 is twice as saient as dimension 1 and
//'   dimension 3 is three times as salient as dimension one.
//'
//' @param lossOrderVector A numVoters lengh vector of doubles. It is the ``order" 
//'   of the loss function being used. In short the Minkowski Distance is the way that
//'   a voter percieves the distance between their ideal point and another alternative. 
//'   The loss function tells us how that distance is converted into utility for the voter. 
//'   If lossOrderVector=1, then the voter has linear loss and their disutility for an
//'   alternative is equal to the minkowski distance. If lossOrderVector=2 then the voter has
//'   a quadratic loss function and their disutility is the square of the minkowski distance.
//'   This has the effect of causing utility to fall slowly at first as they evaluate alternatives
//'   closer to their ideal, but then once an alternative is sufficently far away utility falls much
//'   faster than in teh quadratic case. 
//'   
//' @return prefMatrix A numVoters x numAlts matrix that contains the Minkowski
//'   Utility between each voter and each alternative, given a voter's salience, 
//'   Minkowski order and loss function. The sailence, Minkowski order and loss function
//'   together determine a voter's utility function.
//' @family minkowski
//' @family utility functions
// [[Rcpp::export]]
NumericMatrix prefOrderMinko(NumericMatrix idealsMatrix, NumericMatrix altsMatrix, NumericVector minkoOrderVector, NumericMatrix salienceMatrix, double lossOrderVector){
  
 NumericMatrix distMatrix(idealsMatrix.nrow(),altsMatrix.nrow()); // The container to put the distances in


  for(int k =0; k< altsMatrix.nrow(); ++k){ // This loops over the second set of points, usually the smaller set.
  
      for(int j = 0; j < idealsMatrix.nrow(); ++j) {// This loops over the first set of points, usually the larger set.
        
              // Calculates the Minkowski for two points
              double sum=0;
              double weighted=0;
              for ( int i=0; i<idealsMatrix.ncol(); i++ ){
                  weighted = salienceMatrix(j,i) * fabs(idealsMatrix(j,i) - altsMatrix(k,i) );
                  sum += pow ( weighted, minkoOrderVector(j) );
              }
              distMatrix(j,k) = pow ( sum, ( 1/(minkoOrderVector(j)) ) );
            // Store the distance for the two points in the apropriate location in the distMatrix
                  
      }// end j
  }// end k

 NumericMatrix prefMatrix(idealsMatrix.nrow(),altsMatrix.nrow());

  for(int m =0; m< idealsMatrix.nrow(); ++m){
    NumericVector oneRowOfDistsRcpp = distMatrix(m,_);
     std::vector<double> oneRowOfDistsSTL = as< std::vector<double> >(oneRowOfDistsRcpp);
    // Use the rank() function from rank.h
    std::vector<double> prefOrderSTL(altsMatrix.nrow());
    rank(oneRowOfDistsSTL, prefOrderSTL, "average");
    NumericVector prefOrderRcpp(altsMatrix.nrow()); 
    prefOrderRcpp = prefOrderSTL;
    prefMatrix(m,_) = prefOrderRcpp;
  }

return prefMatrix;
}

