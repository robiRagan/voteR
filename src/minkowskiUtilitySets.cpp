// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include "../inst/include/voteR.h"
using namespace Rcpp;
//' minkowskiUtilitySets
//' 
//' Calculates the sailence weighted Minkowski utility for a set of voters and 
//' a set of alternatives in an 2-dimensional space.
//' 
//' This function will find the salience weighted Minkowski Utility for a set
//' of voters whose ideal points are stacked in a numberVoters x
//' numberDimensions matrix and a set of alternatives that are stacked into a
//' numberAlternatives x numberDimensions matrix. Each voter can have a
//' different 'order' for their Minkowski Distance, a set of `sailence`
//' weights for each policy dimension and a loss order. Each voter can have a
//' different loss order for their loss function. 
//' 
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
//'   faster than in the linear case. 
//'   
//' @return utilMatrix A numVoters x numAlts matrix that contains the Minkowski
//'   Utility between each voter and each alternative, given a voter's salience, 
//'   Minkowski order and loss function. The sailence, Minkowski order and loss function
//'   together determine a voter's utility function.
//' @family minkowski
//' @family utility functions
//' @export
//' 
// [[Rcpp::export]]
NumericMatrix minkowskiUtilitySets(NumericMatrix idealsMatrix, NumericMatrix altsMatrix, NumericVector minkoOrderVector, NumericVector lossOrderVector, NumericMatrix salienceMatrix){
  
 NumericMatrix utilMatrix(idealsMatrix.nrow(),altsMatrix.nrow()); // The container to put the distances in


  for(int k =0; k< altsMatrix.nrow(); ++k){ // This loops over the second set of points, usually the smaller set.
  
      for(int j = 0; j < idealsMatrix.nrow(); ++j) {// This loops over the first set of points, usually the larger set.
        
              // Calculates the Minkowski distance for two points
              long double sum=0;
              long double weighted=0;
              for ( int i=0; i<idealsMatrix.ncol(); i++ ){
                  weighted = salienceMatrix(j,i) * fabs(idealsMatrix(j,i) - altsMatrix(k,i) );
                  sum += pow ( weighted, minkoOrderVector(j) );
              }
              utilMatrix(j,k) = - (pow ( pow ( sum, ( 1/(minkoOrderVector(j)) ) ), lossOrderVector(j) ) );
            // Apply the loss function to the distance for the two points and store the utlity in the apropriate location in the utilMatrix
                  
      }// end j
  }// end k

return utilMatrix;
}

//// THIS HAS BEEN TESTED AGAINST OTHER SOURCES AND WORKS!
