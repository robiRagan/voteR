// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include "../inst/include/voteR.h"
using namespace Rcpp;
//' sgn
//' 
//' Returns the signum of the supplied arguement
//' 
//' @param aScalar
//'   
//' @return Sign of supplied scalar.
//' @export
//' 
// [[Rcpp::export]]
int sgn(float aScalar){
    
    int theSign = (aScalar > 0) - (aScalar < 0);
    
return theSign; 
}
