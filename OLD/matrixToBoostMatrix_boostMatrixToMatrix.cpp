#include <Rcpp.h>
#include "../inst/include/voteR.h"

using namespace Rcpp;

//#include <boost/numeric/ublas/matrix.hpp>
//
//typedef boost::numeric::ublas::matrix<double> matrix;
//
//namespace Rcpp {
//template <> matrix as(SEXP RMatrixSEXP) {
//  NumericMatrix RMatrix(RMatrixSEXP);
//  
//  unsigned int rows = RMatrix.nrow();
//  unsigned int cols = RMatrix.ncol();
//  
//  matrix m(rows, cols);
//  
//  for (unsigned int i = 0; i < rows; ++i) {
//    for (unsigned int j = 0; j < cols; ++j) {
//      double x = RMatrix(i, j);
//      m(i, j) = x;
//    }
//  }
//  return (m);
//}
//
//template <> SEXP wrap(const matrix& m) {
//  unsigned int rows = m.size1();
//  unsigned int cols = m.size2();
//  
//  NumericMatrix RMatrix(rows, cols);
//  
//  for (unsigned int i = 0; i < rows; ++i) {
//    for (unsigned int j = 0; j < cols; ++j) {
//      const double p = m(i, j);
//      RMatrix(i, j) = p;
//    }
//  }
//  
//  return Rcpp::wrap(RMatrix);
//}
//} // namespace Rcpp

//' @export
// [[Rcpp::export]]
NumericVector testAsWrapMM(SEXP RVectorSEXP) {
  ublasMatrix v = as<ublasMatrix>(RVectorSEXP);
  return wrap(v);
}
