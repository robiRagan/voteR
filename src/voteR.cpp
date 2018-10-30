// Set up non-intrusive extension of as<>() and wrap() via template specialisation in int/include/voteR.h and include below. 
// Here is where you write the template specialization for as<>() and wrap()

#include "../inst/include/voteR.h"
#include <Rcpp.h>
#include <math.h>

    namespace Rcpp {
        
        
     // These are the as<>() and wrap() converters. The template extensions are necessary forward declarations and can be found in "inst/include/voteR.h""
        
        ////////////////////////
        ///// as<>() converters.
        ////////////////////////
        
        
        
         // R Matrix to boost:polygon
         template <> boostPolygon as(SEXP aSexpMatrix) {
            NumericMatrix aRcppMatrix(aSexpMatrix);
            boostPolygon aBoostPolygon;
        
            for (int i = 0; i < aRcppMatrix.nrow(); ++i) {
                double x = aRcppMatrix(i,0);
                double y = aRcppMatrix(i,1);
                boostTuple aTempRow(x,y);
    
                aBoostPolygon.outer().push_back(aTempRow); 
                }
        return (aBoostPolygon);
        } 
        
        // R Vector to boost:tuple
        template <> boostTuple as(SEXP aSexpVector) {
  
        NumericVector aRcppVector(aSexpVector);
  
        double a = aRcppVector(0);
        double b = aRcppVector(1);
  
        boostTuple aBoostTuple = boost::make_tuple(a, b);
  
    return (aBoostTuple);
    }
        
        
        
        // R Matrix to ublas matrix
        template <> ublasMatrix as(SEXP aSexpMatrix) {
            NumericMatrix aRcppMatrix(aSexpMatrix);
  
            unsigned int rows = aRcppMatrix.nrow();
            unsigned int cols = aRcppMatrix.ncol();
  
            ublasMatrix aUblassMatrix(rows, cols);
  
            for (unsigned int i = 0; i < rows; ++i) {
                for (unsigned int j = 0; j < cols; ++j) {
                    double x = aUblassMatrix(i, j);
                    aUblassMatrix(i, j) = x;                        
                }
            }
            return (aUblassMatrix);
        }
        
        
        // R vector to ublas vector
        template <> ublasVector as(SEXP aSexpVector) {
            NumericVector aRcppVector(aSexpVector);
            ublasVector aUblasVector(aRcppVector.size());
  
            for (int i = 0; i < aRcppVector.size(); ++i) {
                double x = aRcppVector(i);
                aUblasVector(i) = x; 
            }
            return (aUblasVector);
        }
        
            
        
     ////////////////////////   
    ///// wrap() converters.
    ////////////////////////
    
     // boost:polygon to Rcpp:NumericMatrix
        template <> SEXP wrap(const boostPolygon& aBoostPolygon) {
            
            const std::vector<boostTuple>& points = aBoostPolygon.outer();
            NumericMatrix aRcppMatrix(points.size(), 2);

            for(int i = 0; i < points.size(); ++i) {
                const boostTuple& aTempRow = points[i];
                aRcppMatrix(i,0) = aTempRow.get<0>();
                aRcppMatrix(i,1) = aTempRow.get<1>();
            }
        return Rcpp::wrap(aRcppMatrix);
        }
        
        
    // boost:tuple to Rcpp:NumericVector
        template <> SEXP wrap(const boostTuple& aBoostTuple) {
        NumericVector aRcppVector(2);
  
        aRcppVector(0) = aBoostTuple.get<0>();
        aRcppVector(1) = aBoostTuple.get<1>();
  
  return Rcpp::wrap(aRcppVector);
}
        
        
        
    // ublas matrix to Rccp:NumericMatrix    
    template <> SEXP wrap(const ublasMatrix& aUblasMatrix) {
        unsigned int rows = aUblasMatrix.size1();
        unsigned int cols = aUblasMatrix.size2();
        
        NumericMatrix aRcppMatrix(rows, cols);
        
        for (unsigned int i = 0; i < rows; ++i) {
            for (unsigned int j = 0; j < cols; ++j) {
            const double aTempElement = aRcppMatrix(i, j);
            aRcppMatrix(i, j) = aTempElement;
            }
        }
  return Rcpp::wrap(aRcppMatrix);
}    
        
     // ublas matrix to Rccp:NumericMatrix 
    template <> SEXP wrap(const ublasVector& aUblasVector) {

    NumericVector aRcppVector(aUblasVector.size());
  
        for(int i = 0; i < aUblasVector.size(); ++i) {
        const double aTempElement = aUblasVector[i];
        aRcppVector(i) = aTempElement;
        }
  return Rcpp::wrap(aRcppVector);
    
}
        
        
    
    } // This ends the R Namespace

