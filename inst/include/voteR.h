#ifndef __voteR_h__
#define __voteR_h__
#include "voteR_RcppExports.h"
#include <RcppCommon.h>

#include <boost/geometry.hpp>
#include <boost/geometry/geometries/adapted/boost_tuple.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/multi_point.hpp>
#include <boost/geometry/geometries/polygon.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/foreach.hpp>

#include <deque>

#include <boost/numeric/ublas/vector.hpp>




BOOST_GEOMETRY_REGISTER_BOOST_TUPLE_CS(cs::cartesian)

typedef boost::tuple<double, double> boostTuple;
typedef boost::geometry::model::polygon<boostTuple, true, true> boostPolygon;

typedef boost::numeric::ublas::vector<double> ublasVector;
typedef boost::numeric::ublas::matrix<double> ublasMatrix;

typedef boost::geometry::model::polygon<boost::geometry::model::d2::point_xy<double> > boostPolygon2;

    namespace Rcpp {
    
    // These as<>() and wrap() extensions are only the necessary forward declarations. The actual converters live in "src/voteR.cpp".
        
        /////Non-intrusive extensions of as<>() via template specialization.
        

           
           // R Matrix to boost:polygon
            template <> boostPolygon as(SEXP aSexpMatrix);
            
            // R vector to boost:tuple
            template <> boostTuple as(SEXP aSexpVector);
        
            // R Matrix to ublas matrix
            template <> ublasMatrix as(SEXP aSexpMatrix);
            
            // R Matrix to ublas vector
            template <> ublasVector as(SEXP aSexpVector);
            
            
        
        
        
         /////Non-intrusive extensions of wrap() via template specialization.
        
            // boost:polygon to Rcpp:NumericMatrix
            template <> SEXP wrap(const boostPolygon& aBoostPolygon);
        
            // boost:tuple to Rcpp:NumericVector
            template <> SEXP wrap(const boostTuple& aBoostTuple);
        
        
            // ublas matrix to Rccp:NumericMatrix
            template <> SEXP wrap(const ublasMatrix& aUblasMatrix);
        
            // ublas vector to Rccp:NumericMatrix
            template <> SEXP wrap(const ublasVector& aUblasVector);
            
            
            
    ///// Forward declarations for other functions: 
    
    

    }

#endif // __voteR_h__




    //typedef boost::geometry::model::d2::point_xy<double, double> Point;
    //typedef boost::geometry::model::multi_point<Point> MultiPoint;