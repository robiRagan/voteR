// // [[Rcpp::interfaces(r, cpp)]]
// // [[Rcpp::plugins("cpp11")]]
// #include <Rcpp.h>
// #include <vector>
// // put any other includes you need here.
// 
// using namespace Rcpp;
// //' intersectPolygons
// //'
// //' Takes a set of 5 convex polygons from R and returns the intersection of each pair.
// //'
// //'
// //' @param listOfPolygons An R list with 3 to 5 convex polygons. Each polygon is one
// //'     element of the list and is stored as an R matrix. The x-coordinates for
// //'     the polygon are the first column, and the y-coordinates are the second
// //'     column.
// //'
// //'
// //' @return intersections A List where each element is the intersection of two of
// //'     the provided polygons. For example: If there are 5 polygons the list will contain 10 elements.
// //'     Each element should be NumericalMatrix with x-coordinates as the first
// //'     column and y-coordinates as the second column.
// //'
// //'     The elements of the list should be named based on the two polygons
// //'     that were intersected to find that particular element. For example
// //'     when listOfPolygons[[1]] and listOfPolygons[[2]] are intersected,
// //'     the resulting polygon should be stored in the list as "1_intersect_2".
// //'
// //' @export
// //'
// //[[Rcpp::export]]
// List findIntersections(List listOfPolygons){
// 
// 
// // Here is where you write the code that uses boost::geometry::intersection() to
// // intersect all possible pairs of the provided polygons.
// // The intersection of a pair ( example: listOfPolygons[[1]] and listOfPolygons[[2]] )
// // should be stored as a matrix, with the x-coorinates as the first column and the
// // y-coordinates as the second column (in the same way as the polygons that served as the inputs). 
// // The matrix should be stored in a list for output purposes. Each matrix in the list should
// // be named according to the two polygons that were interesected to generate the matrix. For example
// // when listOfPolygons[[1]] and listOfPolygons[[2]] are intersected, 
// // the resulting polygon should be stored in the list as "1_intersect_2". Or simply "1_2". 
// 
// List returnList;
// 
//     return returnList;
// }
