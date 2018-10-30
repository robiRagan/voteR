#pragma once

#ifndef K_INTERSECTION_H__
#define K_INTERSECTION_H__

// STL headers
#include <vector>

// Boost headers
#include <boost/geometry.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/polygon.hpp>

// Polygon type
typedef boost::geometry::model::polygon<boost::geometry::model::d2::point_xy<double>> Polygon2D;

// Finds k-intersection of the given polygons. 
std::vector<Polygon2D> convexKIntersection(const std::vector<Polygon2D>& polygons,
	const std::size_t k, const double epsilon);

#endif // K_INTERSECTION_H__
