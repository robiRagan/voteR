#include "k_intersection.h"

// STL headers
#include <list>
#include <set>
#include <iostream>

// Boost headers
#include <boost/unordered_map.hpp>

//#define ENABLE_DEBUG_TRACE
#ifdef ENABLE_DEBUG_TRACE
#define DEBUG_TRACE(x) std::cout << x
#else
#define DEBUG_TRACE(x) {}
#endif

namespace {

inline Polygon2D& polygonRef(Polygon2D& polygon) noexcept
{
	return polygon;
}

inline Polygon2D& polygonRef(std::pair<const std::set<std::size_t>, Polygon2D>& kvp) noexcept
{
	return kvp.second;
}

template<class Container>
static std::list<Polygon2D> simplifyPolygons(Container& c, const double epsilon)
{
	std::list<Polygon2D> result;
	if (epsilon > 0.0) {
		for (auto& e: c) {
			Polygon2D simplifiedPolygon;
			auto& polygon = polygonRef(e);
			boost::geometry::simplify(polygon, simplifiedPolygon, epsilon);
			result.push_back(std::move(simplifiedPolygon));
		}
	} else {
		for (auto& e: c) {
			result.push_back(std::move(polygonRef(e)));
		}
	}
	return result;
}

static void mergePolygonsPass(std::list<Polygon2D>& c, const double epsilon)
{
	DEBUG_TRACE("Mergepass (" << c.size() << " items)" << std::endl);

	// Enumerate remaining polygons
	auto unionsEnd = ++c.begin(); 
	auto itp = unionsEnd;
	while (itp != c.end()) {
		
		// Try to merge current polygon with known unions 
		auto& polygon = *itp;
		bool merged = false;

		for (auto itu = c.begin(); itu != unionsEnd; ++itu) {
			auto& union_ = *itu;
			
			DEBUG_TRACE("Merging " << std::endl << '\t'
				<< boost::geometry::wkt(polygon) 
				<< std::endl << "with union" << std::endl
				<< '\t' << boost::geometry::wkt(union_) << std::endl);

			const auto sizeBeforeMerge = c.size();
			boost::geometry::union_(*itu, polygon, c);
			const auto deltaSize = c.size() - sizeBeforeMerge;

			if (deltaSize == 1) {
				// Polygon merged - simpilfy it, update union and move forward
				// to a next polygon.
				DEBUG_TRACE("Merged into" << std::endl 
					<< '\t' << boost::geometry::wkt(c.back()) << std::endl);

				Polygon2D simplifiedPolygon;
				boost::geometry::simplify(c.back(), simplifiedPolygon, epsilon);
				std::swap(union_, simplifiedPolygon);

				c.pop_back();
				merged = true;
				++itp;
				break;
			} else if (deltaSize == 2) {
				// Polygon not merged - dismiss merge results
				c.pop_back();
				c.pop_back();
			} else if(deltaSize > 2) {
				throw std::logic_error("Unexpected result of polygon union");
			}
		}

		if (!merged) {
			// Polygon could not be merged with any of present unions.
			// Therefore the only option now is to add it as a separate union.
			if (itp != unionsEnd) {
				unionsEnd = c.insert(unionsEnd, Polygon2D());
				std::swap(*unionsEnd, *itp);
				itp = c.erase(itp);
			} else {
				++itp;
			}
			++unionsEnd;
		}
	}

	// Retain only unions
	c.erase(unionsEnd, c.end());
}

template<class Container>
static std::vector<Polygon2D> simplifyAndMergePolygons(
	Container& c, const double epsilon)
{
	auto unions = simplifyPolygons(c, epsilon);
	while(true) {
		const auto sizeBeforeMerge = unions.size();
		mergePolygonsPass(unions, epsilon);
		if (unions.size() == sizeBeforeMerge) break;
	}

	// Move to resulting container
	std::vector<Polygon2D> result;
	if (!unions.empty()) {
		result.reserve(unions.size());
		for (auto& u: unions) {
			result.push_back(std::move(u));
		}
	}

	return result;
}


} // namespace

std::vector<Polygon2D> convexKIntersection(const std::vector<Polygon2D>& polygons,
	const std::size_t k, const double epsilon)
{
	// k == 0 is illegal
	if (k == 0) {
		throw std::invalid_argument("Zero-intersection is illegal");
	}

	// no polygons - illegal case
	if (polygons.empty()) {
		throw std::invalid_argument("No polygons");
	}

	// negative epsilon - illegal case
	if (epsilon < 0.0) {
		throw std::invalid_argument("Negative epsilon is not allowed");
	}

	// one-intrsection - just merge original set of polygons
	if (k == 1) {
		auto polygonsCopy = polygons;
		return simplifyAndMergePolygons(polygonsCopy, epsilon);		
	}

	std::vector<Polygon2D> result;
	
	// If k is more than number of polygons, k-intersection is naturally impossible.
	const auto n = polygons.size();
	if (k > n) {
		return result;
	} else if (k == 2) {
		// Memory-optimized variant for k = 2
		for (std::size_t i = 0; i < n; ++i) {
			for (std::size_t j = i + 1; j < n; ++j) {
				const auto initialResultSize = result.size();

				// Obtain new intersection
				boost::geometry::intersection(polygons[i], polygons[j], result);

				// Skip empty intersection
				const auto resultSizeIncrement = result.size() - initialResultSize;
				if (resultSizeIncrement == 0) continue;

				// Report error on the non-convex polygons
				// (intersection of two convex polygons is ether empty or another convex polygon)
				if (resultSizeIncrement != 1)
					throw std::invalid_argument("There is non-convex polygon");
			}
		}

		if (result.size() > 1) {
			return simplifyAndMergePolygons(result, epsilon);
		}
	} else {
		// General algorithm for K > 2
		boost::unordered_map<std::set<std::size_t>, Polygon2D> a, b;
		for (std::size_t i = 0; i < n; ++i)
			a.emplace(std::set<std::size_t> { i }, polygons[i]);

		{
			std::list<Polygon2D> output;
			for (std::size_t i = 1; i < k && !a.empty(); ++i) {
				for (const auto& intersection : a) {
					for (std::size_t j = 0; j < n; ++j) {
						// Skip this polygon if it is already part of this intersection
						if (intersection.first.count(j) > 0) continue;

						// Skip candidate intersection if it is already known
						auto key = intersection.first;
						key.insert(j);
						if (b.count(key) > 0) continue;

						// Obtain new intersection
						output.clear();
						boost::geometry::intersection(intersection.second, polygons[j], output);

						// Skip empty intersection
						if (output.empty()) continue;

						// Intersection of two convex polygons is either empty or another convex polygon.
						// So if we get more than one polygon in the intersection this means
						// there are some non-convex polygons, which is error.
						if (output.size() != 1)
							throw std::invalid_argument("There is non-convex polygon");

						// Store new intersection
						b.emplace(std::move(key), std::move(output.back()));
					}
				}

				// Prepare for next iteration
				a.swap(b);
				b.clear();
			}
		}

		if (!a.empty()) {
			if (a.size() > 1) {
				return simplifyAndMergePolygons(a, epsilon);
			}
			else {
				result.push_back(std::move(a.begin()->second));
			}
		}
	}

	return result;
}
