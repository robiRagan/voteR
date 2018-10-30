#include <fstream>
#include <iostream>
#include <sstream>
#include "k_intersection.h"

int main(int argc, char** argv)
{
	if (argc != 5)
	{
		std::cerr << "Wrong number of arguments." << std::endl;
		std::cerr << "Usage: " << argv[0] << " K_VALUE EPSILON INPUT_FILE OUTPUT_FILE" << std::endl;
		return 1;
	}

	try
	{
		std::size_t argIndex = 1;

		// Parse K
		std::size_t k;
		{
			int n = std::stoi(argv[argIndex]);
			if (n < 1) {
				throw std::runtime_error("Invalid k value");
			}
			k = n;
		}

		// Parse EPSILON
		++argIndex;
		auto epsilon = std::stod(argv[argIndex]);
		if (epsilon < 0.0) {
			throw std::runtime_error("Invalid EPSILON value");
		}

		// Open input file
		++argIndex;
		std::ifstream ifs(argv[argIndex]);
		if (!ifs.is_open()) {
			throw std::runtime_error("Can't open input file for reading");
		}

		// Read number of input polygons
		std::string s;
		if (!std::getline(ifs, s)) {
			throw std::runtime_error("Input file format error: missing number of polygons");
		}

		// Parse number of input polygons
		std::size_t polygonCount;
		{
			int n = std::stoi(s);
			if (n < 2)
				throw std::runtime_error("Invalid number of polygons");
			polygonCount = n;
		}

		// Read and parse input polygons
		std::vector<Polygon2D> inputPolygons(polygonCount);
		for (std::size_t i = 0; i < polygonCount; ++i) {
			if (!std::getline(ifs, s)) {
				throw std::runtime_error("Input file format error: missing subsequnt"
					" polygon definition");
			}
			boost::geometry::read_wkt(s, inputPolygons[i]);
		}

		ifs.close();

		// Make intersection
		auto outputPolygons = convexKIntersection(inputPolygons, k, epsilon);

		// Open output file
		++argIndex;
		std::ofstream ofs(argv[argIndex]);
		if (!ofs.is_open()) {
			throw std::runtime_error("Can't open output file for writing");
		}

		// Write number of output polygons
		ofs << outputPolygons.size() << std::endl;

		// Write output polygons
		for (const auto& polygon : outputPolygons) {
#ifdef _DEBUG
			std::ostringstream str;
			str << boost::geometry::wkt(polygon);
			auto s = str.str();
			ofs << s << std::endl;
			std::cout << s << std::endl;
#else
			ofs << boost::geometry::wkt(polygon) << std::endl;
#endif // _DEBUG
		}

		ofs.close();

#ifdef _DEBUG
		std::cout << outputPolygons.size() << " polygon(s) in the intersection." << std::endl;
#endif // _DEBUG
	} catch (std::exception& ex) {
		std::cerr << "Error: " << ex.what() << std::endl;
		return 2;
	}

	return 0;
}
