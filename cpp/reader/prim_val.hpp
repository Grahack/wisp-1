#pragma once
#include <vector>
#include <boost/variant.hpp>
#include <boost/variant/recursive_variant.hpp>
#include <boost/variant/recursive_wrapper.hpp>

#include <map>


namespace harkon {

typedef boost::make_recursive_variant<int, std::vector<char>, std::string, std::vector<boost::recursive_variant_> >::type prim_val;


}
