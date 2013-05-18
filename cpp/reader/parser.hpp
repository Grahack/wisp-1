#pragma once

#include <boost/variant.hpp>

#include "../object.hpp"

namespace harkon {

typedef std::string error_string;

struct reader_exception: public std::exception {
	reader_exception(std::string const& error) :
			msg(error) {
	}

	virtual ~reader_exception() throw () {
	}

	virtual char const* what() const throw () {
		return msg.c_str();
	}
private:
	std::string msg;
};

object parse(std::string const& str);

}

