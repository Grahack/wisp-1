#include <iostream>

#include "reader/parser.hpp"
#include "interpretter/interpretter.hpp"

int main(int, char**) {

	std::cout << "Welcome to Harkon. :exit to quit\n\n";

	std::string in;

	std::cout << "~> ";

	harkon::environment env = harkon::create_new_environment();


	while (std::getline(std::cin, in)) {

		if (in == ":exit") break;

		try {
			harkon::object r = harkon::parse(in);

			//std::cout << "Parsed: " << harkon::pretty_print(r) << std::endl;
			std::cout << harkon::pretty_print(harkon::eval(r, env)) << std::endl;


		} catch (std::exception const& ex) {
			std:: cout << "Caught exception: " << ex.what() << std::endl;
		}

		std::cout << "~> ";
	}

	return 0;

}
