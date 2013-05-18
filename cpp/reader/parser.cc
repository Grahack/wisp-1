#include <boost/spirit/include/qi.hpp>

#include "parser.hpp"
#include "conversion.hpp"

namespace harkon {

using boost::spirit::qi::rule;
using boost::spirit::qi::grammar;
using boost::spirit::qi::space;
using boost::spirit::qi::char_;
using boost::spirit::qi::alnum;
using boost::spirit::qi::int_;
using boost::spirit::qi::symbols;
using boost::spirit::qi::lit;
using boost::spirit::qi::hex;
using boost::spirit::qi::eol;
using boost::spirit::qi::lexeme;

template<typename Iterator, typename Skipper>
struct sexpr_grammar: grammar<Iterator, Skipper, std::vector<prim_val>()> {
	sexpr_grammar() :
			sexpr_grammar::base_type(start_) {

		esc_char_.add("\\a", '\a')("\\b", '\b')("\\f", '\f')("\\n", '\n')("\\r", '\r')("\\t", '\t')("\\v", '\v')("\\\\",
				'\\')("\\\'", '\'')("\\\"", '\"');

		esc_str_ = '"' >> *(esc_char_ | "\\x" >> hex | (char_ - '"')) >> '"';

		object_ %= list_ | int_ | esc_str_ | symbol_;
		symbol_ %= lexeme[+(char_ - space - ')' - '(')];
		list_ %= '(' >> *object_ >> ')';

		start_ %= +object_;
	}
	rule<Iterator, Skipper, std::vector<prim_val>()> start_;
	rule<Iterator, Skipper, prim_val()> object_;
	rule<Iterator, Skipper, std::vector<char>()> symbol_;
	rule<Iterator, Skipper, std::vector<prim_val>()> list_;
	rule<Iterator, std::string()> esc_str_;
	symbols<char const, char const> esc_char_;
};

template<typename Iterator, typename Skipper, typename Attr>
inline bool harkon_skipper_parse(Iterator& first, Iterator last, Skipper skipper, Attr& attr) {
	sexpr_grammar<Iterator, Skipper> p;
	return boost::spirit::qi::phrase_parse(first, last, p, skipper, attr);
}

template<typename Iterator, typename Attr>
inline bool harkon_parse(Iterator& first, Iterator last, Attr& attr) {
	return harkon_skipper_parse(first, last, (space | (';' >> *(char_ - eol) >> eol)), attr);
}


object parse(std::string const& str) {

	std::string::const_iterator iter = str.begin();
	std::string::const_iterator end = str.end();

	std::vector<prim_val> result;

	bool r = harkon::harkon_parse(iter, end, result);

	if (r) {
		if (result.size() != 1) {
			std::cout << "Warning: parsed " << result.size() << " forms. This isn't yet supported! Truncating to only use the first." << std::endl;
		}
	} else {
		throw reader_exception("Parse error. Stopped at: " + std::string(iter, end));
	}

    return object_from_prim_val(result.front());
}

}
