#pragma once

#include <string>
#include <vector>
#include <sstream>
#include <boost/function.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/static_assert.hpp>

#include "persistent/string.hpp"
#include "persistent/list.hpp"
#include "persistent/map.hpp"

#include <boost/functional/hash.hpp>
#include <boost/variant.hpp>
#include <boost/variant/recursive_wrapper.hpp>

namespace harkon {

struct symbol: persistent::string {
    symbol(char const* c_str, unsigned len) :
            persistent::string(c_str, len) {
    }
    symbol(char const* c_str) :
            persistent::string(c_str) {
    }
    symbol(symbol::MakeCopy, char const* c_str, unsigned len) :
            persistent::string(persistent::string::MakeCopy(), c_str, len) {
    }
};

struct string: persistent::string {
    string(char const* c_str, unsigned len) :
            persistent::string(c_str, len) {
    }
    string(char const* c_str) :
            persistent::string(c_str) {
    }
    string(string::MakeCopy, char const* c_str, unsigned len) :
            persistent::string(persistent::string::MakeCopy(), c_str, len) {
    }
};

struct boolean {
    explicit boolean(bool v) :
            v(v) {
    }
    bool as_bool() const {
        return v;
    }

    bool operator==(boolean other) const {
        return as_bool() == other.as_bool();
    }
private:
    bool v;
};

struct nil {
    bool operator==(nil other) const {
        return true;
    }
};

struct object_list;
struct object_proc;

typedef boost::variant<boolean, char, int, nil, symbol, string, boost::recursive_wrapper<object_list>,
        boost::recursive_wrapper<object_proc> > object;

inline std::size_t hash_value(symbol const& symb) {
    return boost::hash_range(symb.c_str(), symb.c_str() + symb.size());
}

std::string pretty_print(object const& o);

typedef persistent::map<symbol, object> environment;

struct object_list: persistent::list<object> {
    object_list(persistent::list<object> const& base) :
            persistent::list<object>(base) {
    } // automatic conversion
    object_list() :
            persistent::list<object>() {
    } // unshadow
    bool operator==(object_list const& other) const {
        std::cout << "Warning: object_list comparison not implemented yet!" << std::endl; // TODO: implement..
        return false;
    }
};

typedef boost::function<object(object_list args, environment & env)> object_proc_func;

struct object_proc: object_proc_func {
    object_proc(object_proc_func f) :
            object_proc_func(f) {
    }
    bool operator==(object_proc const& other) const {
        std::cout << "Warning: object_proc comparison not implemented yet!" << std::endl; // TODO: implement..
        return false;
    }
};

struct pretty_print_visitor: boost::static_visitor<std::string> {
    std::string operator()(boolean b) const {
        if (b.as_bool())
            return "#t";
        else
            return "#f";
    }
    std::string operator()(char) const {
        return "<char>";
    }
    std::string operator()(int i) const {
        return boost::lexical_cast<std::string>(i);
    }
    std::string operator()(nil) const {
        return "NIL";
    }
    std::string operator()(symbol const& s) const {
        return s.c_str(); // TODO: escaping..
    }
    std::string operator()(string const& ps) {
        return std::string("\"") + ps.c_str() + "\""; // TODO: escaping
    }
    std::string operator()(object_list const& pl) const {
        std::stringstream buff;
        buff << "(";
        for (object_list::const_iterator it(pl.begin()); it != pl.end(); ++it) {
            buff << pretty_print(*it);
            if (it + 1 != pl.end()) {
                buff << " ";
            }
        }
        buff << ")";
        return buff.str();
    }

    std::string operator()(object_proc const& proc) const {
        return "<PROC>";
    }

};

inline std::string pretty_print(object const& o) {
    pretty_print_visitor visitor;
    return boost::apply_visitor(visitor, o);
}


inline bool operator!=(object const& a, object const& b) {
    return !(a == b);
}

}
