#pragma once

#include "../object.hpp"
#include "prim_val.hpp"

namespace harkon {

struct pim_val_converter: boost::static_visitor<object> {

    object operator()(const int x) const {
        return x;
    }

    object operator()(std::vector<char> const& symb) const {

        unsigned len = symb.size();

        if (len == 0)
            return symbol("", 0);
        else
            return symbol(symbol::MakeCopy(), &symb[0], len);
    }

    object operator()(std::vector<prim_val> const& dv) const {

        object_list o;

        for (std::vector<prim_val>::const_reverse_iterator it = dv.rbegin(); it != dv.rend(); ++it) {
           o = o.new_push_front(boost::apply_visitor(pim_val_converter(), *it));
        }

        return o;
    }

    object operator()(std::string const& str) const {
        return string(string::MakeCopy(), str.c_str(), str.size());
    }

};

object object_from_prim_val(prim_val const& pv) {

    return boost::apply_visitor(pim_val_converter(), pv);
}

}

