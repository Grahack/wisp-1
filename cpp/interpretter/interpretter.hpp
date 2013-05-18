#pragma once
#include <stdexcept>

#include "../object.hpp"
#include "../persistent/map.hpp"
#include <boost/bind.hpp>

namespace harkon {

object eval(object const& o, environment & env);

template<typename T>
T expect_as(object const& o) {
    T const* v = boost::get<T>(&o);
    if (v == NULL) {
        throw std::runtime_error("Unexpected " + pretty_print(o) + " was found"); // TODO: lol...
    } else {
        return *v;
    }
}

inline object builtin_add(persistent::list<object> const& args, environment & env) {
    assert(!args.empty());

    int cum = 0;

    for (persistent::list<object>::const_iterator it(args.begin() + 1); it != args.end(); ++it) {
        cum += expect_as<int>(eval(*it, env));
    }

    return cum;
}

inline object builtin_def(persistent::list<object> const& args, environment & env) {
    assert(!args.empty());

    if (args.size() != 3)
        throw std::runtime_error("__builtin_def expected 2 args");

    persistent::list<object>::const_iterator it(args.begin() + 1);
    assert(it != args.end());

    symbol s = expect_as<symbol>(*it);

    ++it;
    assert(it != args.end());

    env.insert(s, eval(*it, env));

    return nil();
}

inline object eval_lambda(environment & captured_env, persistent::list<object> const& lambda,
        persistent::list<object> const& args, environment & env) {

    assert(!args.empty());
    assert(!lambda.empty());

    environment captured_copy = env;
    // captured_copy.insert("recur", object_proc(boost::bind(&eval_lambda, captured_env, lambda, _1, _2)));
    //captured_copy.merge(captured_env);

    persistent::list<object>::const_iterator lit(lambda.begin() + 1);

    object_list lambda_arg_names = expect_as<object_list>(*lit);

    persistent::list<object>::const_iterator vit(args.begin());

    for (object_list::const_iterator arg_it(lambda_arg_names.begin()); arg_it != lambda_arg_names.end() ; ++arg_it) {
        ++vit;
        if (vit == args.end()) {
            throw std::runtime_error("Too few arguments provided when eval lambda result");
        }
        symbol arg = expect_as<symbol>(*arg_it);

        captured_copy.insert(arg, eval(*vit, env));
    }

    ++lit;
    if (lit == lambda.end()) {
        throw std::runtime_error("Error, no body in evaluated lambda");
    }

    object r = eval(*lit, captured_copy);
    // std::cout << "Func result: " << pretty_print(r) << std::endl;
    return r;
}

inline object builtin_lambda(persistent::list<object> const& args, environment & env) {
    assert(!args.empty());

    if (args.size() != 3)
        throw std::runtime_error("__builtin_lambda expected 2 args");

    return object_proc(boost::bind(&eval_lambda, env, args, _1, _2));
}

inline object builtin_eq(persistent::list<object> const& args, environment & env) {
    assert(!args.empty());

    if (args.size() < 3)
        throw std::runtime_error("__builtin_eq needs at least 2 args");

    for (persistent::list<object>::const_iterator it(args.begin() + 1); (it+1) != args.end(); ++it) {
        if (eval(*it, env) != eval(*(it + 1), env))
            return boolean(false);
    }
    return boolean(true);
}

inline object builtin_if(persistent::list<object> const& args, environment & env) {
    assert(!args.empty());

    if (args.size() != 4)
        throw std::runtime_error("__builtin_if expected 3 args");

    persistent::list<object>::const_iterator it(args.begin() + 1);
    assert(it != args.end());

    boolean b = expect_as<boolean>(eval(*it, env));

    ++it;
    assert(it != args.end());

    if (!b.as_bool()) {
        // advance to the false case
        ++it;
        assert(it != args.end());
    }

    return eval(*it, env);
}

inline environment create_new_environment() {
    return environment().new_insert("author", string("Eric Springer")).
            new_insert("#t", boolean(true)).
            new_insert("#f", boolean(false)).
            new_insert("add", object_proc(&builtin_add)).
            new_insert("def", object_proc(&builtin_def)).
            new_insert("if", object_proc(&builtin_if)).
            new_insert("lambda", object_proc(&builtin_lambda)).
            new_insert("eq", object_proc(&builtin_eq));
}

struct eval_visitor: boost::static_visitor<object> {
    eval_visitor(environment & env) :
            env(env) {
    }

    object operator()(boolean b) {
        return b;
    }
    object operator()(char c) {
        return c;
    }
    object operator()(int i) {
        return i;
    }
    object operator()(nil) {
        return nil();
    }
    object operator()(symbol const& s) {
        object const* resolved = env.find(s);
        if (resolved == NULL)
            throw std::runtime_error(std::string("Unable to resolve symbol: ") + s.c_str());

        return *resolved;
    }
    object operator()(string const& s) {
        return s;
    }
    object operator()(persistent::list<object> const& pl) {

        if (pl.empty()) {
            throw std::runtime_error("Invalid to try evaluate an empty list");
        }

        object_proc proc = expect_as<object_proc>(eval(*pl.begin(), env));

        return proc(pl, env);
    }

    object operator()(object_proc const& proc) {
        return string("<function call>");
    }

    environment & env;
};

object eval(object const& o, environment & env) {

    eval_visitor ev(env);
    return boost::apply_visitor(ev, o);
}

}
