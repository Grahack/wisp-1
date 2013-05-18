#pragma once
#include "../alloc.hpp"
#include <cstring>
#include <boost/functional/hash.hpp>

namespace persistent {

struct string {
    string(char const* c_str, unsigned size) :
            val(c_str), length(size) {
    }

    string(char const* c_str) :
            val(c_str), length(::strlen(c_str)) {
    }

    struct MakeCopy {
    };

    string(MakeCopy, char const* from, unsigned len) :
            val(alloc_and_copy(from, len)), length(len) {
    }

    string operator+(string const& other) const {
        char *s = (char*) GC_ALLOC(size() + other.size() + 1);
        ::memcpy(s, c_str(), size());
        ::memcpy(s + size(), other.c_str(), other.size() + 1); // copy the null terminator as well
        return string(s, size() + other.size());
    }

    unsigned size() const {
        return length;
    }

    char const* c_str() const {
        return val;
    }

    int compareTo(string const& other) const {
        return ::strcmp(c_str(), other.c_str());
    }

    bool operator==(string const& other) const {
        return ::strcmp(c_str(), other.c_str()) == 0;
    }

    bool operator!=(string const& other) const {
        return ::strcmp(c_str(), other.c_str()) != 0;
    }

    bool operator<(string const& other) const {
        return ::strcmp(c_str(), other.c_str()) < 0;
    }

    bool operator<=(string const& other) const {
        return ::strcmp(c_str(), other.c_str()) <= 0;
    }

    bool operator>(string const& other) const {
        return ::strcmp(c_str(), other.c_str()) > 0;
    }

    bool operator>=(string const& other) const {
        return ::strcmp(c_str(), other.c_str()) >= 0;
    }

private:
    static char const* alloc_and_copy(char const* source, unsigned len) {
        char* s = (char*) GC_ALLOC(len + 1);
        ::memcpy(s, source, len);
        s[len] = '\0';

        return s;
    }

    char const* val;
    const unsigned length;
};

}
