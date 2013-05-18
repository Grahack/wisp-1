#pragma once

// some code that i wrote, that may or may not become useful in the future

#include "alloc.hpp"
#include "object.hpp"

#include <boost/functional/hash.hpp>


namespace harkon {

// hmm is this even useful?
template<class T>
class gc_alloc {
public:
    // type definitions
    typedef T value_type;
    typedef T* pointer;
    typedef const T* const_pointer;
    typedef T& reference;
    typedef const T& const_reference;
    typedef std::size_t size_type;
    typedef std::ptrdiff_t difference_type;

    // rebind allocator to type U
    template<class U>
    struct rebind {
        typedef gc_alloc<U> other;
    };

    // return address of values
    pointer address(reference value) const {
        return &value;
    }
    const_pointer address(const_reference value) const {
        return &value;
    }

    /* constructors and destructor
     * - nothing to do because the allocator has no state
     */
    gc_alloc() throw () {
    }
    gc_alloc(const gc_alloc&) throw () {
    }
    template<class U>
    gc_alloc(const gc_alloc<U>&) throw () {
    }
    ~gc_alloc() throw () {
    }

    // return maximum number of elements that can be allocated
    size_type max_size() const throw () {
        return std::numeric_limits<std::size_t>::max() / sizeof(T);
    }

    // allocate but don't initialize num elements of type T
    pointer allocate(size_type num, const void* = 0) {
        return (pointer) (GC_ALLOC(num * sizeof(T)));
    }

    // initialize elements of allocated storage p with value value
    void construct(pointer p, const T& value) {
        // initialize memory with placement new
        new ((void*) p) T(value);
    }

    // destroy elements of initialized storage p
    void destroy(pointer p) {
        p->~T();
    }

    // deallocate storage p of deleted elements
    void deallocate(pointer p, size_type num) {
    }
};

// return that all specializations of this allocator are interchangeable
template<class T1, class T2>
bool operator==(const gc_alloc<T1>&, const gc_alloc<T2>&) throw () {
    return true;
}
template<class T1, class T2>
bool operator!=(const gc_alloc<T1>&, const gc_alloc<T2>&) throw () {
    return false;
}

// TODO: should this be seeded?
struct hash_value_visitor: boost::static_visitor<std::size_t> {
    std::size_t operator()(bool b) const {
        return boost::hash_value(b);
    }
    std::size_t operator()(char c) const {
        return boost::hash_value(c);
    }
    std::size_t operator()(int i) const {
        return boost::hash_value(i);
    }
    std::size_t operator()(nil n) const {
        return hash_value(n);
    }
    std::size_t operator()(symbol const& s) const {
        return hash_value(s);
    }
    std::size_t operator()(string const& ps) {
        return hash_value(ps);
    }
    std::size_t operator()(object_list const& pl) const {
        return hash_value(pl);
    }
};

inline std::size_t hash_value(object const& o) {
    hash_value_visitor visitor;
    return boost::apply_visitor(visitor, o);
}


inline std::size_t hash_value(symbol const& s) {
    std::size_t seed = 0x3BB65E2A; // random arbitrary number
    boost::hash_combine(seed, hash_value(static_cast<persistent::string>(s)));
    return seed;
}

inline std::size_t hash_value(string const& s) {
    std::size_t seed = 0xEF13F20E; // random arbitrary number
    boost::hash_combine(seed, hash_value(static_cast<persistent::string>(s)));
    return seed;
}

inline std::size_t hash_value(object_list const& ol) {
    std::size_t seed = 0x4AA3BC2E;

    for (object_list::const_iterator it(ol.begin()); it != ol.end(); ++it) {
        boost::hash_combine(seed, hash_value(*it));
    }

    return seed;
}


inline std::size_t hash_value(nil) {
    return 0x0DD63634;
}



}


