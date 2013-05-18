#pragma once
#include <cassert>
#include "../alloc.hpp"

namespace persistent {

template<typename T>
struct list {
    list() :
            first(NULL) {
    }
private:
    struct node {
        node(T const& payload, node const* next) :
                payload(payload), next(next) {
        }
        T payload;
        node const* next;
    };
public:
    struct iterator {
        iterator(node const* pointing_at) :
                pointing_at(pointing_at) {
        }

        T const& operator*() {
            return pointing_at->payload;
        }

        bool operator==(iterator other) {
            return pointing_at == other.pointing_at;
        }

        bool operator!=(iterator other) {
            return !(*this == other);
        }

        iterator & operator++() {
            pointing_at = pointing_at->next;
            return *this;
        }

        iterator operator+(int x) const {
            assert(x >= 0);

            iterator it(*this);

            for (int i(0); i < x; ++i) {
                ++it;
            }

            return it;
        }

        node const* pointing_at;
    };

    typedef iterator const_iterator;

    iterator begin() const {
        return iterator(first);
    }
    iterator end() const {
        return iterator(NULL);
    }

    bool empty() const {
        return size() == 0;
    }
    unsigned size() const {
        unsigned x(0);
        for (const_iterator it(begin()); it != end(); ++it) {
            ++x;
        }
        return x;
    }

    T const& front() const {
        assert(!empty());
        return first->payload;
    }

    list<T> new_push_front(T const& v) const {
        node const* new_first = GC_NEW(node)(v, first);
        return list<T>(new_first);
    }

    list<T> new_pop_front() const {
        assert(size() > 0);
        return list(first->next);
    }

private:
    list(node const* first) :
            first(first) {
    }

    node const* first;
};

}

