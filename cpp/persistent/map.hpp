#pragma once

#include <algorithm>
#include <typeinfo>
#include <bitset>
#include <vector>

#include <boost/static_assert.hpp>
#include <boost/functional/hash.hpp>
#include <boost/array.hpp>

#include "../alloc.hpp"

namespace persistent {

namespace map_impl {
template<typename K, typename V>
struct i_node;
}

template<typename K, typename V>
struct map {
    map();
    map(map const& other);

    V const* find(K const& k) const;
    bool empty() const;

    void insert(K const& k, V const& v);
    map<K, V> new_insert(K const& k, V const& v) const;

    void merge(map<K, V> other);
    map<K, V> new_merge(map<K, V> other) const;
private:
    map(map_impl::i_node<K, V> const*);
    map_impl::i_node<K, V> const* root;
};

namespace map_impl {

const unsigned BITS = 32;

typedef boost::uint32_t u32;

template<typename K>
inline u32 calc_hash(K const& k) {
    boost::hash<K> h;
    return h(k);
}

inline std::size_t mask(unsigned level, u32 hash) {
    return (hash >> (level * 5)) & 31;
}

inline std::size_t bitpos(unsigned level, u32 hash) {
    return 1 << mask(level, hash);
}

inline std::size_t index(u32 bitmap, unsigned bit) {
    u32 o = bit - 1;
    return std::bitset<BITS>(bitmap & o).count();
}

template<typename K, typename V>
struct i_node {
    virtual V const* find(unsigned level, u32 hash, K const& key) const = 0;
    virtual i_node const* new_insert(unsigned level, u32 hash, K const& key, V const& val) const = 0;
    virtual ~i_node() {
    }
};

template<typename K, typename V>
struct leaf_node: i_node<K, V> {
    leaf_node(K const& key, V const& val);
    leaf_node(K const& key, V const* val);
    virtual ~leaf_node();

    virtual V const* find(unsigned level, u32 hash, K const& key) const;
    virtual i_node<K, V> const* new_insert(unsigned level, u32 hash, K const& key, V const& val) const;

    K key;
    V const* val;
};

template<typename K, typename V>
struct bitmap_data {
    u32 bitmap;
    i_node<K, V> const* const * data_array;

    std::size_t size() const {
        return std::bitset<BITS>(bitmap).count();
    }
    bitmap_data(u32 b, i_node<K, V> const* const * data) :
            bitmap(b), data_array(data) {
    }
};

template<typename K, typename V>
struct i_bitmap_node: i_node<K, V> {
    virtual bitmap_data<K, V> get_vals() const = 0;
};

template<typename K, typename V, int Children>
struct array_node: i_bitmap_node<K, V> {
    array_node(u32 bitmap, boost::array<i_node<K, V> const*, Children> const& d);
    virtual ~array_node();

    virtual V const* find(unsigned level, u32 hash, K const& key) const;
    virtual i_node<K, V> const* new_insert(unsigned level, u32 hash, K const& key, V const& val) const;

    virtual bitmap_data<K, V> get_vals() const;
private:
    u32 bitmap;
    boost::array<i_node<K, V> const*, Children> data;
};

template<typename K, typename V>
struct array_node<K, V, BITS> : i_bitmap_node<K, V> {

    array_node(u32, boost::array<i_node<K, V> const*, BITS> const& d);
    array_node(boost::array<i_node<K, V> const*, BITS> const& d);
    virtual ~array_node();

    virtual V const* find(unsigned level, u32 hash, K const& key) const;
    virtual i_node<K, V> const* new_insert(unsigned level, u32 hash, K const& key, V const& val) const;

    virtual bitmap_data<K, V> get_vals() const;
private:
    boost::array<i_node<K, V> const*, BITS> data;
};

template<typename K, typename V>
struct collision_node: i_node<K, V> {
    collision_node(list<std::pair<K, V const*> > const& values);
    collision_node(K const& k1, V const& v1, K const& k2, V const* v2);
    virtual ~collision_node();

    virtual V const* find(unsigned level, u32 hash, K const& key) const;
    virtual i_node<K, V> const* new_insert(unsigned level, u32 hash, K const& key, V const& val) const;

    list<std::pair<K, V const*> > vals;
};

template<typename K, typename V>
i_node<K, V> const* merge(unsigned level, i_node<K, V> const* left, i_node<K, V> const* right);

#define GEN_ARRAY_CASE(NUM) case NUM: { \
    typedef array_node<K,V,NUM> arr_nd; \
    boost::array<i_node<K,V> const*, NUM> array; \
    std::copy(data.begin(), data.end(), array.begin()); \
    return GC_NEW(arr_nd)(bitmap, array); \
  }

template<typename K, typename V>
i_node<K, V> const* create_array_node(u32 bitmap, std::vector<i_node<K, V> const*> const& data) {

    assert(std::bitset<BITS>(bitmap).count() == data.size());

    switch (data.size()) {
    GEN_ARRAY_CASE(1)
    GEN_ARRAY_CASE(2)
    GEN_ARRAY_CASE(3)
    GEN_ARRAY_CASE(4)
    GEN_ARRAY_CASE(5)
    GEN_ARRAY_CASE(6)
    GEN_ARRAY_CASE(7)
    GEN_ARRAY_CASE(8)
    GEN_ARRAY_CASE(9)
    GEN_ARRAY_CASE(10)
    GEN_ARRAY_CASE(11)
    GEN_ARRAY_CASE(12)
    GEN_ARRAY_CASE(13)
    GEN_ARRAY_CASE(14)
    GEN_ARRAY_CASE(15)
    GEN_ARRAY_CASE(16)
    GEN_ARRAY_CASE(17)
    GEN_ARRAY_CASE(18)
    GEN_ARRAY_CASE(19)
    GEN_ARRAY_CASE(20)
    GEN_ARRAY_CASE(21)
    GEN_ARRAY_CASE(22)
    GEN_ARRAY_CASE(23)
    GEN_ARRAY_CASE(24)
    GEN_ARRAY_CASE(25)
    GEN_ARRAY_CASE(26)
    GEN_ARRAY_CASE(27)
    GEN_ARRAY_CASE(28)
    GEN_ARRAY_CASE(29)
    GEN_ARRAY_CASE(30)
    GEN_ARRAY_CASE(31)
    GEN_ARRAY_CASE(32)
    }
    assert(!"Oh crap, something went really wrong");
}

template<typename K, typename V>
inline i_node<K, V> const* merge_leaf_bitmap(unsigned level, leaf_node<K, V> const* left
        , i_bitmap_node<K, V> const* right) {
    u32 hash = calc_hash(left->key);

    std::size_t new_bit = bitpos(level, hash);

    bitmap_data<K, V> right_data = right->get_vals();
    std::vector<i_node<K, V> const*> new_data;

    for (u32 bit(1); bit; bit <<= 1) {
        if (right_data.bitmap & bit & new_bit) {
            new_data.push_back(merge(level + 1, left, right_data.data_array[index(right_data.bitmap, bit)]));
        } else if (right_data.bitmap & bit) {
            new_data.push_back(right_data.data_array[index(right_data.bitmap, bit)]);
        } else if (bit == new_bit) {
            new_data.push_back(left);
        }
    }

    return create_array_node(right_data.bitmap | new_bit, new_data);
}

template<typename K, typename V>
inline i_node<K, V> const* merge_leaf_leaf(unsigned level, leaf_node<K, V> const* left, leaf_node<K, V> const* right) {
    if (left->key == right->key)
        return right;

    if (level == 6) {
        typedef collision_node<K, V> col_node;
        return GC_NEW(col_node)(
                list<std::pair<K, V const*> >().new_push_front(std::make_pair(left->key, left->val)).new_push_front(
                        std::make_pair(left->key, left->val)));
    }

    assert(level < 6);

    u32 h = calc_hash(right->key);
    u32 hash = calc_hash(left->key);

    std::size_t b = bitpos(level, h);
    std::size_t bit = bitpos(level, hash);

    u32 new_bitmap = b | bit;

    if (b == bit) {
        assert(std::bitset<BITS>(new_bitmap).count() == 1);
        boost::array<i_node<K, V> const*, 1> new_data;

        new_data[0] = merge_leaf_leaf(level + 1, left, right);

        typedef array_node<K, V, 1> nde;
        return GC_NEW(nde)(new_bitmap, new_data);
    } else {
        assert(std::bitset<32>(new_bitmap).count() == 2);
        boost::array<i_node<K, V> const*, 2> new_data;

        typedef leaf_node<K, V> l_nde;

        new_data[0] = (b < bit) ? right : left;
        new_data[1] = (b < bit) ? left : right;

        typedef array_node<K, V, 2> nde;
        return GC_NEW(nde)(new_bitmap, new_data);
    }
}

template<typename K, typename V>
inline i_node<K, V> const* merge_bitmap_leaf(unsigned level, i_bitmap_node<K, V> const* left
        , leaf_node<K, V> const* right) {
    u32 hash = calc_hash(right->key);

    std::size_t new_bit = bitpos(level, hash);

    bitmap_data<K, V> left_data = left->get_vals();
    std::vector<i_node<K, V> const*> new_data;

    for (u32 bit(1); bit; bit <<= 1) {
        if (left_data.bitmap & bit & new_bit) {
            new_data.push_back(merge(level + 1, right, left_data.data_array[index(left_data.bitmap, bit)]));
        } else if (left_data.bitmap & bit) {
            new_data.push_back(left_data.data_array[index(left_data.bitmap, bit)]);
        } else if (bit == new_bit) {
            new_data.push_back(right);
        }
    }

    return create_array_node(left_data.bitmap | new_bit, new_data);

}

template<typename K, typename V>
inline i_node<K, V> const* merge_leaf_coll(unsigned level, leaf_node<K, V> const* left
        , collision_node<K, V> const* right) {
    // TODO: should be pushed at the back, not front
    typedef collision_node<K, V> col_nd;
    return GC_NEW(col_nd)(right->vals.new_push_front(std::make_pair(left->key, left->val)));
}

template<typename K, typename V>
inline i_node<K, V> const* merge_coll_coll(unsigned level, collision_node<K, V> const* left
        , collision_node<K, V> const* right) {

    list<std::pair<K, V const*> > values = left->vals;

    for (typename list<std::pair<K, V const*> >::const_iterator it(right->vals.begin()); it != right->vals.end();
            ++it) {
        values = values.new_push_front(*it);
    }

    typedef collision_node<K, V> col_nd;
    return GC_NEW(col_nd)(values);
}

template<typename K, typename V>
inline i_node<K, V> const* merge_coll_leaf(unsigned level, collision_node<K, V> const* left
        , leaf_node<K, V> const* right) {
    typedef collision_node<K, V> col_nd;
    return GC_NEW(col_nd)(left->vals.new_push_front(std::make_pair(right->key, right->val)));
}

template<typename K, typename V>
inline i_node<K, V> const* merge_bitmap_bitmap(unsigned level, i_bitmap_node<K, V> const* left
        , i_bitmap_node<K, V> const* right) {
    bitmap_data<K, V> left_data = left->get_vals();
    bitmap_data<K, V> right_data = right->get_vals();

    u32 new_bitmap = left_data.bitmap | right_data.bitmap;

    std::vector<i_node<K, V> const*> new_data;

    for (u32 bit(1); bit; bit <<= 1) {
        if (left_data.bitmap & bit & right_data.bitmap) {
            unsigned left_dex = index(left_data.bitmap, bit);
            unsigned right_dex = index(right_data.bitmap, bit);

            i_node<K, V> const* n = merge(level + 1, left_data.data_array[left_dex], right_data.data_array[right_dex]);
            new_data.push_back(n);
        } else if (left_data.bitmap & bit) {
            unsigned dex = index(left_data.bitmap, bit);

            i_node<K, V> const* n = left_data.data_array[dex];
            new_data.push_back(n);
        } else if (right_data.bitmap & bit) {
            unsigned dex = index(right_data.bitmap, bit);

            i_node<K, V> const* n = right_data.data_array[dex];
            new_data.push_back(n);
        }
    }

    return create_array_node(new_bitmap, new_data);
}

template<typename K, typename V>
i_node<K, V> const* merge(unsigned level, i_node<K, V> const* left, i_node<K, V> const* right) {
    assert(left && right);

    if (left == right)
        return left;

    i_bitmap_node<K, V> const* left_bn = dynamic_cast<i_bitmap_node<K, V> const*>(left);
    if (left_bn) {
        i_bitmap_node<K, V> const* right_bn = dynamic_cast<i_bitmap_node<K, V> const*>(right);
        if (right_bn)
            return merge_bitmap_bitmap(level, left_bn, right_bn);
        else {
            leaf_node<K, V> const* right_ln = dynamic_cast<leaf_node<K, V> const*>(right);
            assert(right_ln);

            return merge_bitmap_leaf(level, left_bn, right_ln);
        }
    } else {
        leaf_node<K, V> const* left_ln = dynamic_cast<leaf_node<K, V> const*>(left);
        if (left_ln) {
            i_bitmap_node<K, V> const* right_bn = dynamic_cast<i_bitmap_node<K, V> const*>(right);
            if (right_bn)
                return merge_leaf_bitmap(level, left_ln, right_bn);
            else {
                leaf_node<K, V> const* right_ln = dynamic_cast<leaf_node<K, V> const*>(right);
                if (right_ln)
                    return merge_leaf_leaf(level, left_ln, right_ln);
                else {
                    collision_node<K, V> const* right_cn = dynamic_cast<collision_node<K, V> const*>(right);
                    assert(right_cn);
                    return merge_leaf_coll(level, left_ln, right_cn);
                }
            }
        } else {
            collision_node<K, V> const* left_cn = dynamic_cast<collision_node<K, V> const*>(left);
            assert(left_cn);

            leaf_node<K, V> const* right_ln = dynamic_cast<leaf_node<K, V> const*>(right);
            if (right_ln) {
                return merge_coll_leaf(level, left_cn, right_ln);
            } else {
                collision_node<K, V> const* right_cn = dynamic_cast<collision_node<K, V> const*>(right);
                assert(right_cn);
                return merge_coll_coll(level, left_cn, right_cn);
            }

        }
    }
}

}

template<typename K, typename V>
inline map<K, V>::map() :
        root(NULL) {
}

template<typename K, typename V>
inline map<K, V>::map(map<K, V> const& other) :
        root(other.root) {
}

template<typename K, typename V>
inline map<K, V>::map(map_impl::i_node<K, V> const* r) :
        root(r) {
}

template<typename K, typename V>
inline V const* map<K, V>::find(K const& k) const {
    return (root == NULL) ? NULL : root->find(0, map_impl::calc_hash(k), k);
}

template<typename K, typename V>
inline bool map<K, V>::empty() const {
    return (root == NULL);
}

template<typename K, typename V>
inline void map<K, V>::insert(K const& k, V const& v) {
    if (root == NULL) {
        typedef map_impl::leaf_node<K, V> nde;
        root = GC_NEW(nde)(k, v);
    } else {
        root = root->new_insert(0, map_impl::calc_hash(k), k, v);
    }
}

template<typename K, typename V>
inline map<K, V> map<K, V>::new_insert(K const& k, V const& v) const {
    if (root == NULL) {
        typedef map_impl::leaf_node<K, V> nde;
        return map<K, V>(GC_NEW(nde)(k, v));
    } else {
        return map<K, V>(root->new_insert(0, map_impl::calc_hash(k), k, v));
    }
}

template<typename K, typename V>
inline void map<K, V>::merge(map<K, V> other) {

    if (root != NULL && other.root != NULL)
        root = map_impl::merge(0, root, other.root);
    else if (root == NULL && other.root != NULL)
        root = other.root;
}

template<typename K, typename V>
inline map<K, V> map<K, V>::new_merge(map<K, V> other) const {

    if (root != NULL && other.root != NULL)
        return map<K, V>(map_impl::merge(0, root, other.root));
    else if (root == NULL && other.root == NULL)
        return map<K, V>();
    else if (root != NULL)
        return map<K, V>(root);
    else {
        assert(other.root != NULL);
        return map<K, V>(other.root);
    }
}

namespace map_impl {

template<typename K, typename V, int Children>
inline array_node<K, V, Children>::array_node(u32 bitmap, boost::array<i_node<K, V> const*, Children> const& d) :
        bitmap(bitmap), data(d) {
}

template<typename K, typename V, int Children>
inline array_node<K, V, Children>::~array_node() {
}

template<typename K, typename V, int Children>
V const* array_node<K, V, Children>::find(unsigned level, u32 hash, K const& key) const {
    std::size_t bit = bitpos(level, hash);

    if (bitmap & bit)
        return data[index(bitmap, bit)]->find(level + 1, hash, key);
    else
        return NULL;
}

template<typename K, typename V, int Children>
inline i_node<K, V> const* array_node<K, V, Children>::new_insert(unsigned level, u32 hash, K const& key,
        V const& val) const {

    std::size_t bit = bitpos(level, hash);

    if (bitmap & bit) {
        i_node<K, V> const* new_val = data[index(bitmap, bit)]->new_insert(level + 1, hash, key, val);

        std::size_t dex = index(bitmap, bit);

        boost::array<i_node<K, V> const*, Children> new_data;

        for (unsigned i(0); i < Children; ++i) {
            new_data[i] = (i == dex) ? new_val : data[i];
        }

        typedef array_node<K, V, Children> nde;
        return GC_NEW(nde)(bitmap, new_data);

    } else {
        typedef leaf_node<K, V> l_nde;
        i_node<K, V> const* new_val = GC_NEW(l_nde)(key, val);

        u32 new_bitmap = bitmap | bit;

        std::size_t dex = index(new_bitmap, bit);

        boost::array<i_node<K, V> const*, Children + 1> new_data;
        for (unsigned i(0); i <= Children; ++i) {
            if (i < dex) {
                new_data[i] = data[i];
            } else if (i > dex) {
                new_data[i] = data[i - 1];
            } else {
                assert(i == dex);
                new_data[i] = new_val;
            }
        }

        typedef array_node<K, V, Children + 1> nde;

        return GC_NEW(nde)(new_bitmap, new_data);
    }
}

template<typename K, typename V, int Children>
inline bitmap_data<K, V> array_node<K, V, Children>::get_vals() const {
    return bitmap_data<K, V>(bitmap, &data[0]);
}

template<typename K, typename V>
inline array_node<K, V, BITS>::array_node(u32, boost::array<i_node<K, V> const*, BITS> const& d) :
        data(d) {
}

template<typename K, typename V>
inline array_node<K, V, BITS>::array_node(boost::array<i_node<K, V> const*, BITS> const& d) :
        data(d) {
}

template<typename K, typename V>
inline array_node<K, V, BITS>::~array_node() {
}

template<typename K, typename V>
inline V const* array_node<K, V, BITS>::find(unsigned level, u32 hash, K const& key) const {
    i_node<K, V> const* p = data[mask(level, hash)];
    assert(p != NULL);

    return p->find(level + 1, hash, key);
}

template<typename K, typename V>
inline i_node<K, V> const* array_node<K, V, BITS>::new_insert(unsigned level, u32 hash, K const& key,
        V const& val) const {

    u32 dex = mask(level, hash);
    i_node<K, V> const* p = data[dex];
    assert(p != NULL);

    boost::array<i_node<K, V> const*, BITS> new_data;

    for (unsigned i(0); i < 32; ++i) {
        if (i == dex)
            new_data[i] = p->new_insert(level + 1, hash, key, val);
        else
            new_data[i] = data[i];
    }

    typedef array_node<K, V, BITS> nde;
    return GC_NEW(nde)(new_data);
}

template<typename K, typename V>
inline bitmap_data<K, V> array_node<K, V, BITS>::get_vals() const {

    u32 max = std::numeric_limits<u32>::max();
    assert(std::bitset<BITS>(max).count() == BITS);
    // all ones

    return bitmap_data<K, V>(max, &data[0]);
}

template<typename K, typename V>
inline leaf_node<K, V>::leaf_node(K const& k, V const& v) :
        key(k), val(GC_NEW(V)(v)) {
}

template<typename K, typename V>
inline leaf_node<K, V>::leaf_node(K const& k, V const* v) :
        key(k), val(v) {
}

template<typename K, typename V>
inline V const* leaf_node<K, V>::find(unsigned /*level*/, u32 /*hash*/, K const& k) const {
    return (k == key) ? val : NULL;
}

template<typename K, typename V>
inline i_node<K, V> const* leaf_node<K, V>::new_insert(unsigned level, u32 h, K const& k, V const& v) const {

    if (level != 6) {
        assert(level < 6);
        if (k == key) {
            typedef leaf_node<K, V> nde;
            return GC_NEW(nde)(k, v);
        } else {
            std::size_t b = bitpos(level, h);
            std::size_t bit = bitpos(level, calc_hash(key));

            u32 new_bitmap = b | bit;

            if (b == bit) {
                assert(std::bitset<BITS>(new_bitmap).count() == 1);
                boost::array<i_node<K, V> const*, 1> new_data;

                // ok, this is being tricksy. verify it works...
                new_data[0] = new_insert(level + 1, h, k, v);
                //new_data[0] = leaf_node<K, V>(key, val).new_insert(h, k, v);

                typedef array_node<K, V, 1> nde;
                return GC_NEW(nde)(new_bitmap, new_data);
            } else {
                assert(std::bitset<32>(new_bitmap).count() == 2);
                boost::array<i_node<K, V> const*, 2> new_data;

                typedef leaf_node<K, V> l_nde;

                l_nde const* l = GC_NEW(l_nde)(k, v);
                l_nde const* leaf = GC_NEW(l_nde)(key, val);

                new_data[0] = (b < bit) ? l : leaf;
                new_data[1] = (b < bit) ? leaf : l;

                typedef array_node<K, V, 2> nde;
                return GC_NEW(nde)(new_bitmap, new_data);
            }
        }
    } else {
        typedef collision_node<K, V> col_node;
        return GC_NEW(col_node)(k, v, key, val);
    }
}

template<typename K, typename V>
inline leaf_node<K, V>::~leaf_node() {
}

template<typename K, typename V>
inline collision_node<K, V>::collision_node(persistent::list<std::pair<K, V const*> > const& values) :
        vals(values) {
}

template<typename K, typename V>
inline collision_node<K, V>::collision_node(K const& k1, V const& v1, K const& k2, V const* v2) :
        vals(
                list<std::pair<K, V const*> >().new_push_front(std::make_pair(k2, v2)).new_push_front(
                        std::make_pair(k1, GC_NEW(V)(v1)))) {
}

template<typename K, typename V>
inline collision_node<K, V>::~collision_node() {
}

template<typename K, typename V>
inline V const* collision_node<K, V>::find(unsigned /*level*/, u32 /*hash*/, K const& key) const {

    for (typename list<std::pair<K, V const*> >::const_iterator it(vals.begin()); it != vals.end(); ++it) {
        if ((*it).first == key) // TODO add operator->
            return (*it).second;
    }

    return NULL;
}

template<typename K, typename V>
inline i_node<K, V> const* collision_node<K, V>::new_insert(unsigned /*level*/, u32 /*hash*/, K const& key,
        V const& val) const {
    V const* new_val = GC_NEW(V)(val);
    return GC_NEW(collision_node)(vals.new_push_front(std::make_pair(key, new_val)));
}
}
}

