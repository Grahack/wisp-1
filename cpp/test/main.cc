#include <iostream>

#include "../persistent/list.hpp"
#include "../persistent/string.hpp"
#include "../persistent/map.hpp"

void require(bool cond) {
    if (!cond) {
        std::cout << "Unit test failed. Aborting." << std::endl;
        ::abort();
    }
}

void string_test() {
    persistent::string str("Chicken");
    persistent::string str2("Little");

    require(str + str2 == persistent::string("ChickenLittle"));
}

void map_test() {
    using persistent::map;
    map<std::string, int> phm;

    require(phm.find("Chicken") == NULL);
    phm = phm.new_insert("Chicken", 23);
    require(phm.find("Chicken") != NULL);
    require(*phm.find("Chicken") == 23);

    map<std::string, int> phm2 = phm.new_insert("tiger", 32);
    require(phm2.find("Chicken") != NULL);
    require(phm.find("tiger") == NULL);
    require(*phm2.find("tiger") == 32);

    // test shadowing
    map<std::string, int> phm3 = phm.new_insert("tiger", 1337);
    require(*phm2.find("tiger") == 32); // untouched
    require(*phm3.find("tiger") == 1337);
}


void merge_test() {
    typedef persistent::map<std::string, int> map;

    const map empty_phm;
    require(empty_phm.empty());

    const map single_val = map().new_insert("Chicken", 23);
    require(!single_val.empty());

    map test = single_val.new_merge(empty_phm);
    require(!test.empty());
    require(*test.find("Chicken") == 23);

    test = empty_phm.new_merge(single_val);
    require(!test.empty());
    require(*test.find("Chicken") == 23);

    test = single_val.new_merge(single_val);
    require(!test.empty());
    require(*test.find("Chicken") == 23);

    test = empty_phm.new_merge(empty_phm);
    require(test.empty());


    map double_val = map().new_insert("Rocket", 39).new_insert("Science", 103);
    test = double_val.new_merge(single_val);
    require(!test.empty());
    require(*test.find("Chicken") == 23);
    require(*test.find("Rocket") == 39);
    require(*test.find("Science") == 103);

    test = test.new_merge(test);
    require(!test.empty());
    require(*test.find("Chicken") == 23);
    require(*test.find("Rocket") == 39);
    require(*test.find("Science") == 103);





}


int test_main(int, char**) {

    std::cout << "Harkon Test\n\n";

    map_test();
    string_test();
    merge_test();

    std::cout << "All tests passed!";
    return 0;

}
