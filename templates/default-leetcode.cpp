// File Name: `(upcase (file-name-nondirectory (buffer-file-name)))`
// Author: `user-full-name`
// Email: `user-mail-address`
// Copyright © `(format-time-string "%Y")`, `user-full-name`, all rights reserved. 
// Created: `(format-time-string "%e %B %Y")` 

#include "headers.h"
#include <gtest/gtest.h>

class Solution {
public:

};

TEST(TTT, TTT) {
    Solution s;

}


int main(int argc, char **argv) {
    cout << "Problem: `(upcase (file-name-nondirectory (buffer-file-name)))`" << endl;
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}