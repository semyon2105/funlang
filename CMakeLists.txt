cmake_minimum_required(VERSION 3.4)

project(funlang_test)
add_executable(funlang_test test/main.cpp)

set_property(TARGET funlang_test PROPERTY CXX_STANDARD 14)
set_property(TARGET funlang_test PROPERTY CXX_STANDARD_REQUIRED 14)
