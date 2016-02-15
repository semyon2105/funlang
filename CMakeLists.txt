cmake_minimum_required(VERSION 3.4)

project(Funlang)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED on)

include_directories(include)

set(SOURCES src/lex.cpp src/test.cpp)

add_executable(funlang_test ${SOURCES})
