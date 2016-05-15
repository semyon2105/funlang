cmake_minimum_required(VERSION 3.4)

project(Funlang)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED on)

find_package(Boost 1.56.0 REQUIRED)
find_package(LLVM 3.8 REQUIRED)

include_directories(include ${Boost_INCLUDE_DIRS} ${LLVM_INCLUDE_DIRS})

set(SOURCES_BASE include/lex.h include/token.h include/parse.h include/ast_nodes.h include/ast_printer.h include/codegen.h src/lex.cpp src/ast_nodes.cpp src/parse.cpp src/token.cpp src/ast_printer.cpp src/codegen.cpp)
set(SOURCES_TEST ${SOURCES_BASE} src/test.cpp)
set(SOURCES_DRIVER ${SOURCES_BASE} src/driver.cpp)

add_definitions(-D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS)

add_executable(funlang_test ${SOURCES_TEST})
target_link_libraries(funlang_test LLVM)

add_executable(funlang ${SOURCES_DRIVER})
target_link_libraries(funlang LLVM)
