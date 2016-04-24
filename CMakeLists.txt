cmake_minimum_required(VERSION 3.4)

project(Funlang)

find_library(LIBCXXABI_LIBRARY NAMES libc++ libc++abi)
if (LIBCXXABI_LIBRARY)
    add_compile_options(-stdlib=libc++)
    link_libraries(-nodefaultlibs -lc++ -lc++abi -lm -lc -lgcc_s -lgcc)
endif(LIBCXXABI_LIBRARY)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED on)

find_package(Boost 1.56.0 REQUIRED)
find_package(LLVM 3.7 REQUIRED)

include_directories(include ${Boost_INCLUDE_DIRS} ${LLVM_INCLUDE_DIRS})

set(SOURCES include/util.h include/lex.h include/token.h include/parse.h include/ast_nodes.h include/ast_printer.h include/codegen.h src/lex.cpp src/test.cpp src/ast_nodes.cpp src/parse.cpp src/token.cpp src/ast_printer.cpp src/codegen.cpp src/driver.cpp)

add_definitions(-D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS)

add_executable(funlang_test ${SOURCES})
target_link_libraries(funlang_test LLVM)
