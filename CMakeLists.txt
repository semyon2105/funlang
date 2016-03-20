cmake_minimum_required(VERSION 3.4)

project(Funlang)

#find_library(LIBCXXABI_LIBRARY NAMES libc++ libc++abi)
#if (LIBCXXABI_LIBRARY)
#    add_compile_options(-stdlib=libc++)
#    link_libraries(-nodefaultlibs -lc++ -lc++abi -lm -lc -lgcc_s -lgcc)
#endif(LIBCXXABI_LIBRARY)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED on)

include_directories(include)

set(SOURCES include/lex.h include/token.h src/lex.cpp src/test.cpp include/parse.h include/ast_nodes.h src/ast_nodes.cpp src/parse.cpp include/program_ast.h src/program_ast.cpp src/token.cpp include/ast_printer.h src/ast_printer.cpp)

add_executable(funlang_test ${SOURCES})
