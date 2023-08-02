cmake_minimum_required(VERSION 3.20)

add_library(jutil STATIC jutil.cpp)
target_include_directories(jutil INTERFACE .)

if (JUTIL_TESTS)
    add_executable(jutil_test jutil_test.cpp)
    target_link_libraries(jutil_test PRIVATE jutil)
    target_compile_options(jutil_test PRIVATE -ffreestanding -fno-rtti -fno-exceptions)
    # target_link_options(jutil_test PRIVATE -nostartfiles)
    # target_compile_definitions(jutil_test PRIVATE JUTIL_DEBUG)
endif ()
