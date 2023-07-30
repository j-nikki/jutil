#!/bin/bash

btype="Debug"
if [ "$1" = "--release" ]; then
    shift
    btype="Release"
fi

([ ! -e build ] || rm -rf build) && mkdir build &&
cmake -B build "$@" -DCMAKE_BUILD_TYPE=$btype --preset $btype -DCMAKE_EXPORT_COMPILE_COMMANDS=ON &&
cmake --build build

# after this, you can do
# cmake --build build
# to build the project
