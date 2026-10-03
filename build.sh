#!/bin/sh

if [ ! -e ./build ]; then
   mkdir ./build
fi
if [ ! -e ./tool_build ]; then
   mkdir ./tool_build
fi

make meta-data
if [ $? != 0 ]; then
   exit
fi

make -j 8 versat