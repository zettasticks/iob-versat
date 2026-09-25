# =======================================================
#
# Do not call Makefile directly to build Versat.
# Use build.sh in order to compile versat properly
#
# =======================================================

SHELL:=/bin/sh

# Default rule
all: versat

VERSAT_COMMON_FLAGS := -Wall -Werror=return-type -Wunused-variable -Wno-char-subscripts -Wno-switch-enum -Wno-switch -Wno-unused-function 

# Useful to run addressSanitizer every so often 
# NOTE: disable ASLR or we are gonna keep having random segfaults.
#       You can disbable it temporarely (until reboot) by running the command:
#       " echo 0 | sudo tee /proc/sys/kernel/randomize_va_space "
# VERSAT_COMMON_FLAGS += -fsanitize=address -static-libasan

#VERSAT_COMMON_FLAGS += -ggdb3 # Outputs more debug info but only when debuggin with gdb

# Use these flags to use compiler to fix Wall errors.
# Some errors are disabled since they are not problematic (like unused functions for operator== and stuff like that)
#VERSAT_COMMON_FLAGS += -Werror -Wno-char-subscripts -Wunused-function #-Wno-switch-enum

#VERSAT_COMMON_FLAGS += -Wall -Wno-switch-enum -Wno-switch -Wno-char-subscripts -Werror

VERSAT_DEBUG:=0

USE_FST_FORMAT ?= 0

VERSAT_DIR:=$(shell pwd)
BUILD_DIR:=$(VERSAT_DIR)/build
TOOL_BUILD_DIR:=$(VERSAT_DIR)/tool_build

# VERSAT PATHS
VERSAT_SW_DIR:=$(VERSAT_DIR)/software
VERSAT_PC_DIR:=$(VERSAT_SW_DIR)/pc-emul
VERSAT_TOOLS_DIR:=$(VERSAT_SW_DIR)/tools
VERSAT_COMMON_DIR:=$(VERSAT_SW_DIR)/common
VERSAT_TEMPLATE_DIR:=$(VERSAT_SW_DIR)/templates
VERSAT_COMPILER_DIR:=$(VERSAT_SW_DIR)/compiler

#Tools
HASH      := $(TOOL_BUILD_DIR)/calculateHash
META      := $(TOOL_BUILD_DIR)/meta
VCD2SAIF  := $(TOOL_BUILD_DIR)/vcd2saif
FST2SAIF  := $(TOOL_BUILD_DIR)/fst2saif

VERSAT_COMMON_HEADERS := $(wildcard $(VERSAT_COMMON_DIR)/*.hpp)
VERSAT_COMMON_SOURCES := $(wildcard $(VERSAT_COMMON_DIR)/*.cpp)
VERSAT_COMMON_OBJS := $(patsubst $(VERSAT_COMMON_DIR)/%.cpp,$(BUILD_DIR)/%.o,$(VERSAT_COMMON_SOURCES))
VERSAT_COMMON_INCLUDE := -I$(VERSAT_COMMON_DIR) -I$(VERSAT_SW_DIR)

VERSAT_COMMON_TOOLS_OBJS := $(patsubst $(VERSAT_COMMON_DIR)/%.cpp,$(TOOL_BUILD_DIR)/%.o,$(VERSAT_COMMON_SOURCES))

VERSAT_ALL_HEADERS := $(VERSAT_COMMON_HEADERS) $(wildcard $(VERSAT_COMPILER_DIR)/*.hpp)

VERSAT_INCLUDE := -I$(VERSAT_PC_DIR) -I$(VERSAT_COMPILER_DIR) -I$(BUILD_DIR)/ -I$(VERSAT_COMMON_DIR) -I$(VERSAT_SW_DIR)

VERSAT_COMPILER_SOURCES := $(wildcard $(VERSAT_COMPILER_DIR)/*.cpp)
VERSAT_COMPILER_OBJS := $(patsubst $(VERSAT_COMPILER_DIR)/%.cpp,$(BUILD_DIR)/%.o,$(VERSAT_COMPILER_SOURCES))

CPP_OBJ := $(VERSAT_COMMON_OBJS)
CPP_OBJ += $(VERSAT_COMPILER_OBJS)

COMPILE_OBJ  = g++ -DPC -rdynamic -std=c++17 $(VERSAT_COMMON_FLAGS) -DVERSAT_DEBUG -c -o $@ $< -DROOT_PATH=\"$(abspath $(VERSAT_DIR))\" -g $(VERSAT_COMMON_INCLUDE) $(VERSAT_INCLUDE)

# Common objects (used by Versat and tools)
$(BUILD_DIR)/%.o : $(VERSAT_COMMON_DIR)/%.cpp $(VERSAT_COMMON_HEADERS)
	$(COMPILE_OBJ)

$(TOOL_BUILD_DIR)/%.o : $(VERSAT_COMMON_DIR)/%.cpp $(VERSAT_COMMON_HEADERS)
	$(COMPILE_OBJ)

# Compiler objects
$(BUILD_DIR)/%.o : $(VERSAT_COMPILER_DIR)/%.cpp $(VERSAT_ALL_HEADERS)
	$(COMPILE_OBJ)

# Tools
$(HASH): $(VERSAT_TOOLS_DIR)/calculateHash.cpp $(VERSAT_COMMON_TOOLS_OBJS) $(VERSAT_COMMON_HEADERS)
	g++ -g -DPC -std=c++17 $(VERSAT_COMMON_FLAGS) -DVERSAT_DEBUG -o $@ $< -DROOT_PATH=\"$(abspath $(VERSAT_DIR))\" $(VERSAT_COMMON_INCLUDE) $(VERSAT_COMMON_TOOLS_OBJS) -lbfd

$(META): $(VERSAT_TOOLS_DIR)/meta.cpp
	g++ -g -DPC -std=c++17 -o $@ $<

# Versat
$(VERSAT_DIR)/versat: $(CPP_OBJ) $(VERSAT_ALL_HEADERS)
	g++ -std=c++17 $(VERSAT_COMMON_FLAGS) -DVERSAT_DEBUG -DVERSAT_DIR="$(VERSAT_DIR)" -rdynamic -DROOT_PATH=\"$(abspath $(VERSAT_DIR))\" -o $@ $(CPP_OBJ) $(VERSAT_INCLUDE) -lstdc++ -lm -lgcc -lc -pthread -ldl -lbfd

versat: $(VERSAT_DIR)/versat $(HASH)

meta-data: $(META)
	$(META) $(VERSAT_SW_DIR)/compiler

debug-meta-data: $(META)
	gdb --args $(META) $(VERSAT_SW_DIR)/compiler

clean:
	-rm -fr $(TOOL_BUILD_DIR)
	-rm -fr $(BUILD_DIR)
	-rm -f *.a versat 

.PHONY: versat

.SUFFIXES:

# Disabled FST -> SAIF code, not properly maintained

#$(VERSAT_TOOLS_DIR)/libfst/src/%.o: $(VERSAT_TOOLS_DIR)/libfst/src/%.c
#	gcc $< -c -o $@ -I$(VERSAT_TOOLS_DIR)/libfst/src
#
#ALL_FST_C = $(wildcard $(VERSAT_TOOLS_DIR)/libfst/src/*.c)
#ALL_FST_O = $(patsubst $(VERSAT_TOOLS_DIR)/libfst/src/%.c,$(VERSAT_TOOLS_DIR)/libfst/src/%.o,$(ALL_FST_C))
#
#$(FST2SAIF): $(VERSAT_TOOLS_DIR)/fst2saif.cpp $(ALL_FST_O)
#	g++ -g -std=c++17 $(VERSAT_COMMON_FLAGS) -DVERSAT_DEBUG $(VERSAT_COMMON_INCLUDE) $(VERSAT_COMMON_TOOLS_OBJS) -o $(FST2SAIF) $(VERSAT_TOOLS_DIR)/fst2saif.cpp $(wildcard $(VERSAT_TOOLS_DIR)/libfst/src/*.o) -I$(VERSAT_TOOLS_DIR)/libfst/src -lz -lbfd
#
#fst2saif: $(TOOL_BUILD_DIR)/fst2saif
