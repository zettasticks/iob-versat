SHELL:=/bin/sh

VERSAT_DIR:=$(shell pwd)

# Default rule
all: versat

include $(VERSAT_DIR)/config.mk

# VERSAT PATHS
VERSAT_SW_DIR:=$(VERSAT_DIR)/software
VERSAT_PC_DIR:=$(VERSAT_SW_DIR)/pc-emul
VERSAT_TOOLS_DIR:=$(VERSAT_SW_DIR)/tools
VERSAT_COMMON_DIR:=$(VERSAT_SW_DIR)/common
VERSAT_TEMPLATE_DIR:=$(VERSAT_SW_DIR)/templates
VERSAT_COMPILER_DIR:=$(VERSAT_SW_DIR)/compiler

BUILD_DIR:=$(VERSAT_DIR)/build
_a := $(shell mkdir -p $(BUILD_DIR)) # Creates the folder

TOOL_BUILD_DIR:=$(VERSAT_DIR)/tool_build
_b := $(shell mkdir -p $(TOOL_BUILD_DIR)) # Creates the folder

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
VERSAT_ALL_HEADERS += $(VERSAT_COMPILER_DIR)/versatSpecificationParser_meta.hpp

VERSAT_TEMPLATES:=$(wildcard $(VERSAT_TEMPLATE_DIR)/*.tpl)

VERSAT_INCLUDE := -I$(VERSAT_PC_DIR) -I$(VERSAT_COMPILER_DIR) -I$(BUILD_DIR)/ -I$(VERSAT_COMMON_DIR) -I$(VERSAT_SW_DIR)

VERSAT_COMPILER_SOURCES := $(wildcard $(VERSAT_COMPILER_DIR)/*.cpp)
VERSAT_COMPILER_OBJS := $(patsubst $(VERSAT_COMPILER_DIR)/%.cpp,$(BUILD_DIR)/%.o,$(VERSAT_COMPILER_SOURCES))

CPP_OBJ := $(VERSAT_COMMON_OBJS)
CPP_OBJ += $(VERSAT_COMPILER_OBJS)

COMPILE_TOOL = g++ -g -DPC -std=c++17 $(VERSAT_COMMON_FLAGS) -DVERSAT_DEBUG -o $@ $< -DROOT_PATH=\"$(abspath $(VERSAT_DIR))\" $(VERSAT_COMMON_INCLUDE) $(VERSAT_COMMON_TOOLS_OBJS) -lbfd
COMPILE_TOOL_NO_D = g++ -g -DPC -std=c++17 $(VERSAT_COMMON_FLAGS) -DVERSAT_DEBUG -o $@ $< -DROOT_PATH=\"$(abspath $(VERSAT_DIR))\" $(VERSAT_COMMON_INCLUDE) $(VERSAT_COMMON_TOOLS_OBJS) -lbfd

COMPILE_OBJ  = g++ -DPC -rdynamic -std=c++17 $(VERSAT_COMMON_FLAGS) -DVERSAT_DEBUG -c -o $@ $< -DROOT_PATH=\"$(abspath $(VERSAT_DIR))\" -g $(VERSAT_COMMON_INCLUDE) $(VERSAT_INCLUDE)
COMPILE_OBJ_NO_D = g++ -DPC -rdynamic -std=c++17 $(VERSAT_COMMON_FLAGS) -DVERSAT_DEBUG -c -o $@ $< -DROOT_PATH=\"$(abspath $(VERSAT_DIR))\" -g $(VERSAT_COMMON_INCLUDE) $(VERSAT_INCLUDE)

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
	$(COMPILE_TOOL)

$(META): $(VERSAT_TOOLS_DIR)/meta.cpp
	g++ -g -DPC -std=c++17 -o $@ $<

$(VERSAT_TOOLS_DIR)/libfst/src/%.o: $(VERSAT_TOOLS_DIR)/libfst/src/%.c
	gcc $< -c -o $@ -I$(VERSAT_TOOLS_DIR)/libfst/src

ALL_FST_C = $(wildcard $(VERSAT_TOOLS_DIR)/libfst/src/*.c)
ALL_FST_O = $(patsubst $(VERSAT_TOOLS_DIR)/libfst/src/%.c,$(VERSAT_TOOLS_DIR)/libfst/src/%.o,$(ALL_FST_C))

$(FST2SAIF): $(VERSAT_TOOLS_DIR)/fst2saif.cpp $(ALL_FST_O)
	g++ -g -std=c++17 $(VERSAT_COMMON_FLAGS) -DVERSAT_DEBUG $(VERSAT_COMMON_INCLUDE) $(VERSAT_COMMON_TOOLS_OBJS) -o $(FST2SAIF) $(VERSAT_TOOLS_DIR)/fst2saif.cpp $(wildcard $(VERSAT_TOOLS_DIR)/libfst/src/*.o) -I$(VERSAT_TOOLS_DIR)/libfst/src -lz -lbfd

# Versat
$(VERSAT_DIR)/versat: $(CPP_OBJ) $(VERSAT_ALL_HEADERS)
	g++ -std=c++17 $(VERSAT_COMMON_FLAGS) -DVERSAT_DEBUG -DVERSAT_DIR="$(VERSAT_DIR)" -rdynamic -DROOT_PATH=\"$(abspath $(VERSAT_DIR))\" -o $@ $(CPP_OBJ) $(VERSAT_INCLUDE) -lstdc++ -lm -lgcc -lc -pthread -ldl -lbfd

meta-data $(VERSAT_COMPILER_DIR)/versatSpecificationParser_meta.hpp: $(META) $(VERSAT_COMPILER_DIR)/versatSpecificationParser.meta
	$(META) $(VERSAT_SW_DIR)/compiler

debug-meta-data: $(META)
	gdb --args $(META) $(VERSAT_SW_DIR)/compiler

versat: $(VERSAT_DIR)/versat $(HASH)

clean:
	-rm -fr $(TOOL_BUILD_DIR)
	-rm -fr $(BUILD_DIR)
	-rm -f *.a versat 

clean-all: clean
	-rm -fr $(TOOL_BUILD_DIR)

fst2saif: $(TOOL_BUILD_DIR)/fst2saif

.PHONY: versat

.SUFFIXES:

