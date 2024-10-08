#{let TRACE_TYPE ""}
#{if traceEnabled} 
  #{set TRACE_TYPE "--trace"}
  #{if arch.generateFSTFormat} #{set TRACE_TYPE "--trace-fst"} #{end}
#{end}

TYPE_NAME := @{typename}

HARDWARE_SRC := #{join " " file verilogFiles}@{file}#{end}
#HARDWARE_SRC := $(wildcard @{generatedUnitsLocation}/*.v)
HARDWARE_SRC += $(wildcard @{generatedUnitsLocation}/modules/*.v)

#{for source extraSources}
HARDWARE_SRC += $(wildcard @{source}/*.v)
#{end}

VERILATOR_ROOT?=@{verilatorRoot}

INCLUDE := #{join " " file includePaths}-I@{file}#{end}

all: libaccel.a

# Joins wrapper with verilator object files into a library
libaccel.a: V@{typename}.h wrapper.o createVerilatorObjects
	ar -rcs libaccel.a wrapper.o $(wildcard ./obj_dir/*.o)

createVerilatorObjects: V@{typename}.h wrapper.o
	$(MAKE) verilatorObjects

#./obj_dir/V@{typename}_classes.mk: V@{typename}.h

# TODO: src folder should be set to absolute. Versat compiler knows the location 
V@{typename}.h: $(HARDWARE_SRC)
	verilator --report-unoptflat -GAXI_ADDR_W=@{arch.databusAddrSize} -GAXI_DATA_W=@{arch.databusDataSize} -GLEN_W=16 -CFLAGS "-O2 -march=native" @{TRACE_TYPE} --cc $(HARDWARE_SRC) $(wildcard @{srcDir}/*.v) $(INCLUDE) --top-module $(TYPE_NAME)
	$(MAKE) -C ./obj_dir -f V@{typename}.mk
	cp ./obj_dir/*.h ./

wrapper.o: V@{typename}.h wrapper.cpp
	g++ -std=c++17 -march=native -O2 -g -c -o wrapper.o -I $(VERILATOR_ROOT)/include $(abspath wrapper.cpp)

# Created after calling verilator. Need to recall make to have access to the variables 
-include ./obj_dir/V@{typename}_classes.mk

VERILATOR_SOURCE_DIR:=$(VERILATOR_ROOT)/include
ALL_VERILATOR_FILES:=$(VM_GLOBAL_FAST) $(VM_GLOBAL_SLOW)
ALL_VERILATOR_O:=$(patsubst %,./obj_dir/%.o,$(ALL_VERILATOR_FILES))
ALL_VERILATOR_CPPS:=$(patsubst %,$(VERILATOR_SOURCE_DIR)/%.cpp,$(ALL_VERILATOR_FILES))

./obj_dir/%.o: $(VERILATOR_SOURCE_DIR)/%.cpp
	g++ -w -march=native -O2 -c -o $@ $< -I$(VERILATOR_ROOT)/include

verilatorObjects: $(ALL_VERILATOR_O)
#	@echo $(ALL_VERILATOR_FILES)
#	@echo $(ALL_VERILATOR_CPPS)
#	@echo $(ALL_VERILATOR_O)
