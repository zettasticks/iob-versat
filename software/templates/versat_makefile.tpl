TYPE_NAME := Versat_verilator_wrapper

HARDWARE_FOLDER := @{hardwareFolder}
VHEADER := V$(TYPE_NAME).h pcEmulDefs.h
SOFTWARE_FOLDER := .
HARDWARE_SRC := @{hardwareUnits}
HARDWARE_SRC += @{moduleUnits}
HARDWARE_SRC += Versat_verilator_wrapper.v
VERILATOR_ROOT?=$(shell ./GetVerilatorRoot.sh)
INCLUDE := -I$(HARDWARE_FOLDER)

VERILATOR_ARGS := -Wall --report-unoptflat --debug
VERILATOR_ARGS := -CFLAGS "-O2 -march=native" $(INCLUDE)
VERILATOR_ARGS += -GAXI_ADDR_W=$(shell getconf LONG_BIT)
VERILATOR_ARGS += -GAXI_DATA_W=@{databusDataSize}
VERILATOR_ARGS += -GDATA_W=32
VERILATOR_ARGS += -GDELAY_W=20
VERILATOR_ARGS += -GLEN_W=20
VERILATOR_ARGS += -GADDR_W=@{addressSize}
VERILATOR_ARGS += @{traceType}

all: libaccel.a

# Joins wrapper with verilator object files into a library
libaccel.a: $(VHEADER) verilator_wrapper.o createVerilatorObjects
	ar -rcs libaccel.a verilator_wrapper.o $(wildcard ./obj_dir/*.o)

createVerilatorObjects: $(VHEADER) verilator_wrapper.o
	$(MAKE) verilatorObjects

VUnitWireInfo.h: $(VHEADER)
	./ExtractVerilatedSignals.py $(VHEADER) > VUnitWireInfo.h

$(VHEADER): $(HARDWARE_SRC)
	verilator $(VERILATOR_ARGS) --cc $(HARDWARE_SRC) --top-module $(TYPE_NAME)
	$(MAKE) -C ./obj_dir -f V$(TYPE_NAME).mk
	cp ./obj_dir/*.h ./

verilator_wrapper.o: $(VHEADER) VUnitWireInfo.h verilator_wrapper.cpp
	g++ -std=c++17 -march=native -O2 -g -c -o verilator_wrapper.o -I $(VERILATOR_ROOT)/include $(abspath verilator_wrapper.cpp)

# Created after calling verilator. Need to recall make to have access to the variables
-include ./obj_dir/V$(TYPE_NAME)_classes.mk

VERILATOR_SOURCE_DIR:=$(VERILATOR_ROOT)/include
ALL_VERILATOR_FILES:=$(VM_GLOBAL_FAST) $(VM_GLOBAL_SLOW)
ALL_VERILATOR_O:=$(patsubst %,./obj_dir/%.o,$(ALL_VERILATOR_FILES))

./obj_dir/%.o: $(VERILATOR_SOURCE_DIR)/%.cpp
	g++ -w -march=native -O2 -c -o $@ $< -I$(VERILATOR_ROOT)/include

verilatorObjects: $(ALL_VERILATOR_O)
