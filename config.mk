VERSAT_COMMON_FLAGS := -Wswitch-enum -g # -Werror=switch-enum # -ggdb3 Outputs more debug info but only when debuggin with gdb

VERSAT_DEBUG:=0

USE_FST_FORMAT ?= 0

# VERSAT PATHS
VERSAT_HW_DIR:=$(VERSAT_DIR)/hardware
VERSAT_SW_DIR:=$(VERSAT_DIR)/software
VERSAT_COMP_DIR:=$(VERSAT_SW_DIR)/compiler
VERSAT_SUBMODULES_DIR:=$(VERSAT_DIR)/submodules
VERSAT_PC_DIR:=$(VERSAT_SW_DIR)/pc-emul
VERSAT_TOOLS_DIR:=$(VERSAT_SW_DIR)/tools
VERSAT_COMMON_DIR:=$(VERSAT_SW_DIR)/common
VERSAT_TEMPLATE_DIR:=$(VERSAT_SW_DIR)/templates
VERSAT_COMPILER_DIR:=$(VERSAT_SW_DIR)/compiler

BUILD_DIR:=$(VERSAT_DIR)/build
_a := $(shell mkdir -p $(BUILD_DIR))
