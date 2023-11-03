#
# CORE DEFINITIONS FILE
#

#DATA_W := 32
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

# SUBMODULES
VERSAT_SUBMODULES:=INTERCON MEM DMA
$(foreach p, $(VERSAT_SUBMODULES), $(eval $p_DIR ?=$(VERSAT_SUBMODULES_DIR)/$p))
