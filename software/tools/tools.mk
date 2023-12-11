include $(VERSAT_DIR)/config.mk
include $(VERSAT_COMMON_DIR)/common.mk
include $(VERSAT_TEMPLATE_DIR)/templates.mk

TYPE_INFO_HDR += $(VERSAT_COMMON_DIR)/utils.hpp
TYPE_INFO_HDR += $(VERSAT_COMMON_DIR)/utilsCore.hpp
TYPE_INFO_HDR += $(VERSAT_COMMON_DIR)/memory.hpp
TYPE_INFO_HDR += $(VERSAT_COMMON_DIR)/templateEngine.hpp
TYPE_INFO_HDR += $(VERSAT_COMMON_DIR)/parser.hpp
TYPE_INFO_HDR += $(VERSAT_COMMON_DIR)/verilogParsing.hpp
TYPE_INFO_HDR += $(VERSAT_COMPILER_DIR)/graph.hpp
TYPE_INFO_HDR += $(VERSAT_COMPILER_DIR)/versat.hpp
TYPE_INFO_HDR += $(VERSAT_COMPILER_DIR)/declaration.hpp
TYPE_INFO_HDR += $(VERSAT_COMPILER_DIR)/accelerator.hpp
TYPE_INFO_HDR += $(VERSAT_COMPILER_DIR)/configurations.hpp

VERSAT_INCLUDE += -I$(VERSAT_PC_DIR) -I$(VERSAT_COMPILER_DIR) -I$(BUILD_DIR)/ -I$(VERSAT_COMMON_DIR)

$(BUILD_DIR)/structParser: $(VERSAT_TOOLS_DIR)/structParser.cpp $(VERSAT_COMMON_OBJ_NO_TYPE)
	-g++ -DPC -std=c++17 -MMD -MP -DVERSAT_DEBUG -DSTANDALONE -o $@ -g $(VERSAT_COMMON_INCLUDE) $< $(VERSAT_COMMON_OBJ_NO_TYPE)

$(BUILD_DIR)/verilogParser: $(VERSAT_TOOLS_DIR)/verilogParser.cpp $(VERSAT_COMMON_OBJ) $(VERSAT_TEMPLATES_OBJ) $(BUILD_DIR)/typeInfo.o
	-g++ -DPC -std=c++17 -MMD -MP -DVERSAT_DEBUG -DSTANDALONE -o $@ -g $(VERSAT_COMMON_INCLUDE) -I$(BUILD_DIR) $< $(VERSAT_COMMON_OBJ) $(VERSAT_TEMPLATES_OBJ) $(BUILD_DIR)/typeInfo.o

$(BUILD_DIR)/embedFile: $(VERSAT_TOOLS_DIR)/embedFile.cpp $(VERSAT_COMMON_OBJ_NO_TYPE)
	-g++ -DPC -std=c++17 -MMD -MP -DVERSAT_DEBUG -DSTANDALONE -o $@ -g $(VERSAT_COMMON_INCLUDE) $< $(VERSAT_COMMON_OBJ_NO_TYPE)

$(BUILD_DIR)/typeInfo.cpp $(BUILD_DIR)/repr.hpp $(BUILD_DIR)/repr.cpp : $(TYPE_INFO_HDR) $(BUILD_DIR)/structParser
	$(BUILD_DIR)/structParser $(BUILD_DIR) $(TYPE_INFO_HDR)

$(BUILD_DIR)/typeInfo.o: $(BUILD_DIR)/typeInfo.cpp
	-g++ -DPC -MMD -MP -std=c++17 -g -c -o $@ $(GLOBAL_CFLAGS) $< $(VERSAT_INCLUDE)

$(BUILD_DIR)/repr.o: $(BUILD_DIR)/repr.cpp $(BUILD_DIR)/repr.hpp
	-g++ -DPC -MMD -MP -std=c++17 -g -c -o $@ $(GLOBAL_CFLAGS) $< $(VERSAT_INCLUDE)
