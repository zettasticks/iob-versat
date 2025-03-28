#pragma once

#include <vector>
#include <unordered_map>

#include "../common/utils.hpp"
#include "../common/memory.hpp"
#include "../common/logger.hpp"

#include "globals.hpp"
#include "accelerator.hpp"
#include "configurations.hpp"
#include "debugVersat.hpp"

// Forward declarations
struct Accelerator;
struct FUDeclaration;
struct IterativeUnitDeclaration;
struct FUInstance;

enum VersatDebugFlags{
  OUTPUT_GRAPH_DOT,
  GRAPH_DOT_FORMAT,
  OUTPUT_ACCELERATORS_CODE,
  OUTPUT_VERSAT_CODE,
  USE_FIXED_BUFFERS
};

typedef uint GraphDotFormat;
enum GraphDotFormat_{
  GRAPH_DOT_FORMAT_NAME = 0x1,
  GRAPH_DOT_FORMAT_TYPE = 0x2,
  GRAPH_DOT_FORMAT_ID = 0x4,
  GRAPH_DOT_FORMAT_DELAY = 0x8,
  GRAPH_DOT_FORMAT_EXPLICIT = 0x10, // indicates which field is which when outputting name
  GRAPH_DOT_FORMAT_PORT = 0x20, // Outputs port information for edges and port instance
  GRAPH_DOT_FORMAT_LATENCY = 0x40 // Outputs latency information for edges and port instances which know their latency information
};

struct DelayToAdd{
  Edge edge;
  String bufferName;
  String bufferParameters;
  int bufferAmount;
};

namespace BasicTemplates{
  extern CompiledTemplate* acceleratorTemplate;
  extern CompiledTemplate* iterativeTemplate;
  extern CompiledTemplate* topAcceleratorTemplate;
  extern CompiledTemplate* topConfigurationsTemplate;
  extern CompiledTemplate* acceleratorHeaderTemplate;
  extern CompiledTemplate* externalInternalPortmapTemplate;
  extern CompiledTemplate* externalPortTemplate;
  extern CompiledTemplate* externalInstTemplate;
  extern CompiledTemplate* internalWiresTemplate;
}

struct GraphMapping;

// Temp
bool EqualPortMapping(PortInstance p1,PortInstance p2);

// General info
bool IsUnitCombinatorial(FUInstance* inst);

// Graph fixes
Array<DelayToAdd> GenerateFixDelays(Accelerator* accel,EdgeDelay* edgeDelays,Arena* out);
void FixDelays(Accelerator* accel,Hashmap<Edge,DelayInfo>* edgeDelays);

// Accelerator merging
DAGOrderNodes CalculateDAGOrder(Pool<FUInstance>* instances,Arena* out);

// Misc
bool CheckValidName(String name); // Check if name can be used as identifier in verilog
bool IsTypeHierarchical(FUDeclaration* decl);
FUInstance* GetInputInstance(Pool<FUInstance>* nodes,int inputIndex);
FUInstance* GetOutputInstance(Pool<FUInstance>* nodes);

FUDeclaration* RegisterModuleInfo(ModuleInfo* info);

// Accelerator functions
FUInstance* CreateFUInstance(Accelerator* accel,FUDeclaration* type,String entityName);
Pair<Accelerator*,SubMap*> Flatten(Accelerator* accel,int times);

void FillDeclarationWithAcceleratorValues(FUDeclaration* decl,Accelerator* accel);
void FillDeclarationWithDelayType(FUDeclaration* decl);

// Static and configuration sharing
void ShareInstanceConfig(FUInstance* inst, int shareBlockIndex);
void SetStatic(Accelerator* accel,FUInstance* inst);

enum SubUnitOptions{
  SubUnitOptions_BAREBONES,
  SubUnitOptions_FULL
};

// Declaration functions

// nocheckin: Replace RegisterFU with a AllocateFU function.
//            We do not need anything from the declaration during registering, so might as well simplify this part.
FUDeclaration* RegisterFU(FUDeclaration declaration);
FUDeclaration* GetTypeByName(String str);
FUDeclaration* RegisterIterativeUnit(Accelerator* accel,FUInstance* inst,int latency,String name);
FUDeclaration* RegisterSubUnit(Accelerator* circuit,SubUnitOptions options = SubUnitOptions_FULL);

// Helper functions, useful to implement custom units
FUInstance* CreateOrGetInput(Accelerator* accel,String name,int portNumber);
FUInstance* CreateOrGetOutput(Accelerator* accel);

// Helper functions to create sub accelerators
int GetInputPortNumber(FUInstance* inputInstance);

#include "typeSpecifics.inl"
