#pragma once

#include "utils.hpp"
#include "verilogParsing.hpp"
#include "configurations.hpp"

#include "addressGen.hpp"

struct COM_Unit;
struct COM_Edge;

struct SP_Node;
struct V_Node;

enum SpecialUnitType{
  SpecialUnitType_NONE = 0,
  SpecialUnitType_INPUT = 1,
  SpecialUnitType_OUTPUT = 2,
  SpecialUnitType_FIXED_BUFFER = 3,
  SpecialUnitType_VARIABLE_BUFFER = 4
};

struct Parameter{
  String name;
  SYM_Expr defaultVal;
  ParamFlags flags;
};

struct ParamNameAndValue{
  String name;
  int value;
};

struct ParamNameAndValue2{
  Token name;
  SYM_Expr value;
};

enum DeclarationType{
  DeclarationType_NIL,
  DeclarationType_SINGLE,
  DeclarationType_COMPOSITE,
  DeclarationType_SPECIAL,
  DeclarationType_MERGED,
  DeclarationType_ITERATIVE
};

struct DECL_PortInfo{
  SYM_Expr size;
  int delay;
};

// TODO: A lot of duplicated data exists since the change to merge.

// TODO: There is a lot of crux between parsing and creating the FUDeclaration for composite accelerators 
//       the FUDeclaration should be composed of something that is in common to all of them.
// NOTE: A FUDeclaration represents a concrete type, although the size of stuff might depend on parameters.
//       The general structure is fixed (amount of inputs/outputs and so on) but the size is not.
struct FUDeclaration{
  String metaName;

  String name;
  Array<Parameter> parameters; // Only the parameters that differ from basic declaration

  Array<Wire> configs;
  Array<Wire> states;
  
  Array<DECL_PortInfo> inputs;
  Array<DECL_PortInfo> outputs;

  int numberDelays;
  Array<SYM_Expr> memoryMapped;
  Array<ExternalMemorySymbolic> externalMemory;
  String operation;
  SingleInterfaces singleInterfaces;

  // Graph related data for modular units =======================================
  COM_Unit* units;
  COM_Edge* edges;

  AddressGenInst supportedAddressGen;
  DeclarationType type;
};
extern FUDeclaration FUDeclaration_Nil;

struct FUDeclarationNode{
  FUDeclarationNode* next;
  FUDeclaration val;
};


#if 0

FUDeclaration* RegisterFU(FUDeclaration declaration);

FUDeclaration* GetTypeByName(String str);
FUDeclaration* GetTypeByNameOrFail(String name);

FUDeclaration* GetTypeByName(String str,Array<ParamNameAndValue> metaParams);

String DECL_MangleName(String typeName,Array<ParamNameAndValue> metaParams,Arena* out);

bool HasMultipleConfigs(FUDeclaration* decl);
// Because of merge, we need units that can delay the datapath for different values depending on the datapath that is being configured.

// ======================================
// Declaration inspection

Wire* GetConfigWireByName(FUDeclaration* decl,String name);

#endif

// Simple operations should also be stored here.
namespace BasicDeclaration{
  extern FUDeclaration* nil;

  extern FUDeclaration* input;
  extern FUDeclaration* output;

#if 0
  extern FUDeclaration* variableBuffer;
  extern FUDeclaration* fixedBuffer;
  extern FUDeclaration* multiplexer;
  extern FUDeclaration* combMultiplexer;
  extern FUDeclaration* timedMultiplexer;
  extern FUDeclaration* stridedMerge;
  extern FUDeclaration* pipelineRegister;
#endif
}

enum DECL_MetaType{
  DECL_MetaType_NIL,
  DECL_MetaType_SIMPLE,
  DECL_MetaType_COMPOSITE
};

// TODO: We might implement a validation step for DECL_Meta in the future to make sure
//       that we do not have problems with duplicated parameters and stuff like that.
//       Just preserving the nodes and doing instantiation when needed is a bit clubersome.
struct DECL_Meta{
  DECL_Meta* next;
  DECL_MetaType type;
  String name;

  union{
    V_Node* simpleUnit;
    SP_Node* compositeUnit;
  };
};

// ======================================
// Type

bool IsNil(FUDeclaration* decl);

// ======================================
// Init

void DECL_Init();

// ======================================
// Parameter handling

Array<ParamNameAndValue> DECL_GetDefaultParams(DECL_Meta* meta,Arena* out);

// ======================================
// Register (does not exist)

// Returns true if already exists
// TODO: Could we just reuse the node struct and have everything use the same struct?
bool DECL_RegisterMeta(String name,V_Node* module);
bool DECL_RegisterMeta(String name,SP_Node* module);

FUDeclaration* DECL_RegisterFU(String name);

// ======================================
// Get or create if needed

FUDeclaration* DECL_GetType(String name,Array<ParamNameAndValue> params);

// ======================================
// Instantiation

FUDeclaration* DECL_InstantiateSimple(DECL_Meta* meta,Array<ParamNameAndValue> normalizedParams);
