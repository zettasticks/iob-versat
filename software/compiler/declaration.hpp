#pragma once

#include "utils.hpp"
#include "verilogParsing.hpp"
#include "configurations.hpp"

#include "addressGen.hpp"

#include "hardwareInterfaces.hpp"

#include "declaration_meta.hpp"

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

struct DECL_Param{
  String name;
  SYM_Expr v;
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
  Array<DECL_Param> parameters; // All parameters even if equal to default values.

  String name; // NOTE: Serialized if parameters affect verilog or C code.

  // Interfaces =================================================================
  Array<DECL_PortInfo> inputs;
  Array<DECL_PortInfo> outputs;
  Array<Wire> configs;
  Array<Wire> states;
  int numberDelays;
  Array<SYM_Expr> memoryMapped; // We only care about address size, right? Everything else must be standard. For now.
  Array<HW_Instance> externalMemory;
  
  // For external memory we care about wire size. 
  String operation;
  DECL_SingleInterface singleInterfaces;

  // Graph related data for modular units =======================================
  COM_Unit* units;
  COM_Edge* edges;

  AddressGenInst supportedAddressGen;
  DeclarationType type;

  bool error;
};
extern FUDeclaration FUDeclaration_Nil;

struct FUDeclarationNode{
  FUDeclarationNode* next;
  FUDeclaration val;
};

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

struct DECL_Meta{
  DECL_Meta* next;
  DECL_MetaType type;
  String name;

  Array<DECL_Param> params;

  bool anyError;

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
// Register (does not exist)

// Returns true if already exists
// TODO: Could we just reuse the node struct and have everything use the same struct?
bool DECL_RegisterMeta(String name,V_Node* module);
bool DECL_RegisterMeta(String name,SP_Node* module);

FUDeclaration* DECL_RegisterFU(String name);

// ======================================
// Get or create if needed

FUDeclaration* DECL_GetType(String name,Array<DECL_Param> params);

// ======================================
// Instantiation

FUDeclarationNode* DECL_InstantiateSimple(DECL_Meta* meta,Array<DECL_Param> normalizedParams);
