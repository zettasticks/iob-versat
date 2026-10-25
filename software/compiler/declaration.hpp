#pragma once

#include "utils.hpp"
#include "verilogParsing.hpp"
#include "configurations.hpp"

#include "addressGen.hpp"

#include "hardwareInterfaces.hpp"

#include "compiler.hpp"
#include "declaration_meta.hpp"

/*

TODO: SYM_Expr that we have inside FUDeclaration cannot depend on identifiers except if they are DECL_Meta params. Any identifier that is not a parameter would lead to wrong verilog code since the identifier would not exist in Verilog.

Basically configs.size,state.size,mm,2p,dp sizes can only have identifiers contained in the set of params. Any other identifier would lead to a verilog error code since the indentifier would not exist.

We need to check this and properly report error.

*/

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
  SYM_Expr val;
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
  Array<int> inputDependencies; // empty means depends on all inputs.
};

enum DECL_MetaType{
  DECL_MetaType_NIL,
  DECL_MetaType_SIMPLE,
  DECL_MetaType_MODULE,
  DECL_MetaType_MERGE
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

struct DECL_StaticWire{
  DECL_Meta* meta;
  String unitName;
  Wire wire;
};

struct FUDeclaration{
  DECL_Meta* meta;
  Array<DECL_Param> parameters; // All parameters even if equal to default values.

  String name; // NOTE: Serialized if parameters affect verilog or C code.

  // Interfaces =================================================================
  Array<DECL_PortInfo> inputs;
  Array<DECL_PortInfo> outputs;
  Array<Wire> configs;
  Array<Wire> states;
  Array<HW_Instance> memoryMapped; // Only address can change, everything else must be standard
  Array<HW_Instance> externalMemory;  // For external memory we care about wire size. 

  Array<DECL_StaticWire> statics;

  // NOTE: Delays have multiple jobs. For source units they encode the number of cycles unit must wait before outputting
  //       For sink the cycles to wait before starting to receive.
  //       Compute units might have delays since they care about when the data is valid.
  int numberDelays;

  String operation;
  int numberDatabus;
  int numberDones;
  DECL_SingleInterface singleInterfaces;
  AddressGenInst supportedAddressGen;
  DeclarationType type;

  // Graph related data for modular units =======================================
  COM_Graph baseGraph;
  COM_Graph fixedDelays;

  COM_FunctionNode* funcs;

  bool isSource;
  bool error;
};
extern FUDeclaration FUDeclaration_Nil;

struct FUDeclarationNode{
  FUDeclarationNode* next;
  FUDeclaration* val;
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

// ======================================
// Type

bool IsNil(FUDeclaration* decl);
u64  Hash(DECL_StaticWire in);
bool Equal(DECL_StaticWire lhs,DECL_StaticWire rhs);

bool DECL_IsInput(FUDeclaration* decl);
bool DECL_IsOutput(FUDeclaration* decl);

bool DECL_IsSource(FUDeclaration* decl);
bool DECL_IsCompute(FUDeclaration* decl);
bool DECL_IsSink(FUDeclaration* decl);
bool DECL_IsVersatDefault(FUDeclaration* decl);
bool DECL_IsVersatGenerated(FUDeclaration* decl);
bool DECL_IsVersatOperation(FUDeclaration* decl);
bool DECL_IsComposite(FUDeclaration* decl);

// NOTE: Units that can have any number of inputs. Not predefined.
bool DECL_HasVariableInputs(FUDeclaration* decl);

// ======================================
// Init

void DECL_Init();

// ======================================
// Helpers

Array<DECL_Param> DECL_Copy(Array<DECL_Param> in,Arena* out);

// Returns true if already exists
// TODO: Could we just reuse the node struct and have everything use the same struct?
bool DECL_RegisterMeta(String name,V_Node* module);
bool DECL_RegisterMeta(String name,SP_Node* module);

FUDeclaration* DECL_RegisterFU(String name,DeclarationType type);

// ======================================
// Get or create if needed

FUDeclaration* DECL_GetType(String name,Array<DECL_Param> params);
FUDeclaration* DECL_GetBufferType(int delay);
FUDeclaration* DECL_GetVariableBufferType();

// ======================================
// Instantiation

FUDeclaration* DECL_InstantiateSimple(DECL_Meta* meta,Array<DECL_Param> normalizedParams);
FUDeclaration* DECL_InstantiateModule(DECL_Meta* meta,Array<DECL_Param> normalizedParams);
FUDeclaration* DECL_InstantiateMerge(DECL_Meta* meta,Array<DECL_Param> normalizedParams);
