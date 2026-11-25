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

enum DECL_ParamFlags{
  DECL_ParamFlags_NIL = 0,
  // TODO: Proper implementation of this probably requires adding another step where we separate compilation and instantiation into individual parts.
  DECL_ParamFlags_INFLUENCES_GRAPH = (1 << 1)
};

struct DECL_ParamDef{
  String name;
  SYM_Expr defaultVal;
  DECL_ParamFlags flags;
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
  Array<int> inputDependencies; // Empty means depends on all inputs.
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

  Array<DECL_ParamDef> params;

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

struct DECL_MMInfo{
  HW_Instance inst;
  //String addrName;
  HIER_Name addrName;
};

struct DECL_MuxInfo{
  String unitName;
  int port;
};

struct DECL_MergeData{
  String typeName;
  String mergeName;
  
  Array<int> fixedDelays;
  Array<int> variableDelays;
  Array<DECL_MuxInfo> muxConfigs;
};

// nocheckin
struct DECL_MMMapNames{
  int trueIndex;
  HIER_Name name;
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

  Array<DECL_MMInfo> memoryMapped; // Only address can change, everything else must be standard
  Array<DECL_MMMapNames> memoryMappedNames; // nocheckin TODO: Added in a hacky way to resolve merge problem. If we proceed with this approach then take a look at module and simple declarations as well.

  Array<HW_Instance> externalMemory;  // For external memory we care about wire size. 

  Array<DECL_StaticWire> statics;

  // NOTE: Delays have multiple jobs. For source units they encode the number of cycles unit must wait before outputting
  //       For sink the cycles to wait before starting to receive.
  //       Compute units might have delays since they care about when the data is valid.
  //int numberDelays;
  int numberInputDelays;
  int numberOutputDelays;
  Array<int> extraDelays; // NOTE: Only for module units. Merge units use the calculated delays.

  String operation;
  int numberDatabus;
  int numberDones;
  DECL_SingleInterface singleInterfaces;
  AddressGenInst supportedAddressGen;
  DeclarationType type;

  // Graph related data for modular units =======================================
  COM_Graph baseGraph;
  COM_Graph fixedDelays;

  // Merge specific data ========================================================
  Array<DECL_MergeData> mergeData;

  COM_FunctionNode* funcs;

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
}

// ======================================
// Type

bool IsNil(FUDeclaration* decl);
u64  Hash(DECL_StaticWire in);
bool Equal(DECL_StaticWire lhs,DECL_StaticWire rhs);

bool DECL_IsInput(FUDeclaration* decl);
bool DECL_IsOutput(FUDeclaration* decl);

bool DECL_IsSink(FUDeclaration* decl);
bool DECL_IsCompute(FUDeclaration* decl);
bool DECL_IsComputeWithDelay(FUDeclaration* decl);
bool DECL_IsSource(FUDeclaration* decl);

bool DECL_IsVersatDefault(FUDeclaration* decl);
bool DECL_IsVersatGenerated(FUDeclaration* decl);
bool DECL_IsVersatOperation(FUDeclaration* decl);
bool DECL_IsComposite(FUDeclaration* decl);

bool DECL_IsMergeMultiplexer(FUDeclaration* decl);

// NOTE: Units that can have any number of inputs. Not predefined.
bool DECL_HasVariableInputs(FUDeclaration* decl);

// ======================================
// Init

void DECL_Init();

// ======================================
// Helpers

Array<DECL_Param> DECL_Copy(Array<DECL_Param> in,Arena* out);

// ======================================
// Name mangling

String DECL_MangleName(DECL_Meta* meta,Array<DECL_Param> params,bool isTopLevel,Arena* out);

// ======================================
// Meta register

// Returns true if already exists
// TODO: Could we just reuse the node struct and have everything use the same struct?
bool DECL_RegisterMeta(String name,V_Node* module);
bool DECL_RegisterMeta(String name,SP_Node* module);

FUDeclaration* DECL_RegisterFU(String name,DeclarationType type);

// ======================================
// Get or create if needed

FUDeclaration* DECL_GetType(String name,Array<DECL_Param> params,bool isTopLevel = false);
FUDeclaration* DECL_GetBufferType(int delay);
FUDeclaration* DECL_GetVariableBufferType();

// ======================================
// Instantiation

FUDeclaration* DECL_InstantiateSimple(DECL_Meta* meta,Array<DECL_Param> normalizedParams,bool isTopLevel);
FUDeclaration* DECL_InstantiateModule(DECL_Meta* meta,Array<DECL_Param> normalizedParams,bool isTopLevel);
FUDeclaration* DECL_InstantiateMerge(DECL_Meta* meta,Array<DECL_Param> normalizedParams,bool isTopLevel);

// ======================================
// Merge graph getter

COM_Unit* DECL_GetMergeReconStartUnit(FUDeclaration* decl);
