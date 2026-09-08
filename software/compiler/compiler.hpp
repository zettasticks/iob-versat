#pragma once 

#include "versatSpecificationParser.hpp"
#include "configurations.hpp"
#include "parser.hpp"

#if 1

// Remember, because of stuff like inserting buffers/delays/muxs, we need to be able to write to this.
// Changing name is required. Or maybe not. If we have access to the partitions we could just compute the 
// final name.

// Anyway, lets start small and work from there. The only thing that I want without fail is easy copy.


// COM_Instance needs to contain a bunch of read-only data that needs to be simply copied
// While also containing a bunch of data that can change 

struct COM_Connection;

struct COM_Unit{
  COM_Unit* next;
  COM_Unit* prev;
  COM_Unit* partNext;
  COM_Unit* partPrev;

  String name;
  FUDeclaration* decl;

  bool isShared;
  bool isStatic;
  bool debug;
  int sharedIndex;

#if 0  
  int level;
  FUDeclaration* decl;
  String typeName;
  String parentTypeName;

  int localIndex;

  int id;
  String name;
  String baseName; // NOTE: If the unit does not belong to the merge partition the baseName will equal name.
  //String fullName;

  //Array<Wire> configs;
  //Array<Wire> states;

  //Array<ExternalMemorySymbolic> externalMemory;
  SingleInterfaces singleInterfaces;
  
  Opt<int> globalStaticPos; // Separating static from global makes stuff simpler. If mixing together, do not forget that struct generation cares about source of configPos.
  Opt<int> globalConfigPos;
  Opt<int> localConfigPos;

  //Array<int> individualWiresGlobalStaticPos;
  //Array<int> individualWiresGlobalConfigPos;
  //Array<int> individualWiresLocalConfigPos;
  //Array<bool> individualWiresShared;
  
  //Array<ParamAndValue> params;

  bool isGloballyStatic;
  
  
  Opt<int> statePos;
  
  // Nil if no mem map, 0 if mem mapped with no address bits and any positive number is the number of bits.
  SYM_Expr memMapSym;
  iptr memMapped; // If memMapSym is non nil then this contains the start address

  int memGlobalIndex;
  int memSize;
  //String globalMemDecisionMask;
  int memStart;
  int memEnd;

  Opt<int> delayPos;
  //Array<int> extraDelay;
  int baseNodeDelay;
  int numberDelays;

  // TODO: There are a couple of variables like these that we could just put into an union.
  // Only makes sense on buffer units.
  int variableBufferDelay;

  bool isComposite;
  bool isMerge;

  int nIOs;

  // Sepcific to merge muxs
  bool isMergeMultiplexer;
  int mergePort;
  int muxGroup; // TODO: I think that we can remove muxGroup. We know which units belong or not to a given merge partition and we know their input value so there is no point in keeping the harder to understand and compute muxGroups.

  bool doesNotBelong; // For merge units, if true then this unit does not actually exist for the given partition
  int special;
  int localOrder;
  //FUInstance* inst; // Points to the recon instance for merge declarations.

  //Array<int> inputDelays;
  //Array<int> outputLatencies;
  //Array<int> portDelay;
  int partitionIndex; // TODO: What does this do? Probably a remnant from the old implementation.

  //Array<SimplePortConnection> inputs; 

  //Array<SimplePortInstance> inputsDirectly;
  //Array<bool> outputIsConnected;

  AddressGenInst supportedAddressGen;

  SpecialUnitType specialType;

  StructInfo* structInfo;
#endif

  COM_Connection* outputs;
};
extern COM_Unit COM_Unit_Nil;

// ======================================
// 

enum COM_ExprType{
  COM_ExprType_NIL,
  COM_ExprType_FUNC_CALL,
  COM_ExprType_ARRAY_ACCESS,
  COM_ExprType_VAR,
  COM_ExprType_EXPR
};

// ======================================
// Connections

struct COM_Port{
  COM_Unit* unit;
  int port;
};
extern COM_Port COM_Port_Nil;

struct COM_Connection{
  COM_Connection* next;

  COM_Unit* out;
  int outPort;
  COM_Unit* in;
  int inPort;
  int delay;
};

// ======================================
// Env

enum COM_EntType{
  COM_EntType_NIL,
  COM_EntType_PARAM,
  COM_EntType_MODULE_INPUT,
  COM_EntType_MODULE_UNIT,
  COM_EntType_MODULE_UNIT_ARRAY,

  COM_EntType_ARG_NONE,
  COM_EntType_ARG_FIXED,
  COM_EntType_ARG_DYN,
  COM_EntType_ARG_BUFFER,

  COM_EntType_VAR_WITH_LEFTOVER_RANGE,

  COM_EntType_VAR_WITH_CONFIG,
  COM_EntType_VAR_WITH_STATE,
  COM_EntType_VAR_WITH_VIRTUAL_MEM,
  
};

struct COM_Ent{
  COM_EntType type;
  Token name;

  COM_Unit* unit;
  int currentValue;
  int scope;
  SP_Node* node;
  Direction dir;
  String wireName;
  int port;
};
extern COM_Ent COM_Ent_Nil;

struct COM_EntNode{
  COM_EntNode* next;
  COM_Ent v;
};

struct COM_EntPort{
  COM_Ent ent;
  int port;
};
extern COM_EntPort COM_EntPort_Nil;

struct COM_Env{
  Arena* arena;
  Arena* errorArena;

  int shareIndex;
  int tempIndex;

  COM_Unit* head;
  COM_Unit* tail;

  COM_Connection* conHead;
  COM_Connection* conTail;
  
  int scope;
  COM_EntNode*entFreeList;
  COM_EntNode* entHead;
  COM_EntNode* entTail;
  
  bool anyError;
};

struct COM_ConstantResult{
  int value;
  bool anyError;
  bool nonConstant : 1;
  bool divByZero : 1;
};

// ======================================
// Var Groups

struct COM_ConnectInfo{
  COM_ConnectInfo* next;
  
  int port;
  int delay;
  COM_Ent ent;
};
extern COM_ConnectInfo COM_ConnectInfo_Nil;

struct COM_ConnectInfoList{
  COM_ConnectInfo* head;
  COM_ConnectInfo* tail;
  int size;
};

// ======================================
// 

struct COM_RangeValues{
  int low;
  int high;
  bool error;
  bool constant;
};

// ======================================
// Functions

enum COM_StmtType{
  COM_StmtType_ASSIGN,
  COM_StmtType_ADDR_GEN,
  COM_StmtType_MEM_COPY
};

struct COM_Stmt{
  COM_Stmt* next;

  COM_StmtType type;
  String lhs;
  String rhs;
  AddressAccess* access;
};

// ======================================
// Type stuff

bool IsNil(COM_Ent ent);
bool IsNil(COM_ConnectInfo* con);

bool COM_Ent_IsVar(COM_EntType in);
bool COM_Ent_IsArray(COM_EntType in);

// ======================================
// Constant expressions and computations

SYM_Expr           COM_SymbolicFromExpression(COM_Env* env,SP_Node* expr);
COM_ConstantResult COM_ComputeConstantValue(COM_Env* env,SP_Node* expr);

// ======================================
// Compilation helpers

COM_ConnectInfoList COM_UnpackVarGroup(COM_Env* env,SP_Node* top,Arena* out);
COM_RangeValues     COM_CalculateRange(COM_Env* env,SP_Node* rangeOrExpr,bool mustBeConstant);
COM_Ent             COM_ResolveEntity(COM_Env* env,SP_Node* varAccessNode);
COM_EntPort         COM_InstantiateExpression(COM_Env* env,SP_Node* top,Arena* out);

// ======================================
// Compilation

COM_Unit* COM_InstantiateModule(SP_Node* moduleDef,Array<ParamNameAndValue> topLevelParams,Arena* out);

// ======================================
// Env

COM_Ent* COM_PushEnt(COM_Env* env,Token name,COM_EntType type);
COM_Ent  COM_GetEnt(COM_Env* env,Token name,bool canFail);
COM_Ent  COM_ArrayAccess(COM_Env* env,COM_Ent array,int index);

void COM_PushScope(COM_Env* env);
void COM_PopScope(COM_Env* env);

// ======================================
// Env Connections

void COM_Connect(COM_Env* env,COM_Ent out,int outPort,COM_Ent in,int inPort,int delay,Arena* arenaOut);

// ======================================
// Env error reporting

void COM_ReportError(COM_Env* env,String msg);
void COM_ReportError(COM_Env* env,String msg,SP_Node* top);
void COM_ReportError(COM_Env* env,String msg,Token token);

// ======================================
// Repr

String COM_Repr(COM_Unit* top,Arena* out);

#endif
