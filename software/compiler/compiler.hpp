#pragma once 

#include "versatSpecificationParser.hpp"
#include "declaration.hpp"
#include "symbolic.hpp"
#include "addressGen.hpp"

struct FUDeclaration;

// NOTE: Delay type is not really needed anymore because we can figure out the delay of a unit by: wether it contains inputs and outputs, the position on the graph and if we eventually add (input and output delay) whether it contains those as well.

enum DelayType {
  DelayType_BASE               = 0x0,
  DelayType_SINK_DELAY         = 0x1,
  DelayType_SOURCE_DELAY       = 0x2,
  DelayType_COMPUTE_DELAY      = 0x4
};
C_STYLE_ENUM(DelayType);
#define CHECK_DELAY(inst,T) ((inst->declaration->delayType & T) == T)

struct ParamAndValue{
  String name;
  SYM_Expr val;
};

// Remember, because of stuff like inserting buffers/delays/muxs, we need to be able to write to this.
// Changing name is required. Or maybe not. If we have access to the partitions we could just compute the 
// final name.

// Anyway, lets start small and work from there. The only thing that I want without fail is easy copy.
// If we can copy units easily then we can solve anything later on worst case scenario.

// COM_Instance needs to contain a bunch of read-only data that needs to be simply copied
// While also containing a bunch of data that can change 

struct COM_Unit{
  COM_Unit* next;
  COM_Unit* prev;
  COM_Unit* mergeNext;
  COM_Unit* mergePrev;

  // Data provided by parser ====================================================
  String name;
  FUDeclaration* decl;

  Array<ParamAndValue> params;

  bool isStatic;
  bool isShared;
  int sharedIndex;

  int special;
  bool debug;

  // Computed data ==============================================================
  int id;

  int configPos;
  int statePos;
  int delayPos;
  int numberDelays;

  int mergePort;

  // TODO: 

  Array<int> individualWiresConfigPos;
  Array<bool> individualWiresShared;

  bool doesNotBelong; // For merge units, if true then this unit does not actually exist for the given partition

  // Delay stuff ================================================================

  int calculatedDelay; // TODO: This is not what we actually want, since latency and delays should be calculated based on port info instead of unit. We still keep the same unit delay for now.
  Array<int> extraDelay;
  int baseNodeDelay;

  Array<int> inputDelays;
  Array<int> outputLatencies;
  Array<int> portDelay;

  // Computed modular data ======================================================
  Array<char> memoryMappedInterfaces; // TODO: Type
  Array<char> externalMemories; // TODO: Type
};
extern COM_Unit COM_Unit_Nil;

// ======================================
// Connections

struct COM_Port
{
  COM_Unit* unit;
  int port;
};
extern COM_Port COM_Port_Nil;

struct COM_Edge{
  COM_Edge* next;

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
  COM_EntType_GENERATED_UNIT,

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
  String name;

  Token token;
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
  Arena* outArena;
  Arena* arena;
  Arena* errorArena;

  int shareIndex;
  int tempIndex;

  COM_Unit* unitHead;
  COM_Unit* unitTail;

  COM_Edge* conHead;
  COM_Edge* conTail;
  
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
// Compilation helpers

struct COM_RangeValues{
  int low;
  int high;
  bool error;
  bool constant;
};

enum COM_ExprType{
  COM_ExprType_NIL,
  COM_ExprType_FUNC_CALL,
  COM_ExprType_ARRAY_ACCESS,
  COM_ExprType_VAR,
  COM_ExprType_WIRE,
  COM_ExprType_EXPR,
  COM_ExprType_NAME
};

struct COM_UnpackedExpr{
  COM_ExprType type;
  
  COM_Ent ent;
  SP_Node* expr;
  String name;
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

struct COM_Function{
  COM_Function* next;
  COM_Stmt* stmts;
};

// ======================================
// Compiled module

struct COM_Module{
  String name;
  COM_Unit* units;
  COM_Edge* edges;
  COM_Function* funcs;
};

// ======================================
// Type stuff

bool IsNil(COM_Ent ent);
bool IsNil(COM_ConnectInfo* con);

bool COM_Ent_IsVar(COM_EntType in);
bool COM_Ent_IsArray(COM_EntType in);
bool COM_Ent_IsExpr(COM_EntType in);
bool COM_Ent_IsWire(COM_EntType in);

// ======================================
// Constant expressions and computations

SYM_Expr           COM_SymbolicFromExpression(COM_Env* env,SP_Node* expr);
COM_ConstantResult COM_ComputeConstantValue(COM_Env* env,SP_Node* expr);

// ======================================
// Compilation helpers

COM_ConnectInfoList COM_UnpackVarGroup(COM_Env* env,SP_Node* top,Arena* out);
COM_RangeValues     COM_CalculateRange(COM_Env* env,SP_Node* range);
COM_Ent             COM_ResolveEntity(COM_Env* env,SP_Node* varAccessNode,bool canFail);
COM_EntPort         COM_InstantiateExpression(COM_Env* env,SP_Node* top,Arena* out);
COM_UnpackedExpr    COM_UnpackExpr(COM_Env* env,SP_Node* expr);

// ======================================
// Compilation

COM_Module COM_InstantiateModule(SP_Node* moduleDef,Array<ParamAndValue> topLevelParams,Arena* out);

// ======================================
// Env

COM_Ent* COM_PushEnt(COM_Env* env,String name,COM_EntType type);
COM_Ent* COM_PushEnt(COM_Env* env,Token name,COM_EntType type);
COM_Ent  COM_GetEnt(COM_Env* env,Token name,bool canFail);
COM_Ent  COM_ArrayAccess(COM_Env* env,COM_Ent array,int index);

COM_Unit* COM_PushUnit(COM_Env* env);

void COM_PushScope(COM_Env* env);
void COM_PopScope(COM_Env* env);

// ======================================
// Env Connections

void COM_Connect(COM_Env* env,COM_Ent out,int outPort,COM_Ent in,int inPort,int delay);
void COM_Connect(COM_Env* env,COM_Unit* out,int outPort,COM_Unit* in,int inPort,int delay);

// ======================================
// Env error reporting

void COM_ReportError(COM_Env* env,String msg);
void COM_ReportError(COM_Env* env,String msg,SP_Node* top);
void COM_ReportError(COM_Env* env,String msg,Token token);

// ======================================
// Repr

String COM_Repr(COM_Unit* top,Arena* out);
void   COM_DebugPushDotGraph(String filename,COM_Unit* top,COM_Edge* edges);
