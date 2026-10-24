#pragma once 

#include "versatSpecificationParser.hpp"
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

struct COM_Param{
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
  COM_Unit* mergePrev; // Instead of prev we probably only care about returning to node 0.

 // NOTE: General purpose pointer to help map from units to temporary
  // data (avoids having to create maps all the time). Good practice
  // to zero out after every usage
  void* general;

  // Data provided by parser ====================================================
  String name;
  FUDeclaration* decl;

  bool isStatic;
  bool isShared;
  int sharedIndex;

  int special;
  bool debug;

  // Computed data ==============================================================
  int id;

  int numberInputs; // Some units that support variable inputs have this value different than declaration 

  Array<int> extraDelay; // TODO: Probably not good anymore.

#if 0
  int configPos;
  int statePos;
  int delayPos;
  int numberDelays;

  int mergePort;

  // TODO: 
  Array<int> individualWiresConfigPos;
  Array<bool> individualWiresShared;
#endif

  bool doesNotBelong; // For merge units, if true then this unit does not actually exist for the given partition

  // Delay stuff ================================================================
  // TODO: The only thing that we actually need is to store extra delay, right?
  //       It is the only piece of data that code generation actually needs in order to function.

#if 0
  int calculatedDelay; // TODO: This is not what we actually want, since latency and delays should be calculated based on port info instead of unit. We still keep the same unit delay for now.
  int baseNodeDelay;

  Array<int> inputDelays;
  Array<int> outputLatencies;
  Array<int> portDelay;
#endif

  // Computed modular data ======================================================
  Array<char> memoryMappedInterfaces; // TODO: Type
  Array<char> externalMemories; // TODO: Type
};
extern COM_Unit COM_Unit_Nil;

struct COM_UnitList{
  COM_Unit* head;
  COM_Unit* tail;
};

struct COM_UnitNode{
  COM_UnitNode* next;
  COM_Unit* v;
};

struct COM_UnitNodeList{
  COM_UnitNode* head;
  COM_UnitNode* tail;
  int count;
};

// ======================================
// Connections

struct COM_Port
{
  COM_Unit* unit;
  int port;
};
extern COM_Port COM_Port_Nil;

struct COM_PortNode{
  COM_PortNode* next;
  COM_Port v;
};

struct COM_Edge{
  COM_Edge* next;
  void* general;

  // out:outPort -> in:inPort
  union{
    struct{
      union{
        struct {
          COM_Unit* out;
          int outPort;
        };
        COM_Port conOut;
      };
      union{
        struct {
          COM_Unit* in;
          int inPort;
        };
        COM_Port conIn;
      };
    };
    COM_Port units[2];
  };

  int delay;
};

struct COM_EdgeList{
  COM_Edge* head;
  COM_Edge* tail;
};

// ======================================
// Graph

struct COM_Graph{
  COM_UnitList units;
  COM_EdgeList edges;

  COM_Unit* freeUnits;
  COM_Edge* freeEdges;
};

struct COM_UnitMap{
  COM_UnitMap* next;
  COM_Unit* unit0;
  COM_Unit* unit1;
};

struct COM_EdgeMap{
  COM_EdgeMap* next;
  COM_Edge* edge0;
  COM_Edge* edge1;
};

struct COM_CopyResult{
  COM_Graph result;
  // Maps from input to output
  COM_EdgeMap* edgeMapping;
  COM_UnitMap* unitMapping;
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
  
  // Var.Wire
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
  int delay;
};
extern COM_EntPort COM_EntPort_Nil;

struct COM_Env{
  Arena* outArena;
  Arena* arena;
  Arena* errorArena;

  int shareIndex;
  int tempIndex;

  COM_Graph graph;
 
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

// nocheckin
#if 0
enum COM_UnpackedType{
  COM_UnpackedType_NIL,
  COM_UnpackedType_FUNC_CALL,
  COM_UnpackedType_ARRAY_ACCESS,
  COM_UnpackedType_VAR,
  COM_UnpackedType_WIRE,
  COM_UnpackedType_EXPR,
  COM_UnpackedType_NAME
};

struct COM_UnpackedExpr{
  COM_UnpackedType type;
  
  COM_Ent ent;
  SP_Node* expr;
  String name;
};
#endif

// ======================================
// User Functions

enum COM_StmtType{
  COM_StmtType_NIL,
  COM_StmtType_ASSIGN,
  COM_StmtType_ADDR_GEN,
  COM_StmtType_MEM_COPY
};

struct COM_Stmt{
  COM_Stmt* next;
  COM_StmtType type;

  // TODO: Probably gonna need to change into hierarchical names.
  String lhs;
  String rhs;
  AccessAndType access;
};

enum COM_ArgType{
  COM_ArgType_NIL,
  COM_ArgType_SIMPLE,
  COM_ArgType_ADDR,
};

struct COM_Arg{
  COM_ArgType type;
  String name;
};

enum COM_FunctionType{
  COM_FunctionType_NIL,
  COM_FunctionType_CONFIG,
  COM_FunctionType_STATE,
  COM_FunctionType_MEM
};

struct COM_Function{
  COM_FunctionType type;
  String name;
  Array<COM_Arg> args;
  COM_Stmt* stmts;
};

// What is the most raw form of storing this stuff?
// Names are hierarchical names, address gen are address gen, simple calculations.

struct COM_FunctionNode{
  COM_FunctionNode* next;
  COM_Function v;
};

// ======================================
// Compiled module

struct COM_Module{
  String name;
  COM_Graph graph;
  COM_FunctionNode* funcs;
};

// ======================================
// Type stuff

bool IsNil(COM_Ent ent);
bool IsNil(COM_ConnectInfo* con);

// nocheckin
//bool IsNil(COM_UnpackedExpr in);

bool COM_Ent_IsVar(COM_EntType in);
bool COM_Ent_IsArray(COM_EntType in);
bool COM_Ent_IsExpr(COM_EntType in);
bool COM_Ent_IsWire(COM_EntType in);

void           COM_Copy(COM_Unit* out,COM_Unit* in);
COM_Unit*      COM_Copy(COM_Unit* unit,Arena* out);
COM_CopyResult COM_Copy(COM_Graph in,bool computeMapping,Arena* out);

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

// nocheckin
//COM_UnpackedExpr COM_UnpackExpr(COM_Env* env,SP_Node* expr);
//COM_UnpackedExpr COM_CollapseToName(COM_UnpackedExpr expr,Arena* out);

// ======================================
// Compilation

COM_Module COM_InstantiateModule(SP_Node* moduleDef,Array<COM_Param> topLevelParams,Arena* out);

// ======================================
// Env

COM_Ent* COM_PushEnt(COM_Env* env,String name,COM_EntType type);
COM_Ent* COM_PushEnt(COM_Env* env,Token name,COM_EntType type);
COM_Ent  COM_GetEnt(COM_Env* env,Token name,bool canFail);
COM_Ent  COM_ArrayAccess(COM_Env* env,COM_Ent array,int index);

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

typedef String COM_ReprFunction(COM_Unit* node,COM_Edge* edge,Arena* out);
inline String COM_DefaultReprFunction(COM_Unit* node,COM_Edge* edge,Arena* out){return {};};

String COM_DebugPushDotGraph(String name,COM_Unit* units,COM_Edge* edges,Arena* out,COM_ReprFunction repr = COM_DefaultReprFunction);
void   COM_DebugOutputDotGraph(String filepath,String name,COM_Unit* units,COM_Edge* edges,COM_ReprFunction repr = COM_DefaultReprFunction);

// ======================================
// Graph helpers

COM_Unit* COM_GetInputUnit(COM_Graph graph,int index);
COM_Unit* COM_GetOutputUnit(COM_Graph graph);

COM_Unit* COM_PushUnit(COM_Graph* graph,String name,FUDeclaration* decl,Arena* out);
COM_Edge* COM_PushEdge(COM_Graph* graph,Arena* out);
COM_Edge* COM_InsertMiddle(COM_Graph* graph,COM_Edge* edge,COM_Unit* toInsert,int inPort,int outPort,Arena* out);

void      COM_ClearGeneralPtr(COM_Unit* units);
void      COM_ClearGeneralPtr(COM_Edge* edges);

// ======================================
// Graph algorithms

struct COM_EdgeDelay{
  COM_EdgeDelay* next;
  COM_Edge* edge;
  int delay;
  bool isAny;
};

struct COM_PortDelay{
  COM_PortDelay* next;

  COM_Unit* unit;
  int port;
  Direction dir;
  int latency;
};

struct COM_DelayResult{
  COM_EdgeDelay* edgeDelays;
  COM_PortDelay* portDelays;
  Array<int> inputDelays;
  Array<int> outputLatencies;
};

COM_UnitNodeList COM_CalculateDAGOrder(COM_Graph graph,Arena* out);
COM_DelayResult  COM_CalculateDelay(COM_Graph graph,COM_UnitNodeList order,Arena* out);
COM_Graph        COM_Flatten(COM_Graph in,Arena* out);
