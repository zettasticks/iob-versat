#pragma once

#include "declaration.hpp"
#include "merge.hpp"

#include "embeddedData.hpp"
#include "userConfigs.hpp"

struct ConfigFunctionDef;
struct SpecExpression;
struct CAST;

typedef TrieMap<String,FUInstance*> InstanceTable;

enum PortRangeType{
  PortRangeType_SINGLE,
  PortRangeType_PORT_RANGE,
  PortRangeType_ARRAY_RANGE,
  PortRangeType_DELAY_RANGE,
  PortRangeType_ERROR
};

struct ConnectionExtra{
  Range<MathExpression*> port;
  Range<MathExpression*> delay;
};

struct Var{
  Token name;

  ConnectionExtra extra;
  Array<Range<MathExpression*>> index;

  bool IsArrayAccess(){return index.size > 0;}
  
  // TODO: Is this needed?
  //bool isArrayAccess;
};

struct VarGroup{
  Array<Var> vars;
};

enum SpecOperation{
  SpecOperation_NIL,

  SpecOperation_ADD,
  SpecOperation_SUB,
  SpecOperation_MUL,
  SpecOperation_DIV,
  SpecOperation_NOT,
  SpecOperation_AND,
  SpecOperation_OR,
  SpecOperation_XOR,
  SpecOperation_RHR,
  SpecOperation_SHR,
  SpecOperation_RHL,
  SpecOperation_SHL
};

enum SpecType{
  SpecType_OPERATION,
  SpecType_VAR,
  SpecType_NAME,
  SpecType_LITERAL,
  SpecType_SINGLE_ACCESS,
  SpecType_ARRAY_ACCESS,
  SpecType_FUNCTION_CALL
};

// NOTE: Other than binary and unary operations, common to most languages, we also support "reduced" forms of binary operations.
//       This is operations like '+(a[0..10])' that perform a reduce like operation. For these we do not store expressions, instead we only support this for Vars. This means that expressions has a size of 0 for these ones.

struct SpecExpression{
  Array<SpecExpression*> expressions;
  Array<Var> varArgs;

  SpecOperation op;
  Var var;
  int val;
  Token name;
  
  // NOTE: If array access, expressions is an array of the expressions in order and var contains the array name.
  SpecType type;
};

enum MathOperation{
  MathOperation_NIL,

  MathOperation_ADD,
  MathOperation_SUB,
  MathOperation_MUL,
  MathOperation_DIV
};

enum MathType{
  MathType_OPERATION,
  MathType_NAME,
  MathType_LITERAL,
  MathType_ACCESS, // A.B type of expression
  MathType_ARRAY_ACCESS, // Array access contains the all the accesses in a row (A[1][2][3] will contain 3 expressions)
  MathType_FUNCTION_CALL
};

static inline bool MathType_IsSymbolic(MathType in){return (in == MathType_NAME || in == MathType_LITERAL || in == MathType_OPERATION);}

struct MathExpression{
  Array<MathExpression*> expressions;

  MathOperation op;
  int val;
  Token name;
  
  MathType type;
};

//TODO: We probably want to remove this after we move more logic to Env
Array<Token> AccumTokens(MathExpression* top,Arena* out);

struct VarDeclaration{
  Token name;
  Array<MathExpression*> arrayDims;
};

struct ParameterDeclaration{
  Token name;
  MathExpression* defaultValue;
};

struct PortExpression{
  FUInstance* inst;
  ConnectionExtra extra;
};

enum InstanceDeclarationType{
  InstanceDeclarationType_NONE = 0,
  InstanceDeclarationType_STATIC = 1,
  InstanceDeclarationType_SHARE_CONFIG = 2
};

struct InstanceDeclaration{
  InstanceDeclarationType modifier;
  Token typeName;
  Array<VarDeclaration> declarations; // share(config) groups can have multiple different declarations. TODO: It is kinda weird that inside the syntax, the share allows groups of instances to be declared while this does not happen elsewhere. Not enought to warrant a look for now, but keep in mind for later.

  // NOTE: We could create a different expression type 
  Array<Pair<String,MathExpression*>> parameters;
  Array<ParamNameAndValue> metaParams;

  Array<Token> shareNames;
  bool negateShareNames;
  bool debug;
  bool sim;
  
  // Set later
  int shareIndex;
};

enum ConnectionType{
  ConnectionType_NONE,
  ConnectionType_LOOP,
  ConnectionType_EQUALITY,
  ConnectionType_CONNECTION
};

struct ConnectionDef{
  ConnectionType type;
  VarGroup output;

  // Loops
  Token loopVar;
  MathExpression* loopStart;
  MathExpression* loopEnd;
  Array<ConnectionDef*> loopExpressions;

  // TODO: Union.
  VarGroup input;
  SpecExpression* expression;
};

struct TypeAndInstance{
  Token typeName;
  Token instanceName;
  Array<ParamNameAndValue> params;
};

struct DefBase{
  Token name;
};

struct SP_Node; // nocheckin

struct ModuleDef : public DefBase{
  Token numberOutputs; // TODO: Not being used. Not sure if we gonna actually add this or not.
  Array<ParameterDeclaration> params;
  Array<VarDeclaration> inputs;
  Array<InstanceDeclaration> declarations;
  Array<ConnectionDef*> connections;
  Array<ConfigFunctionDef> configs;
  SP_Node* node;
};

struct MergeDef : public DefBase{
  Array<TypeAndInstance> declarations;
  Array<SpecificMergeNode> specifics;
  Array<Token> mergeModifiers;
};

struct ConstructDef{
  ConstructType type;
  union {
    DefBase base;
    ModuleDef module;
    MergeDef merge;
  };
  SP_Node* node;
};

struct HierarchicalName{
  Token instanceName;
  Var subInstance;
};

typedef Pair<HierarchicalName,HierarchicalName> SpecNode;

bool IsModuleLike(ConstructDef def);
Array<Token> TypesUsed(ConstructDef def,Arena* out);

Array<ConstructDef> ParseVersatSpecification(String content,Arena* out);

FUDeclaration* InstantiateModule(String content,ModuleDef def,Array<ParamNameAndValue> params = {});

// TODO: Move this function to a better place, no reason to be inside spec parser
FUDeclaration* InstantiateSpecifications(String content,ConstructDef def);

// TODO Move to a better place after we probably remove the userConfigs.hpp file.
// TODO: We cannot represent an array access followed by a wireOrPort access.
//       This is probably not the best way to proceed.
enum ConfigIdentifierType{
  ConfigIdentifierType_BASE,
  ConfigIdentifierType_ACCESS,
  ConfigIdentifierType_ARRAY,
  ConfigIdentifierType_FUNC_CALL
};

// TODO: Probably rename this.
// ConfigIdentifier is parsed in reverse order than a normal AST. Name appears first and expressions appear after.
struct ConfigIdentifier{
  ConfigIdentifierType type;
  ConfigIdentifier* next;

  union{
    Token name;
    Token functionName;
  }; 

  MathExpression* arrayExpr;
  Array<MathExpression*> arguments;
};

// ======================================
// Environment

// Map from name in hierarchical access (ex: a.b.c[0].d) to an entity.

enum EntityType{
  EntityType_NIL,
  EntityType_FU,
  EntityType_FU_ARRAY,
  EntityType_PARAM,
  EntityType_GEN_VALUE, // Gen value depends on currently gen variable value.
  EntityType_MEM_PORT, // User can "represent" a memory port by doing something like mem.in0 (input port 0).
  EntityType_ACCESS_EXPR,
  EntityType_CONFIG_WIRE,
  EntityType_STATE_WIRE,
  EntityType_FUNCTION,
  EntityType_VARIABLE_INPUT,
  EntityType_VARIABLE_SPECIAL, // For variables that exist "by default"
  EntityType_SYM,
  EntityType_RUNTIME_COMPUTATION // Special values that need extensive runtime computation are replaced by a simple identifier which is then used in the symbolic expressions, while we retain info about the type of computation which is then perform at runtime.
};

enum EntityVarFlags{
  EntityVarFlags_NONE = 0,
  EntityVarFlags_ADDRESS = (1 << 1)
};

struct Entity{
  EntityType type;
  EntityVarFlags flags;

  // TODO: Would really help if we separated token into a text position and the string part since there are a lot of entities that do not have a text position but have a name 
  Token name;
  //String name;
  Direction dir;
  SYM_Expr size;
  FUInstance* inst;
  ConfigFunction* func;
  FUDeclaration* decl;
  SYM_Expr sym;
  Array<int> dims;
  
  String functionName;
  Array<SYM_Expr> args;

  int arrayDims; // This is probably just a symptom of trying to only support 1D arrays. But since we need to generate C code this is probably for the best.
  int port;
  int val;
  bool isInput;
};

static bool Entity_IsArray(Entity in){
  bool res = false;
  res |= (in.type == EntityType_FU_ARRAY);
  res |= (in.type == EntityType_VARIABLE_INPUT && in.arrayDims > 0);
  return res;
}

Entity MakeEntity(FUInstance* inst);

extern Entity Entity_Nil;

bool Nil(Entity ent);

enum EnvScopeType{
  EnvScopeType_GLOBAL,
  EnvScopeType_FUNCTION,
  EnvScopeType_FOR_LOOP
};

struct EnvScope{
  ArenaMark mark;
  EnvScopeType type;

  TrieMap<String,Entity>* variable;
};

struct EntityAndLeftoverAccess2{
  Array<Entity> accesses;
  Entity first;
  Entity last;
};

struct EntityAccess{
  Array<Entity> entities;
  MathExpression* leftover;
};

struct FUAccess{
  Entity entity;
  Array<MathExpression*> leftovers;
};

struct Env{
  Arena* scopeArena; // Grows and shrinks with scope pushing and poping
  Arena* miscArena; // Always growns and stores a bunch of different things.

  ArenaList<String>* errors;
  Accelerator* circuit;

  InstanceTable* table;

  int insertedInputs;

  Array<EnvScope*> scopes;
  int currentScope;
  
  int currentComputationIndex;
  ArenaList<Entity>* computations;

  void ReportError(Token badToken,String msg);

  // By default we are inside a module scope.
  void PushScope(EnvScopeType type);
  void PopScope();

  FUInstance* CreateInstance(FUDeclaration* type,String name);
  FUInstance* CreateFUInstanceWithDeclaration(FUDeclaration* type,String name,InstanceDeclaration decl);

  // TODO: The arrayIndexIfArray does not tell us if we are trying to access an array or not.
  //       We probably need to encode such info so that we can properly error report
  FUInstance* GetFUInstance(Token name,Array<int> arrayIndexIfArray);

  FUInstance* GetFUInstance(Var var);

  void PushEntity(Token name,Entity ent);
  void PushEntity(String name,Entity ent);

  Entity GetEntityFromAccess(Entity ent,Token access);

  Entity GetEntity(Token name);
  Array<Entity> GetEntity(ConfigIdentifier* id,Arena* out);
  Array<Entity> GetEntity(MathExpression* spec,Arena* out);

  // Only called in a context where we expect an FU. Does not try to peel until it finds one, it already expects one.
  FUAccess ResolveFU(MathExpression* spec,Arena* out);

  Array<int> ConvertRangeToStart(Array<Range<MathExpression*>> range,Arena* out);
  Array<int> ConvertRangeToEnd(Array<Range<MathExpression*>> range,Arena* out);
  
  Array<int> ConvertRangeToIndex(Array<Range<MathExpression*>> range,Arena* out);

  Array<int> CalculateArraySize(Array<MathExpression*> exprs,Arena* out);
  int CalculateConstantExpression(MathExpression* top);

  void AddInput(VarDeclaration decl);
  void AddInstance(InstanceDeclaration decl,VarDeclaration var);

  void AddConnection(ConnectionDef def);
  void AddEquality(ConnectionDef def);

  Entity AddParam(Token name,int val);
  Entity AddVariable(Token name,MathExpression* arraySize = nullptr,EntityVarFlags flags = {});
  
  Entity AddComputation(String functionName,Array<SYM_Expr> expressions);
  Array<Entity> GetAllComputations(Arena* out);

  void SetGenVariable(Token name,int value);

  FUInstance* InstantiateReduction(Var var,FUDeclaration* decl);
  PortExpression InstantiateSpecExpression(SpecExpression* root);

  // TODO: We want to be able to differentiate between a constant expression or not.
  //       Would be useful if we also returned a boolean indicating that the expression
  //       was constant or not.
  SYM_Expr SymbolicFromMathExpression(MathExpression* spec);
};

Env* StartEnvironment(Arena* freeUse,Arena* freeUse2);


// NOTE: Even thought the specs use closed intervals, all the values inside the iterators
//       are half intervals. Close on bottom and open on the top.
//       The StartIteration functions perform the fixup to make sure that everything lines up

struct DimIterator{
  Array<int> endValues;
  Array<int> startValue;
  Array<int> current;

  int Size();
  void Invalidate();

  void Advance();
  bool IsValid();
  Array<int> Current();
};

DimIterator* StartIteration(Array<int> endValues,Array<int> startValues,Arena* out);
DimIterator* StartIteration(int size,Arena* out);

void ArrayIndexIncrementInPlace(Array<int> dims,Array<int> startValue,Array<int> index);

struct FUInstanceIterator{
  Env* env;
  Entity ent;

  bool used;
  DimIterator* iter;

  void Advance();
  bool IsValid();
  FUInstance* Current();
};

FUInstanceIterator StartIteration(Env* env,Entity ent,Arena* out);

struct Connection{
  Token name;

  int port;
  int delay;
  Array<int> arrayIndex;
};

struct VarIterator{
  Token name;
  
  int startPort;
  int currentPort;
  int endPort;

  int startDelay;
  int currentDelay;
  int endDelay;

  DimIterator* arrayIndex;

  int Size();
  void Invalidate();

  void Advance();
  bool IsValid();
  Connection Current();
};

VarIterator* StartIteration(Env* env,Var var,Arena* out);

struct GroupIterator{
  Env* env;
  VarGroup* group;
  Array<VarIterator*> innerIters;
  int currentIter;

  int Size();
  void Invalidate();

  bool IsValid();
  void Advance();
  Connection Current();
};

GroupIterator IterateGroup(Env* env,VarGroup* group,Arena* out);

// Parser stuff ===============================================================
// TODO: Remove all the list nodes, we can just have them be free and the "compiler" will just switch on the base type instead, no need to group stuff like we are currently doing.
#include "versatSpecificationParser_meta.hpp"

struct SP_Node{
  union {
    SP_Node* next;
    SP_Node* first;
  };

  union {
    SP_Node* childs;
    SP_Node* second;
  };

  SP_Type type;
  Token token;
};

struct SP_NodeNode{
  SP_NodeNode* next;
  SP_Node* node;
};

// ======================================
// Type

bool SP_Type_IsLoop(SP_Type in);

// ======================================
// Parsing Helpers

SP_Node* SP_PushNode(Arena* out,SP_Type type,Token token,SP_Node* childs);
#define SP_Append(HEAD,TAIL,NODE) LL_Append(HEAD,TAIL,next,NODE)

// ======================================
// Print

String SP_Repr(SP_Node* top,Arena* out);

// ======================================
// Parsing helpers

SP_Node* ParseExpressionInternal(Parser* parser,Arena* out,int bindingPower);
SP_Node* SP_ParseExpression(Parser* parser,Arena* out);
SP_Node* SP_ParseRange(Parser* parser,Arena* out);
SP_Node* SP_ParseVar(Parser* parser,Arena* out);
SP_Node* SP_ParseVarDeclaration(Parser* parser,Arena* out);
SP_Node* SP_ParseModuleInputDeclaration(Parser* parser,Arena* out);
SP_Node* SP_ParseInstanceDeclaration(Parser* parser,Arena* out);
SP_Node* SP_ParseVarGroup(Parser* parser,Arena* out);
SP_Node* SP_ParseConnection(Parser* parser,Arena* out);
SP_Node* SP_ParseConfigStatements(Parser* parser,Arena* out);
SP_Node* SP_ParseConfigFunction(Parser* parser,Arena* out);

// ======================================
// Parsing

SP_Node* SP_ParseModuleDef(Parser* parser,Arena* out);

// ======================================
// Helpers

SP_Node*     SP_UnpackExpr(SP_Node* exprNode);
SP_NodeNode* SP_Flatten(SP_Node* top,Arena* out);
