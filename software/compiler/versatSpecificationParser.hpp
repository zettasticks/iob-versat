#pragma once

#include "merge.hpp"

#include "embeddedData.hpp"

struct ConfigFunctionDef;

typedef TrieMap<String,FUInstance*> InstanceTable;

enum PortRangeType{
  PortRangeType_SINGLE,
  PortRangeType_PORT_RANGE,
  PortRangeType_ARRAY_RANGE,
  PortRangeType_DELAY_RANGE,
  PortRangeType_ERROR
};

// TODO: Remove Connection extra. Store every range inside Var.
struct ConnectionExtra{
  Range<int> port;
  Range<int> delay;
};

struct Var{
  Token name;
  ConnectionExtra extra;
  Range<int> index;
  bool isArrayAccess;
};

struct VarGroup{
  Array<Var> vars;
  Token fullText;
};

enum SpecType{
  SpecType_OPERATION,
  SpecType_VAR,
  SpecType_LITERAL
};

struct SpecExpression{
  Array<SpecExpression*> expressions;
  union{
    const char* op;
    Var var;
    Value val;
  };
  Token text;
  
  enum {OPERATION,VAR,LITERAL} type;
};

struct VarDeclaration{
  Token name;
  int arraySize;
  bool isArray;
};

struct GroupIterator{
  VarGroup group;
  int groupIndex;
  int varIndex; // Either port, delay or array unit.
};

struct PortExpression{
  FUInstance* inst;
  ConnectionExtra extra;
};

enum InstanceDeclarationType{
  InstanceDeclarationType_NONE,
  InstanceDeclarationType_STATIC,
  InstanceDeclarationType_SHARE_CONFIG
};

// TODO: It's kinda bad to group stuff this way. Just because the spec parser groups everything, does not mean that we need to preserve the grouping. We could just parse once and create N different instance declarations for each instance (basically flattening the declarations) instead of preserving this grouping and probably simplifying stuff a bit.
//       The problem is that we would have to put some sharing logic inside the parser. Idk. Push through and see later if any change is needed.
struct InstanceDeclaration{
  InstanceDeclarationType modifier;
  Token typeName;
  Array<VarDeclaration> declarations; // share(config) groups can have multiple different declarations. TODO: It is kinda weird that inside the syntax, the share allows groups of instances to be declared while this does not happen elsewhere. Not enought to warrant a look for now, but keep in mind for later.
  Array<Pair<String,SymbolicExpression*>> parameters;
  Array<Token> addressGenUsed; // NOTE: We do not check if address gen exists at parse time, we check it later.
  Array<Token> shareNames;
  bool negateShareNames;
  bool debug;
  
  // Set later
  int shareIndex;
};

#if 1
enum ConnectionType{
  ConnectionType_EQUALITY,
  ConnectionType_CONNECTION
};
#endif

struct ConnectionDef{
  ConnectionType type;

  Range<Cursor> loc;

  VarGroup output;

  Array<Token> transforms;
  
  union{
    VarGroup input;
    SpecExpression* expression;
  };
};

struct TypeAndInstance{
  Token typeName;
  Token instanceName;
};

struct DefBase{
  Token name;
};

struct ModuleDef : public DefBase{
  Token numberOutputs; // TODO: Not being used. Not sure if we gonna actually add this or not.
  Array<VarDeclaration> inputs;
  Array<InstanceDeclaration> declarations;
  Array<ConnectionDef> connections;
  Array<ConfigFunctionDef> configs;
};

struct MergeDef : public DefBase{
  Array<TypeAndInstance> declarations;
  Array<SpecificMergeNode> specifics;
  Array<Token> mergeModifiers;
};

struct AddressGenForDef;

struct AddressGenDef : public DefBase{
  AddressGenType type;

  Array<Token> inputs;
  Array<AddressGenForDef> loops;
  Array<Token> symbolicTokens;
};

struct ConstructDef{
  ConstructType type;
  union {
    DefBase base;
    ModuleDef module;
    MergeDef merge;
    AddressGenDef addressGen;
  };
};

struct Transformation{
  int inputs;
  int outputs;
  Array<int> map;
};

struct HierarchicalName{
  Token instanceName;
  Var subInstance;
};

typedef Pair<HierarchicalName,HierarchicalName> SpecNode;

void ReportError(String content,Token faultyToken,String error);
void ReportError2(String content,Token faultyToken,Token goodToken,String faultyError,String good);

bool IsModuleLike(ConstructDef def);
Array<Token> TypesUsed(ConstructDef def,Arena* out);
Array<Token> AddressGenUsed(ConstructDef def,Array<ConstructDef> allConstructs,Arena* out);

Array<ConstructDef> ParseVersatSpecification(String content,Arena* out);

// TODO: Move this function to a better place, no reason to be inside spec parser
FUDeclaration* InstantiateSpecifications(String content,ConstructDef def);

Opt<AddressGenDef> ParseAddressGen(Tokenizer* tok,Arena* out);


// ======================================
// Hierarchical access (WIP)

// Map from name in hierarchical access (ex: a.b.c[0].d) to an entity. The VARIABLE part is that this mapping is also used inside the parser to map stuff to things like variables or to special names that are specific to a given part of the code.

enum EntityType{
  EntityType_FU,
  EntityType_FU_ARRAY,
  EntityType_NODE,
  EntityType_CONFIG_WIRE,
  EntityType_STATE_WIRE,
  EntityType_CONFIG_FUNCTION,
  EntityType_VARIABLE_INPUT,
  EntityType_VARIABLE_SPECIAL // For variables that exist "by default"
};

enum VariableType{
  VariableType_VOID_PTR,
  VariableType_INTEGER
};

struct Entity{
  EntityType type;

  // TODO: Union
  //union {
  InstanceInfo* info;
  FUInstance* instance;

  bool isInput;

  Wire* wire;

  ConfigFunction* func;
  String varName;

  int arraySize;
  String arrayBaseName;

  VariableType varType;
  //};
};

enum ScopeType{
  ScopeType_CONFIG_FUNCTION,
  ScopeType_FOR_LOOP
};

struct EnvScope{
  ArenaMark mark;

  //TrieMap<String,VariableType>* variableTypes;
  TrieMap<String,Entity>* variable;
};

// Env is more of a parser related thing than it is an accelerator related thing.
// Need to copy this to a better place and start using it in other parts of the code that should use it.
// NOTE: This is more like a FUDeclaration builder than an environment, I think
// NOTE: Make the easy changes first and then see what happens.
struct Env{
  Arena* scopeArena;
  Arena* miscArena;

  // Store errors in here.
  ArenaList<String>* errors;
  Accelerator* circuit;

  InstanceTable* table;

  int insertedInputs;

  Array<EnvScope*> scopes;
  int currentScope;

  void ReportError(Token badToken,String msg);

  // By default we are inside a module scope.
  void PushScope();
  void PopScope();

  FUInstance* CreateInstance(FUDeclaration* type,String name);
  
  FUInstance* GetFUInstance(Var var);
  FUInstance* GetOutputInstance();

  Entity* PushNewEntity(Token name);
  Entity* GetEntity(Token name);

  void AddInput(VarDeclaration decl);
  void AddInstance(InstanceDeclaration decl,VarDeclaration var);

  void AddConnection(ConnectionDef def);
  void AddEquality(ConnectionDef def);

  PortExpression InstantiateSpecExpression(SpecExpression* root);
};

Env* StartEnvironment(Arena* freeUse,Arena* freeUse2);

Opt<Entity> GetEntityFromHierAccess(AccelInfo* info,Array<String> accessExpr);
Opt<Entity> GetEntityFromHierAccessWithEnvironment(AccelInfo* info,Env* env,Array<String> accessExpr);

//void AddOrSetVariable(Env* env,String name,VariableType type);

struct FUInstanceIterator{
  Env* env;
  Entity* ent;
  int index;
  int max;

  FUInstanceIterator Next();
  bool IsValid();
  FUInstance* Current();
};

FUInstanceIterator StartIteration(Env* env,Entity* ent);

