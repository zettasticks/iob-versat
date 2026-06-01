#pragma once

#include "addressGen.hpp"
#include "hierName.hpp"

struct FUDeclaration;
struct AddressAccess;
struct ConfigIdentifier;

// Move user configuration functions here.

// ============================================================================
// Parsed structs only (not validated and no strings is saved)

// TODO: While this exists, we do not want to have different flow if possible. I think that it should be possible to describe the data in such a way that we do not have to make this distinction.
enum UserConfigType{
  UserConfigType_NONE,
  UserConfigType_CONFIG,
  UserConfigType_MEM,
  UserConfigType_STATE
};

enum ConfigStatementType{
  ConfigStatementType_FOR_LOOP,
  ConfigStatementType_GEN_LOOP,
  ConfigStatementType_EQUALITY,
  ConfigStatementType_FUNCTION_CALL
};

inline bool IsLeaf(ConfigStatementType type){return (type == ConfigStatementType_EQUALITY || type == ConfigStatementType_FUNCTION_CALL);}

inline bool IsLoop(ConfigStatementType type){return (type == ConfigStatementType_FOR_LOOP || type == ConfigStatementType_GEN_LOOP);}

struct ConfigStatement{
  ConfigStatementType type;

  // Why have a ConfigIdentifier and a SpecExpression?

  // TODO: Union
  // Statement
  ConfigIdentifier* lhs;
  MathExpression* rhs;

  // Loops
  AddressGenForDef def;
  Array<ConfigStatement*> childs; // Only for loops contains these right now.
};

struct ConfigVarDeclaration{
  Token name;
  Token type;
  MathExpression* arraySize; // If non null then its an array.
};

struct ConfigFunctionDef{
  UserConfigType type;
  
  Token name;
  Array<ConfigVarDeclaration> variables;
  Array<ConfigStatement*> statements;
  bool debug;
};

// ============================================================================
// Instantiation and manipulation

// TODO: We probably want to remove the need for the user to specify this. Stupid to force the user to have to provide a switch when we can just figure out on our side. Still need to take care about the differences between config/mem and state.
enum ConfigFunctionType{
  //ConfigFunctionType_NIL,

  ConfigFunctionType_CONFIG,
  ConfigFunctionType_STATE,
  ConfigFunctionType_MEM
};

enum TransferDirection{
  TransferDirection_NONE = 0,
  TransferDirection_READ = 1,
  TransferDirection_WRITE = 2
};

struct FunctionMemoryTransfer{
  TransferDirection dir;

  SYM_Expr size;
  String variable;
  HIER_Name name;
};

enum ConfigStuffType{
  ConfigStuffType_MEMORY_TRANSFER,
  ConfigStuffType_ADDRESS_GEN,
  ConfigStuffType_ASSIGNMENT
};

// TODO: We probably want to remove this and just pass everything into ConfigStuff.
struct ConfigAssignment{
  String lhs;
  SYM_Expr rhs;
  String rhsId;

  String special;
  bool noAccess; // TODO: For now this is used to indicate that we do not want to access 
};

struct ConfigStuff{
  ConfigStuffType type;

  // TODO: This lhs is only for access. Need to join stuff with assign and access if we eventually cleanup the code.
  HIER_Name lhs;

  String pointerVarName;

  String extraLoopStartAndEndTemplate;

  bool extra;
  SYM_Expr trueStart;
  SYM_Expr trueEnd;
  SYM_Expr unitCount;
  SYM_Expr index;

  union{
    ConfigAssignment assign;
    FunctionMemoryTransfer transfer;
    AccessAndType access;
  };
};

enum ConfigVarType{
  ConfigVarType_SIMPLE,
  ConfigVarType_ADDRESS,
  ConfigVarType_FIXED,
  ConfigVarType_DYN
};

struct ConfigVariable{
  ConfigVarType type;
  String name;
  int arraySize;
  bool usedOnLoopExpressions;
};

struct ConfigFunction{
  ConfigFunctionType type;

  String individualName;
  String fullName;
  FUDeclaration* decl; // Every config function is associated to one declaration only.

  Array<ConfigStuff> stuff;
  Array<ConfigVariable> variables;
  String structToReturnName;
  String stateStructContent;

  bool supportsSizeCalc;

  // TODO: This is only worth it if we can generate extra stuff that might help figure out the problems. Otherwise just use gdb.
  // NOTE: Some stuff that we might want is to profile (gather statistics data) and stuff like that that we can then report at the end of a run.
  bool debug;
};
 
// Can fail (parsed data is validated in here)
// TODO: Instead of passing the content, it would be easier if the function was capable of reporting the errors without having to accesss the text, just by storing the relevant tokens and the upper parts of the code is responsible for reporting it. The language is not that complicated meaning that we are free to just define all the possible errors in a large enum and having a simple structure that stores all the relevant data. 
ConfigFunction* InstantiateConfigFunction(Env* env,ConfigFunctionDef* def,FUDeclaration* declaration,String content,Arena* out);

extern ConfigFunction ConfigFunction_Nil;
