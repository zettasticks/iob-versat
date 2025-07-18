#pragma once

#include "embeddedData.hpp"

#include "utils.hpp"
#include "parser.hpp"

struct Arena;
struct SymbolicExpression;

typedef Range<Expression*> ExpressionRange;

struct MacroDefinition{
  String content;
  Array<String> functionArguments;
};

enum VersatStage{
  VersatStage_COMPUTE, // The default is Compute
  VersatStage_READ,
  VersatStage_WRITE
};

struct PortDeclaration{
  Hashmap<String,Value>* attributes;
  ExpressionRange range;
  String name;
  enum {INPUT,OUTPUT,INOUT} type;
};

struct ParameterExpression{
  String name;
  Expression* expr;
};

struct Module{
  Array<ParameterExpression> parameters;
  Array<PortDeclaration> ports;
  String name;
  bool isSource;
};

struct Wire{
  String name;
  int bitSize;
  VersatStage stage;
  bool isStatic;
  SymbolicExpression* sizeExpr;
};

struct WireExpression{
  String name;
  ExpressionRange bitSize;
  VersatStage stage;
  bool isStatic;
};

enum ExternalMemoryType{TWO_P = 0,DP}; // Two ports: one input and one output (think FIFO). Dual port: two input or output ports (think LUT)

// TODO: Because we changed memories to be byte space instead of symbol space, maybe it would be best to change how the address bit size is stored. These structures are supposed to be clean, and so the parser should identify any differences in address size and report an error. These structures should only have one address if we keep going with the byte space memories idea and the data size is used to calculate the bitSize for each respective port
template<typename T>
struct ExternalMemoryTwoPortsTemplate{ // tp
  T bitSizeIn;
  T bitSizeOut;
  T dataSizeIn;
  T dataSizeOut;
};

typedef ExternalMemoryTwoPortsTemplate<ExpressionRange> ExternalMemoryTwoPortsExpression;

template<typename T>
struct ExternalMemoryDualPortTemplate{ // dp
  T bitSize;
  T dataSizeIn;
  T dataSizeOut;
};

typedef ExternalMemoryDualPortTemplate<int> ExternalMemoryDualPort;
typedef ExternalMemoryDualPortTemplate<ExpressionRange> ExternalMemoryDualPortExpression;

template<typename T>
struct ExternalMemoryTemplate{
  union{
	ExternalMemoryTwoPortsTemplate<T> tp;
	ExternalMemoryDualPortTemplate<T> dp[2];
  };
};

typedef ExternalMemoryTemplate<int> ExternalMemory;
typedef ExternalMemoryTemplate<ExpressionRange> ExternalMemoryExpression;

// TODO: Do not know if it was better if this was a union. There are some differences that we currently ignore because we can always fill the interface with the information that we need. It's just that the code must take those into account, while the union approach would "simplify" somewhat the type system.
template<typename T>
struct ExternalMemoryInterfaceTemplate : public ExternalMemoryTemplate<T>{
  // union{
    // tp;
    // dp[2];
  // };
  ExternalMemoryType type;
  int interface;
};

typedef ExternalMemoryInterfaceTemplate<int> ExternalMemoryInterface;
typedef ExternalMemoryInterfaceTemplate<ExpressionRange> ExternalMemoryInterfaceExpression;

struct ExternalMemoryID{
  int interface;
  ExternalMemoryType type;
};

struct ExternalInfoTwoPorts : public ExternalMemoryTwoPortsExpression{
  bool write;
  bool read;
};

struct ExternalInfoDualPort : public ExternalMemoryDualPortExpression{
  bool enable;
  bool write;
};

struct ExternalMemoryInfo{
  union{
	ExternalInfoTwoPorts tp;
	ExternalInfoDualPort dp[2];
  };
  ExternalMemoryType type;
};

template<> struct std::hash<ExternalMemoryID>{
  std::size_t operator()(ExternalMemoryID const& s) const noexcept{
    std::size_t hash = s.interface * 2 + (int) s.type;
    return hash;
  }
};

inline bool operator==(const ExternalMemoryID& lhs,const ExternalMemoryID& rhs){
  bool res = (memcmp(&lhs,&rhs,sizeof(ExternalMemoryID)) == 0);
  return res;
}

struct ModuleInfo{
  String name;
  Array<ParameterExpression> defaultParameters;
  Array<int> inputDelays;
  Array<int> outputLatencies;
  Array<WireExpression> configs;
  Array<WireExpression> states;
  Array<ExternalMemoryInterfaceExpression> externalInterfaces;
  int nDelays;
  int nIO;
  ExpressionRange memoryMappedBits;
  ExpressionRange databusAddrSize;
  bool doesIO;
  bool memoryMapped;
  bool hasDone;
  bool hasClk;
  bool hasReset;
  bool hasRun;
  bool hasRunning;
  bool isSource;
  bool signalLoop;
};

String PreprocessVerilogFile(String fileContent,Array<String> includeFilepaths,Arena* out);
Array<Module> ParseVerilogFile(String fileContent,Array<String> includeFilepaths,Arena* out); // Only handles preprocessed files
ModuleInfo ExtractModuleInfo(Module& module,Arena* out);

Value Eval(Expression* expr,Array<ParameterExpression> parameters);
