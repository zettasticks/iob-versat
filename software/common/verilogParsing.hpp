#ifndef INCLUDED_VERILOG_PARSER
#define INCLUDED_VERILOG_PARSER

#include <vector>
#include <unordered_map>

//#include "versatPrivate.hpp"
#include "utils.hpp"
#include "parser.hpp"
//#include "configurations.hpp"

typedef std::unordered_map<String,Value> ValueMap;
typedef std::unordered_map<String,String> MacroMap;

struct Arena;
struct CompiledTemplate;

typedef Range<Expression*> ExpressionRange;

struct PortDeclaration{
  ValueMap attributes;
  //Range range; // Cannot be a range, otherwise we cannot deal with parameters
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
  //ValueMap parameters;
  std::vector<PortDeclaration> ports;
  String name;
  bool isSource;
};

// TODO: This is a copy of Wire from configurations.hpp, but we do not include because this file should not include from the compiler code.
template<typename T>
struct WireTemplate{
  String name;
  T bitSize;
  bool isStatic; // This is only used by the verilog parser (?) to store info. TODO: Use a different structure in the verilog parser which contains this and remove from Wire   
};

typedef WireTemplate<int> Wire;
typedef WireTemplate<ExpressionRange> WireExpression;

enum ExternalMemoryType{TWO_P = 0,DP}; // Two ports: one input and one output (think FIFO). Dual port: two input or output ports (think LUT)

// TODO: Because we changed memories to be byte space instead of symbol space, maybe it would be best to change how the address bit size is stored. These structures are supposed to be clean, and so the parser should identify any differences in address size and report and error. These structures should only have one address if we keep going with the byte space memories idea.
template<typename T>
struct ExternalMemoryTwoPortsTemplate{ // tp
  T bitSizeIn;
  T bitSizeOut;
  T dataSizeIn;
  T dataSizeOut;
};

typedef ExternalMemoryTwoPortsTemplate<int> ExternalMemoryTwoPorts;
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
  // tp
  // dp[2]
  ExternalMemoryType type;
  int interface;
};

typedef ExternalMemoryInterfaceTemplate<int> ExternalMemoryInterface;
typedef ExternalMemoryInterfaceTemplate<ExpressionRange> ExternalMemoryInterfaceExpression;

struct ExternalMemoryID{
  int interface;
  ExternalMemoryType type;
};

template<> class std::hash<ExternalMemoryID>{
public:
  std::size_t operator()(ExternalMemoryID const& s) const noexcept{
    std::size_t hash = s.interface * 2 + (int) s.type;
    return hash;
  }
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

// Contain info parsed directly by verilog.
// This probably should be a union of all the memory types
// The code in the verilog parser almost demands it

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

struct ModuleInfoInstance{
  String name;
  Array<ParameterExpression> parameters;
  Array<int> inputDelays;
  Array<int> outputLatencies;
  Array<Wire> configs;
  Array<Wire> states;
  Array<ExternalMemoryInterface> externalInterfaces;
  int nDelays;
  int nIO;
  int memoryMappedBits;
  int databusAddrSize;
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

String PreprocessVerilogFile(Arena* output, String fileContent,std::vector<String>* includeFilepaths,Arena* tempArena);
std::vector<Module> ParseVerilogFile(String fileContent, std::vector<String>* includeFilepaths, Arena* tempArena); // Only handles preprocessed files
ModuleInfo ExtractModuleInfo(Module& module,Arena* permanent,Arena* tempArena);
void OutputModuleInfos(FILE* output,ModuleInfoInstance info,String nameSpace,CompiledTemplate* unitVerilogData,Arena* temp,Array<Wire> configsHeaderSide,Array<String> statesHeaderSide); // TODO: This portion should be remade, parameters are not the ones we want (VerilogParsing do not need to know about Accelerator)

Array<String> GetAllIdentifiers(Expression* expr,Arena* arena);
Value Eval(Expression* expr,Array<ParameterExpression> parameters);

#endif // INCLUDED_VERILOG_PARSER

