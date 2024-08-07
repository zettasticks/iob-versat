#include "declaration.hpp"

#include "versat.hpp"

Pool<FUDeclaration> globalDeclarations;

namespace BasicDeclaration{
  FUDeclaration* buffer;
  FUDeclaration* fixedBuffer;
  FUDeclaration* input;
  FUDeclaration* output;
  FUDeclaration* multiplexer;
  FUDeclaration* combMultiplexer;
  FUDeclaration* stridedMerge;
  FUDeclaration* timedMultiplexer;
  FUDeclaration* pipelineRegister;
}

static int zeros[99] = {};
static Array<int> zerosArray = {zeros,99};

static int ones[64] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};

static FUDeclaration* RegisterCircuitInput(){
  Arena* permanent = permanent;
  FUDeclaration decl = {};

  decl.name = STRING("CircuitInput");
  decl.baseConfig.inputDelays = Array<int>{zeros,0};
  decl.baseConfig.outputLatencies = Array<int>{zeros,1};
  //decl.delayType = DelayType::DelayType_SOURCE_DELAY;
  decl.type = FUDeclarationType_SPECIAL;
  
  return RegisterFU(decl);
}

static FUDeclaration* RegisterCircuitOutput(){
  Arena* permanent = permanent;
  FUDeclaration decl = {};

  decl.name = STRING("CircuitOutput");
  decl.baseConfig.inputDelays = Array<int>{zeros,50};
  decl.baseConfig.outputLatencies = Array<int>{zeros,0};
  //decl.delayType = DelayType::DelayType_SINK_DELAY;
  decl.type = FUDeclarationType_SPECIAL;

  return RegisterFU(decl);
}

static FUDeclaration* RegisterLiteral(){
  Arena* permanent = permanent;
  FUDeclaration decl = {};

  decl.name = STRING("Literal");
  decl.baseConfig.outputLatencies = Array<int>{zeros,1};

  return RegisterFU(decl);
}

static void RegisterOperators(){
  struct Operation{
    const char* name;
    const char* operation;
  };

  Operation unary[] =  {{"NOT","{0}_{1} = ~{2}"},
                        {"NEG","{0}_{1} = -{2}"}};
  Operation binary[] = {{"XOR","{0}_{1} = {2} ^ {3}"},
                         {"ADD","{0}_{1} = {2} + {3}"},
                         {"SUB","{0}_{1} = {2} - {3}"},
                         {"AND","{0}_{1} = {2} & {3}"},
                         {"OR"  ,"{0}_{1} = {2} | {3}"},
                         {"RHR","{0}_{1} = ({2} >> {3}) | ({2} << (32 - {3}))"}, // TODO: Only for 32 bits
                         {"SHR","{0}_{1} = {2} >> {3}"},
                         {"RHL","{0}_{1} = ({2} << {3}) | ({2} >> (32 - {3}))"}, // TODO: Only for 32 bits
                         {"SHL","{0}_{1} = {2} << {3}"}};

  FUDeclaration decl = {};
  decl.baseConfig.inputDelays = Array<int>{zeros,1};
  decl.baseConfig.outputLatencies = Array<int>{zeros,1};
  decl.isOperation = true;

  for(unsigned int i = 0; i < ARRAY_SIZE(unary); i++){
    decl.name = STRING(unary[i].name);
    decl.operation = unary[i].operation;
    RegisterFU(decl);
  }

  decl.baseConfig.inputDelays = Array<int>{zeros,2};
  for(unsigned int i = 0; i < ARRAY_SIZE(binary); i++){
    decl.name = STRING(binary[i].name);
    decl.operation = binary[i].operation;
    RegisterFU(decl);
  }
}

FUDeclaration* GetTypeByName(String name){
  for(FUDeclaration* decl : globalDeclarations){
    if(CompareString(decl->name,name)){
      return decl;
    }
  }
  
  return nullptr;
}

FUDeclaration* GetTypeByNameOrFail(String name){
  FUDeclaration* decl = GetTypeByName(name);

  if(!decl){
    LogFatal(LogModule::TOP_SYS,"Didn't find the following type: %.*s",UNPACK_SS(name));
  }
  return decl;
}

FUDeclaration* RegisterFU(FUDeclaration decl){
  FUDeclaration* type = globalDeclarations.Alloc();
  *type = decl;

  return type;
}

void InitializeSimpleDeclarations(){
  RegisterOperators();
  RegisterCircuitInput();
  RegisterCircuitOutput();
  RegisterLiteral();
}

bool HasMultipleConfigs(FUDeclaration* decl){
  bool res = (decl->configInfo.size >= 2);
  return res;
}
