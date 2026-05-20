#include "declaration.hpp"

#include "configurations.hpp"
#include "globals.hpp"
#include "versat.hpp"

FUDeclaration FUDeclaration_Nil = {};

Pool<FUDeclaration> globalDeclarations;

namespace BasicDeclaration{
  FUDeclaration* nil = &FUDeclaration_Nil;

  FUDeclaration* variableBuffer;
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

static FUDeclaration* RegisterCircuitInput(){
  FUDeclaration decl = {};
  decl.type = FUDeclarationType_SINGLE;
  decl.name = "CircuitInput";

  decl.info.infos = PushArray<MergePartition>(globalPermanent,1);
  decl.info.infos[0].inputDelays = Array<int>{zeros,0};
  decl.info.infos[0].outputLatencies = Array<int>{zeros,1};
  
  return RegisterFU(decl);
}

static FUDeclaration* RegisterCircuitOutput(){
  FUDeclaration decl = {};
  decl.type = FUDeclarationType_SINGLE;
  decl.name = "CircuitOutput";

  decl.info.infos = PushArray<MergePartition>(globalPermanent,1);
  decl.info.infos[0].inputDelays = Array<int>{zeros,50};
  decl.info.infos[0].outputLatencies = Array<int>{zeros,0};

  return RegisterFU(decl);
}

static FUDeclaration* RegisterLiteral(){
  FUDeclaration decl = {};
  decl.type = FUDeclarationType_SINGLE;
  decl.name = "Literal";

  decl.info.infos = PushArray<MergePartition>(globalPermanent,1);
  decl.info.infos[0].outputLatencies = Array<int>{zeros,1};
  
  return RegisterFU(decl);
}

static void RegisterOperators(){
  struct Operation{
    String name;
    String operation;
  };

  Operation unary[] =  {{"NOT" ,"~{0}"},
                        {"NEG" ,"-{0}"}};
  Operation binary[] = {{"XOR" ,"{0} ^ {1}"},
                         {"ADD","{0} + {1}"},
                         {"SUB","{0} - {1}"},
                         {"AND","{0} & {1}"},
                         {"OR" ,"{0} | {1}"},
                         {"RHR","({0} >> {1}) | ({0} << (DATA_W - {1}))"},
                         {"SHR","{0} >> {1}"},
                         {"RHL","({0} << {1}) | ({0} >> (DATA_W - {1}))"},
                         {"SHL","{0} << {1}"}};

  FUDeclaration decl = {};
  decl.type = FUDeclarationType_SINGLE;
  decl.isOperation = true;

  for(unsigned int i = 0; i < ARRAY_SIZE(unary); i++){
    decl.info.infos = PushArray<MergePartition>(globalPermanent,1);
    decl.info.infos[0].inputDelays = Array<int>{zeros,1};
    decl.info.infos[0].outputLatencies = Array<int>{zeros,1};

    decl.name = unary[i].name;
    decl.operation = unary[i].operation;
    RegisterFU(decl);
  }

  for(unsigned int i = 0; i < ARRAY_SIZE(binary); i++){
    decl.info.infos = PushArray<MergePartition>(globalPermanent,1);
    decl.info.infos[0].inputDelays = Array<int>{zeros,2};
    decl.info.infos[0].outputLatencies = Array<int>{zeros,1};

    decl.name = binary[i].name;
    decl.operation = binary[i].operation;
    RegisterFU(decl);
  }
}

bool IsNil(FUDeclaration* decl){
  bool res = (decl->type == FUDeclarationType_NIL);
  return res;
}

FUDeclaration* RegisterFU(FUDeclaration decl){
  Assert(decl.type != FUDeclarationType_NIL);
  
  FUDeclaration* type = globalDeclarations.Alloc();
  *type = decl;

  return type;
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
  Assert(decl);
  return decl;
}

void InitializeSimpleDeclarations(){
  RegisterOperators();
  RegisterCircuitInput();
  RegisterCircuitOutput();
  RegisterLiteral();
}

bool HasMultipleConfigs(FUDeclaration* decl){
  bool res = (decl->MergePartitionSize() >= 2);
  return res;
}

// ======================================
// Declaration inspection

Wire* GetConfigWireByName(FUDeclaration* decl,String name){
  for(Wire& w : decl->configs){
    if(w.name == name){
      return &w;
    }
  }

  return nullptr;
}

String DECL_MangleName(String typeName,Array<ParamNameAndValue> params,Arena* out){
  TEMP_REGION(temp,out);

  Array<ParamNameAndValue> ordered = CopyArray(params,temp);
  
  for(int i = 0; i < ordered.size; i++){
    for(int j = i + 1; j < ordered.size; j++){
      if(CompareStringOrdered(ordered[i].name,ordered[j].name) > 0){
        SWAP(ordered[i],ordered[j]);
      }
    }
  }

  auto b = StartString(temp);
  b->PushString(typeName);

  for(ParamNameAndValue val : ordered){
    b->PushString("_");
    b->PushString(val.name);
    b->PushString("_");
    SYM_Repr(b,val.value);
  }

  String res = EndString(out,b);
  return res;
}
