#include "declaration.hpp"


#if 0

#include "configurations.hpp"
#include "globals.hpp"
#include "versat.hpp"

Pool<FUDeclaration> globalDeclarations;

FUDeclaration* RegisterFU(FUDeclaration decl){
  Assert(decl.type != DeclarationType_NIL);
  
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

FUDeclaration* GetTypeByName(String str,Array<ParamNameAndValue> params){
  TEMP_REGION(temp,nullptr);

  String mangledName = DECL_MangleName(str,params,temp);
  FUDeclaration* res = GetTypeByName(mangledName);
  return res;
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
#if 1
  return typeName;
#else
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
    b->PushString("%d",val.value);
  }

  String res = EndString(out,b);
  return res;
#endif
}

#endif

// ======================================
// Globals

readonly FUDeclaration FUDeclaration_Nil = {};

struct DECL_StateTag{
  Arena* arena;
  
  FUDeclarationNode* head = 0;
  FUDeclarationNode* tail = 0;
};

static DECL_StateTag DECL_State;

// ======================================
// 

static FUDeclaration* RegisterCircuitInput(){
  FUDeclaration* decl = DECL_RegisterFU("CircuitInput");
  decl->type = DeclarationType_SPECIAL;
  return decl;
}

static FUDeclaration* RegisterCircuitOutput(){
  FUDeclaration* decl = DECL_RegisterFU("CircuitOutput");
  decl->type = DeclarationType_SPECIAL;
  return decl;
}

static FUDeclaration* RegisterLiteral(){
  FUDeclaration* decl = DECL_RegisterFU("Literal");
  decl->type = DeclarationType_SINGLE;
  decl->outputs = PushArray<DECL_PortInfo>(DECL_State.arena,1);
  decl->outputs[0].delay = 0;
  
  return decl;
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

  for(unsigned int i = 0; i < ARRAY_SIZE(unary); i++){
    FUDeclaration* decl = DECL_RegisterFU(unary[i].name);

    decl->type = DeclarationType_SINGLE;
    decl->inputs = PushArray<DECL_PortInfo>(DECL_State.arena,1);
    decl->outputs = PushArray<DECL_PortInfo>(DECL_State.arena,1);
    decl->operation = unary[i].operation;
  }

  for(unsigned int i = 0; i < ARRAY_SIZE(binary); i++){
    FUDeclaration* decl = DECL_RegisterFU(binary[i].name);

    decl->inputs = PushArray<DECL_PortInfo>(DECL_State.arena,2);
    decl->outputs = PushArray<DECL_PortInfo>(DECL_State.arena,1);
    decl->operation = binary[i].operation;
  }
}


namespace BasicDeclaration{
  FUDeclaration* nil = &FUDeclaration_Nil;

  FUDeclaration* input;
  FUDeclaration* output;

#if 0
  FUDeclaration* variableBuffer;
  FUDeclaration* fixedBuffer;
  FUDeclaration* multiplexer;
  FUDeclaration* combMultiplexer;
  FUDeclaration* stridedMerge;
  FUDeclaration* timedMultiplexer;
  FUDeclaration* pipelineRegister;
#endif
}

// ======================================
// Type

bool IsNil(FUDeclaration* decl){
  bool res = (decl && decl->type == DeclarationType_NIL);
  return res;
}

// ======================================
// Init

void DECL_Init(){
  Arena arenaInst = InitArena(Megabyte(1));
  DECL_State.arena = &arenaInst;
  
  RegisterOperators();
  RegisterCircuitInput();
  RegisterCircuitOutput();
  RegisterLiteral();
}

// ======================================
// Register (does not exist)

FUDeclaration* DECL_RegisterFU(String name){
  FUDeclarationNode* newDecl = PushStruct<FUDeclarationNode>(DECL_State.arena);
  
  newDecl->val.name = PushString(DECL_State.arena,name);

  LL_Append(DECL_State.head,DECL_State.tail,next,newDecl);
  return &newDecl->val;
}

// ======================================
// Get or create if needed

FUDeclaration* DECL_GetType(String name,Array<ParamNameAndValue> params){
  FUDeclaration* res = &FUDeclaration_Nil;

  for(FUDeclarationNode* ptr = DECL_State.head; ptr; ptr = ptr->next){
    if(ptr->val.name == name){
      res = &ptr->val;
      break;
    }
  }

  return res;
}
