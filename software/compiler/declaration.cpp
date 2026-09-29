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

  DECL_Meta* metaHead = 0;
  DECL_Meta* metaTail = 0;
};

static DECL_StateTag DECL_State = {};

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
  static Arena arenaInst = InitArena(Megabyte(1));
  DECL_State.arena = &arenaInst;
  
  RegisterOperators();
  RegisterCircuitInput();
  RegisterCircuitOutput();
  RegisterLiteral();
}

// ======================================
// Parameter handling

Array<ParamNameAndValue> DECL_GetDefaultParams(DECL_Meta* meta,Arena* out){
  TEMP_REGION(temp,out);
  
  Array<ParamNameAndValue> res = {};
  if(meta->type == DECL_MetaType_SIMPLE){
    V_Node* top = meta->simpleUnit;

    // TODO: We assuming that there is no duplicated param or anything like that.
    // TODO: A proper implementation of this needs to be able to evaluate expressions.
    //       Otherwise how do we calculate the default value of a parameter that contains any form of 
    auto l = PushList<ParamNameAndValue>(temp);
    for(V_Node* ptr = top->childs; ptr; ptr = ptr->next){
      if(ptr->type == V_NodeType_PARAMETER){
        String name = ptr->token.val;
        V_Node* val = ptr->childs;
        
        if(val->type == V_NodeType_EXPR){
          val = val->childs;
        }

        Assert(val->type == V_NodeType_LITERAL);
        
        Token token = val->token;
        
        // TODO: Do not care about strings and stuff like that, but we might want to represent in
        //       the code base and carry the information around. Do not know right now, handle later.
        if(token.type != TokenType_NUMBER){
          continue;
        }
        
        // TODO: We might want to implement proper number support, instead of assuming 32 bit integer.
        V_ParsedNumber num = V_ParseNumber(token.val.data,token.val.data + token.val.size);
        int val = num.decimalNumber;

        ParamNameAndValue* param = l->PushElem();
        param->name = PushString(out,name);
        param->value = val;
     }
    }

    res = PushArray(out,l);
  } else {

  }

  return res;
}

// ======================================
// Register (does not exist)

bool DECL_RegisterMeta(String name,V_Node* module){
  DECL_Meta* exists = 0;
  LL_Find(DECL_State.metaHead,next,exists,it->name == name);

  // TODO: Proper solve this
  Assert(!exists);

  DECL_Meta* node = PushStruct<DECL_Meta>(DECL_State.arena);
  node->name = PushString(DECL_State.arena,name);
  node->type = DECL_MetaType_SIMPLE;
  node->simpleUnit = module;
  LL_Append(DECL_State.metaHead,DECL_State.metaTail,next,node);

  return exists;
}

bool DECL_RegisterMeta(String name,SP_Node* module){
  DECL_Meta* exists = 0;
  LL_Find(DECL_State.metaHead,next,exists,it->name == name);

  // TODO: Proper solve this
  Assert(!exists);

  DECL_Meta* node = PushStruct<DECL_Meta>(DECL_State.arena);
  node->name = PushString(DECL_State.arena,name);
  node->type = DECL_MetaType_COMPOSITE;
  node->compositeUnit = module;
  LL_Append(DECL_State.metaHead,DECL_State.metaTail,next,node);

  return exists;
}

FUDeclaration* DECL_RegisterFU(String name){
  FUDeclarationNode* newDecl = PushStruct<FUDeclarationNode>(DECL_State.arena);
  
  newDecl->val.name = PushString(DECL_State.arena,name);

  LL_Append(DECL_State.head,DECL_State.tail,next,newDecl);
  return &newDecl->val;
}

// ======================================
// Get or create if needed

FUDeclaration* DECL_GetType(String name,Array<ParamNameAndValue> params){
  TEMP_REGION(temp,DECL_State.arena);

  FUDeclaration* res = &FUDeclaration_Nil;

  DECL_Meta* metaExists = 0;
  LL_Find(DECL_State.metaHead,next,metaExists,it->name == name);

  // Get parameters and normalize (insert default value if not exists) ==========
  // NOTE: Normalized parameter order should match default parameter order.
  Array<ParamNameAndValue> defaultParams = {};
  Array<ParamNameAndValue> normalizedParams = {};
  if(metaExists){
    defaultParams = DECL_GetDefaultParams(metaExists,temp);

    if(params.size == 0){
      normalizedParams = defaultParams;
    } else {
      auto l = PushList<ParamNameAndValue>(temp);
      for(ParamNameAndValue def : defaultParams){
        bool paramExists = 0;

        for(ParamNameAndValue param : params){
          if(def.name == param.name){
            paramExists = 1;
          
            break;
          }
        }

        if(paramExists){
          *l->PushElem() = param;
        } else {
          *l->PushElem() = def;
        }
      }
      normalizedParams = PushArray(temp,l);
    }

    Assert(normalizedParams.size == defaultParams.size);
  }

  // Check if a declaration with same params already exists =====================
  int paramSize = defaultParams.size;
  for(FUDeclarationNode* ptr = DECL_State.head; ptr; ptr = ptr->next){
    if(ptr->val.name == name){

      Assert(ptr->val.parameters.size == defaultParams.size);

      bool allEqual = 1;
      for(int i = 0; i < paramSize; i++){
        ParamNameAndValue our = normalizedParams[i];
        ParamNameAndValue them = ptr->val.parameters[i];

        Assert(our.name == them.name);

        if(our.value != them.value){
          allEqual = 0;
        }
      }

      if(allEqual){
        res = &ptr->val;
        break;
      }
    }
  }

  if(res == &FUDeclaration_Nil && metaExists){
    if(metaExists->type == DECL_MetaType_SIMPLE){
      res = DECL_InstantiateSimple(metaExists,normalizedParams);
    }
  }

  return res;
}

// ======================================
// Instantiation

FUDeclaration* DECL_InstantiateSimple(DECL_Meta* meta,Array<ParamNameAndValue> normalizedParams){
  -- LEFT HERE - Need to pick on the logic of ExtractModuleInfo and RegisterModuleInfo + proper param handling.
}
