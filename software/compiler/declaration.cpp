#include "declaration.hpp"

#include "hardwareInterfaces.hpp"

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
// Register special units

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
// Register (does not exist)

bool DECL_RegisterMeta(String name,V_Node* module){
  TEMP_REGION(temp,DECL_State.arena);

  DECL_Meta* exists = 0;
  LL_Find(DECL_State.metaHead,next,exists,it->name == name);
  Assert(!exists); // TODO: Proper solve this, either report error or allow overriding

  // TODO: We assuming that there is no duplicated param or anything like that.
  bool anyError = 0;
  auto l = PushList<DECL_Param>(temp);
  for(V_Node* ptr = module->childs; ptr; ptr = ptr->next){
    if(ptr->type == V_NodeType_PARAMETER){
      String name = ptr->token.val;

      V_SymConvResult converted = V_ConvertToSym(ptr->childs,{});

      anyError |= converted.error;
      DECL_Param* p = l->PushElem();
      p->name = PushString(DECL_State.arena,name);
      p->v = converted.res;
    }
  }

  DECL_Meta* node = PushStruct<DECL_Meta>(DECL_State.arena);
  node->name = PushString(DECL_State.arena,name);
  node->type = DECL_MetaType_SIMPLE;
  node->simpleUnit = module;
  node->params = PushArray(DECL_State.arena,l);
  node->anyError |= anyError;
  LL_Append(DECL_State.metaHead,DECL_State.metaTail,next,node);
  
  return node;
}

bool DECL_RegisterMeta(String name,SP_Node* module){
  DECL_Meta* exists = 0;
  LL_Find(DECL_State.metaHead,next,exists,it->name == name);
  Assert(!exists); // TODO: Proper solve this, either report error or allow overriding

  DECL_Meta* node = PushStruct<DECL_Meta>(DECL_State.arena);
  node->name = PushString(DECL_State.arena,name);
  node->type = DECL_MetaType_COMPOSITE;
  node->compositeUnit = module;
  LL_Append(DECL_State.metaHead,DECL_State.metaTail,next,node);

  return node;
}

FUDeclaration* DECL_RegisterFU(String name){
  FUDeclarationNode* res = PushStruct<FUDeclarationNode>(DECL_State.arena);
  
  res->val.name = PushString(DECL_State.arena,name);

  LL_Append(DECL_State.head,DECL_State.tail,next,res);
  return &res->val;
}

// ======================================
// Get or create if needed

FUDeclaration* DECL_GetType(String name,Array<DECL_Param> params){
  TEMP_REGION(temp,DECL_State.arena);

  FUDeclaration* res = &FUDeclaration_Nil;

  DECL_Meta* metaExists = 0;
  LL_Find(DECL_State.metaHead,next,metaExists,it->name == name);

  // Get parameters and normalize (insert default value if not exists) ==========
  // NOTE: Normalized parameter order should match default parameter order.
  // TODO: If parameters contain an expression we might need to evaluate it.
  //       Meaning that we cannot just do this, we might need to change some things in here.
  // TODO: Test an example where we have A(.X(2)) where A is module #(parameter X=1,parameter Y=X+1)
  //       and check that Y == 3.
  // NOTE: The only reason that Im not doing this right now is because need to see if we actually care
  //       about instantiating a declaration with expressions for parameters.
  //       Im assuming that we only care about instantiating declarations with constant values but
  //       we might also want to be able to instantiate declarations with expressions for parameters.
  //       At which point things become a bit more complicated and I rather have automatic tests in place
  //       before any change 
  Array<DECL_Param> defaultParams = {};
  Array<DECL_Param> normalizedParams = {};
  if(metaExists){
    defaultParams = metaExists->params;

    if(params.size == 0){
      normalizedParams = defaultParams;
    } else {
      auto l = PushList<DECL_Param>(temp);
      for(DECL_Param def : defaultParams){
        bool paramExists = 0;
        DECL_Param param = {};

        for(DECL_Param p : params){
          if(def.name == p.name){
            paramExists = 1;
            param = p;

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
        DECL_Param our = normalizedParams[i];
        DECL_Param them = ptr->val.parameters[i];

        Assert(our.name == them.name);

        if(!SYM_Equal(our.v,them.v)){
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
      FUDeclarationNode* node = DECL_InstantiateSimple(metaExists,normalizedParams);
      LL_Append(DECL_State.head,DECL_State.tail,next,node);
      
      res = &node->val;
    }
  }

  if(res->error){
    res = &FUDeclaration_Nil;
  }

  return res;
}

// ======================================
// Instantiation

FUDeclarationNode* DECL_InstantiateSimple(DECL_Meta* meta,Array<DECL_Param> normalizedParams){
  TEMP_REGION(temp,DECL_State.arena);

  V_Node* top = meta->simpleUnit;
  Assert(top->type = V_NodeType_MODULE);

  String moduleName = top->token.val;
  
  for(V_Node* ptr = top->attributes; ptr; ptr = ptr->next){
    // TODO: module attributes
  }

  // Convert param into sym pair ================================================
  Array<SYM_Pair> params = PushArray<SYM_Pair>(temp,normalizedParams.size);
  for(int i = 0; i < normalizedParams.size; i++){
    params[i].name = normalizedParams[i].name;
    params[i].val = normalizedParams[i].v;
  }

  // Check repeated wires =======================================================
  TrieSet<String>* repeated = PushTrieSet<String>(temp);
  for(V_Node* ptr = top->attributes; ptr; ptr = ptr->next){
    if(!V_IsPort(ptr->type)){
      continue;
    }
    String name = ptr->token.val;
    bool exists = repeated->ExistsOrInsert(name);
    if(exists){
      // TODO: Report error
    }
  }

  // Extract wire info ==========================================================
  struct DECL_InterfaceInfo{
    DECL_InterfaceInfo* next;
    HW_Interface* inter;
    int interfaceIndex;
    Array<bool> wiresSeen;
    Array<int> wiresSize;
  };

  enum DECL_SimpleType{
    DECL_SimpleType_NIL,
    DECL_SimpleType_INPUT,
    DECL_SimpleType_OUTPUT,
    DECL_SimpleType_DELAY
  };

  struct DECL_SimpleWireInfo{
    DECL_SimpleWireInfo* next;
    DECL_SimpleType type;
    int index;
    int latency;
  };

  struct DECL_ConfigOrState{
    DECL_ConfigOrState* next;
    String name;
    int size;
    VersatStage stage;
    bool isState;
  };

  bool error = 0;

  DECL_SimpleWireInfo* simpleHead = 0;
  DECL_SimpleWireInfo* simpleTail = 0;
  DECL_InterfaceInfo* complexHead = 0;
  DECL_InterfaceInfo* complexTail = 0;
  DECL_ConfigOrState* configStateHead = 0;
  DECL_ConfigOrState* configStateTail = 0;

  DECL_SingleInterface simple = {};
  int inputCount = 0;
  int outputCount = 0;
  int delayCount = 0;
  int configCount = 0;
  int stateCount = 0;
  int memoryMappedCount = 0;
  int externalMemoryCount = 0;
  SYM_Expr memoryMappedAddrSize = SYM_0;

  for(V_Node* ptr = top->childs; ptr; ptr = ptr->next){
    if(!V_IsPort(ptr->type)){
      continue;
    }

    String wireName = ptr->token.val;
    bool output = (ptr->type == V_NodeType_OUTPUT);
    bool input  = (ptr->type == V_NodeType_INPUT);
    bool inout  = (ptr->type == V_NodeType_INOUT);

    // Unpack port size range =====================================================
    V_Node* range = ptr->childs;
    if(!range){
      range = &V_Node_Range0;
    }
    V_SymConvResult topConv = V_ConvertToSym(range->first,params);
    V_SymConvResult bottomConv = V_ConvertToSym(range->second,params);
    
    SYM_EvaluateResult topEval = SYM_ConstantEvaluate(topConv.res);
    SYM_EvaluateResult bottomEval = SYM_ConstantEvaluate(bottomConv.res);

    error |= topConv.error | bottomConv.error | topEval.Error() | bottomEval.Error();

    int top = topEval.result;
    int bottom = bottomEval.result;
    int portSize = top - bottom + 1;

    // Unpack attributes ==========================================================
    int versatLatency = 0;
    VersatStage stage = {};
    for(V_Node* attr = ptr->attributes; attr; attr = attr->next){
      Assert(attr->type == V_NodeType_ATTRIBUTE);

      V_Node* id = attr->childs;
      V_Node* val = attr->childs->next;
      
      String name = id->token.val;
      
      bool found = 0;
      if(!found && name == "versat_latency"){
        found = 1;
        error |= (val->token.type != TokenType_NUMBER);
        versatLatency = ParseInt(val->token.val);
      }
      if(!found && name == "versat_stage"){
        error |= (val->token.type != TokenType_C_STRING);
        
        String content = Cut(val->token.val,1,1);

        if(content == "Read"){
          found = 1;
          stage = VersatStage_READ;
        } else if(content == "Write"){
          found = 1;
          stage = VersatStage_WRITE;
        } else if(content == "Compute"){
          found = 1;
        }

        if(!found){
          error = 1;
          // TODO: Better error reporting.
        }
      }

      if(!found){
        error = 1;
        // TODO: Report error
      }
    }

    // Process simple known and single bit wires ==================================
    bool found = 0;
    if(!found){
      DECL_SingleInterface s = DECL_SingleInterafacesFromName(wireName);
      found = (s != DECL_SingleInterface_NIL);
      simple |= s;
    }

    // Process single known name wires ============================================
    if(!found){
      int index = 0;

      DECL_SimpleType type = {};
      if(!found && SubString(wireName,2) == "in" && IsNumber(Cut(wireName,2,0))){
        found = 1;
        index = ParseInt(Cut(wireName,2,0));
        inputCount = MAX(inputCount,index + 1);
        type = DECL_SimpleType_INPUT;
      }
      if(!found && SubString(wireName,3) == "out" && IsNumber(Cut(wireName,3,0))){
        found = 1;
        index = ParseInt(Cut(wireName,3,0));
        outputCount = MAX(outputCount,index + 1);
        type = DECL_SimpleType_OUTPUT;
      }
      if(!found && SubString(wireName,5) == "delay" && IsNumber(Cut(wireName,5,0))){
        found = 1;
        index = ParseInt(Cut(wireName,5,0));
        delayCount = MAX(delayCount,index + 1);
        type = DECL_SimpleType_DELAY;
      }

      if(found){
        DECL_SimpleWireInfo* info = PushStruct<DECL_SimpleWireInfo>(temp);
        info->type = type;
        info->index = index;
        info->latency = versatLatency;

        LL_Append(simpleHead,simpleTail,next,info);
      }
    }

    // Process complex interfaces =================================================
    if(!found){
      for(HW_Interface* inter : HW_AllInterfaces){
        HW_ValueResult res = HW_ExtractValues(inter,wireName,temp);

        if(res.error){
          continue;
        }

        found = 1;
        int size = inter->wires.size;

        DECL_InterfaceInfo* info = 0;
        LL_Find(complexHead,next,info,it->inter == inter && it->interfaceIndex == res.interface);

        bool isExternalMemory = (inter == &HW_Default::HW_DP || inter == &HW_Default::HW_2P);
        bool isMemoryMapped = (inter == &HW_Default::HW_MM);

        if(!info){
          info = PushStruct<DECL_InterfaceInfo>(temp);
          info->wiresSize = PushArray<int>(temp,size * inter->maxPorts);
          info->wiresSeen = PushArray<bool>(temp,size * inter->maxPorts);
          info->interfaceIndex = res.interface;
          info->inter = inter;

          if(isExternalMemory){
            externalMemoryCount += 1;
          } else if(isMemoryMapped){
            memoryMappedCount += 1;
          } else {
            Assert(false && "Need to add logic to dependent on the type of interface being used");
          }

          LL_Append(complexHead,complexTail,next,info);
        }
      
        info->wiresSeen[res.port * size + res.wireIndex] = 1;
        info->wiresSize[res.port * size + res.wireIndex] = portSize;

        if(isMemoryMapped && (inter->wires[res.wireIndex].prop & HW_WireProperty_ADDR)){
          memoryMappedAddrSize = SYM_Lit(portSize);
        }
      }
    }
    
    // Process config or state wires ==============================================
    if(!found){
      bool isState = false;
      if(input){
        found = 1;
        configCount += 1;
      } else if(output){
        found = 1;
        isState = 1;
        stateCount += 1;
      } else {
        // TODO: Error, cannot handle inout
      }

      DECL_ConfigOrState* info = PushStruct<DECL_ConfigOrState>(temp);
      info->isState = isState;
      info->name = wireName;
      info->size = portSize;
      info->stage = stage;

      LL_Append(configStateHead,configStateTail,next,info);
    }

    if(!found){
      error = 1;

      if(inout){
        printf("Versat does not support inout ports");
      } else {
        printf("Error, something very strange happened processing port: %.*s\n",UN(moduleName));
      }
    }
  }

  // Final checks ===============================================================
  {
    Array<bool> seenInput = PushArray<bool>(temp,inputCount);
    Array<bool> seenOutput = PushArray<bool>(temp,outputCount);
    Array<bool> seenDelay = PushArray<bool>(temp,delayCount);
  
    for(DECL_SimpleWireInfo* ptr = simpleHead; ptr; ptr = ptr->next){
      if(ptr->type == DECL_SimpleType_INPUT){
        seenInput[ptr->index] = 1;
      }
      if(ptr->type == DECL_SimpleType_OUTPUT){
        seenOutput[ptr->index] = 1;
      }
      if(ptr->type == DECL_SimpleType_DELAY){
        seenDelay[ptr->index] = 1;
      }
    }
    for(int i = 0; i < seenInput.size; i++){
      if(!seenInput[i]){
        error = 1;
        printf("Cannot have gaps in delay indexes, in%d does not exist for unit: '%.*s'\n",i,UN(moduleName));
      }
    }
    for(int i = 0; i < seenOutput.size; i++){
      if(!seenOutput[i]){
        error = 1;
        printf("Cannot have gaps in delay indexes, out%d does not exist for unit: '%.*s'\n",i,UN(moduleName));
      }
    }
    for(int i = 0; i < seenDelay.size; i++){
      if(!seenDelay[i]){
        error = 1;
        printf("Cannot have gaps in delay indexes, delay%d does not exist for unit: '%.*s'\n",i,UN(moduleName));
      }
    }

    TrieMap<HW_Interface*,int>* interCount = PushTrieMap<HW_Interface*,int>(temp);
    for(DECL_InterfaceInfo* ptr = complexHead; ptr; ptr = ptr->next){
      int count = interCount->GetOrElse(ptr->inter,0);
      count = MAX(count,ptr->interfaceIndex + 1);
      interCount->Insert(ptr->inter,count);

      for(int i = 0; i < ptr->wiresSeen.size; i++){
        if(!ptr->wiresSeen[i]){
          error = 1;

          int index = i % ptr->inter->wires.size;
          int port  = i / ptr->inter->wires.size;

          String repr = HW_GetWireRepresentation(ptr->inter,index,port,ptr->interfaceIndex,temp);
          printf("Cannot have missing wires in interface, '%.*s' does not exist for unit: '%.*s'\n",UN(repr),UN(moduleName));
        }
      }
    }
  }

  // Pack =======================================================================
  Arena* out = DECL_State.arena;

  FUDeclarationNode* node = PushStruct<FUDeclarationNode>(out);

  FUDeclaration* res = &node->val;
  res->metaName = meta->name;
  res->parameters = PushArray<DECL_Param>(out,normalizedParams.size);
  res->name = PushString(DECL_State.arena,moduleName); // TODO: Serialize if needed.
  res->inputs = PushArray<DECL_PortInfo>(out,inputCount);
  res->outputs = PushArray<DECL_PortInfo>(out,outputCount);
  res->configs = PushArray<Wire>(out,configCount);
  res->states = PushArray<Wire>(out,stateCount);
  res->numberDelays = delayCount;
  res->memoryMapped = PushArray<SYM_Expr>(out,memoryMappedCount);
  res->externalMemory = PushArray<HW_Instance>(out,externalMemoryCount);
  res->singleInterfaces = simple;
  res->error = error;

  for(int i = 0; i < normalizedParams.size; i++){
    res->parameters[i].name = PushString(out,normalizedParams[i].name);
    res->parameters[i].v = normalizedParams[i].v;
  }

  for(DECL_SimpleWireInfo* ptr = simpleHead; ptr; ptr = ptr->next){
    int index = ptr->index;
    switch(ptr->type){
     case DECL_SimpleType_INPUT:{
       res->inputs[index].delay = ptr->latency;
     } break;
     case DECL_SimpleType_OUTPUT:{
       res->outputs[index].delay = ptr->latency;
     } break;
     case DECL_SimpleType_DELAY:{
       // Nothing
     } break;
    }
  }

  int configIndex = 0;
  int stateIndex = 0;
  for(DECL_ConfigOrState* ptr = configStateHead; ptr; ptr = ptr->next){
    if(ptr->isState){
      res->states[stateIndex].name = PushString(out,ptr->name);
      res->states[stateIndex].sizeExpr = SYM_Lit(ptr->size);
      res->states[stateIndex].stage = ptr->stage;
      stateIndex += 1;
    } else {
      res->configs[configIndex].name = PushString(out,ptr->name);
      res->configs[configIndex].sizeExpr = SYM_Lit(ptr->size);
      res->configs[configIndex].stage = ptr->stage;
      configIndex += 1;
    }
  }
  
  if(res->memoryMapped.size){
    res->memoryMapped[0] = memoryMappedAddrSize;
  }
  
  int externalIndex = 0;
  for(DECL_InterfaceInfo* ptr = complexHead; ptr; ptr = ptr->next){
    int totalWireSize = ptr->inter->wires.size * ptr->inter->maxPorts;
    
    res->externalMemory[externalIndex].inter = ptr->inter;
    res->externalMemory[externalIndex].index = externalIndex;
    res->externalMemory[externalIndex].wires = PushArray<HW_Wire>(out,totalWireSize);
    for(int i = 0; i < totalWireSize; i++){
      res->externalMemory[externalIndex].wires[i].size = SYM_Lit(ptr->wiresSize[i]);
    }

    externalIndex += 1;
  }

  return node;
}
