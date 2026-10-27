#include "declaration.hpp"

#include "hardwareInterfaces.hpp"
#include "debugVersat.hpp"
#include "merge.hpp"

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
  static DECL_PortInfo info = {};
  info.size = SYM_DataW;

  FUDeclaration* decl = DECL_RegisterFU("CircuitInput",DeclarationType_SPECIAL);
  decl->outputs.data = &info;
  decl->outputs.size = 1;
  return decl;
}

static FUDeclaration* RegisterCircuitOutput(){
  FUDeclaration* decl = DECL_RegisterFU("CircuitOutput",DeclarationType_SPECIAL);
  return decl;
}

static FUDeclaration* RegisterLiteral(){
  FUDeclaration* decl = DECL_RegisterFU("Literal",DeclarationType_SINGLE);
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
    FUDeclaration* decl = DECL_RegisterFU(unary[i].name,DeclarationType_SINGLE);

    decl->type = DeclarationType_SINGLE;
    decl->inputs = PushArray<DECL_PortInfo>(DECL_State.arena,1);
    decl->outputs = PushArray<DECL_PortInfo>(DECL_State.arena,1);
    decl->operation = unary[i].operation;
  }

  for(unsigned int i = 0; i < ARRAY_SIZE(binary); i++){
    FUDeclaration* decl = DECL_RegisterFU(binary[i].name,DeclarationType_SINGLE);

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
  FUDeclaration* pipelineRegister;
#endif
}

// ======================================
// Type

bool IsNil(FUDeclaration* decl){
  bool res = (decl && decl->type == DeclarationType_NIL);
  return res;
}
u64 Hash(DECL_StaticWire in){
  u64 res = Hash(in.meta) + Hash(in.unitName) + Hash(in.wire);
  return res;
}
bool Equal(DECL_StaticWire lhs,DECL_StaticWire rhs){
  bool res = (lhs.meta == rhs.meta && lhs.unitName == rhs.unitName && lhs.wire == rhs.wire);
  return res;
}

bool DECL_IsInput(FUDeclaration* decl){
  bool res = (decl == BasicDeclaration::input);
  return res;
}

bool DECL_IsOutput(FUDeclaration* decl){
  bool res = (decl == BasicDeclaration::output);
  return res;
}

bool DECL_IsSink(FUDeclaration* decl){
  bool res = (decl->numberInputDelays > 0);
  return res;
}

bool DECL_IsCompute(FUDeclaration* decl){
  bool res = (decl->inputs.size > 0 && decl->outputs.size > 0 && decl->numberOutputDelays == 0);
  return res;
}

bool DECL_IsComputeWithDelay(FUDeclaration* decl){
  bool res = (DECL_IsCompute(decl) && decl->numberInputDelays > 0);
  return res;
}

bool DECL_IsSource(FUDeclaration* decl){
  bool res = (decl->numberOutputDelays > 0);
  return res;
}

bool DECL_IsVersatDefault(FUDeclaration* decl){
  bool res = (decl->type == DeclarationType_SINGLE && decl->operation.size == 0);
  return res;
}

bool DECL_IsVersatGenerated(FUDeclaration* decl){
  bool res = (decl->type == DeclarationType_COMPOSITE ||
              decl->type == DeclarationType_MERGED);
  return res;
}

bool DECL_IsVersatOperation(FUDeclaration* decl){
  bool res = (decl->operation.size > 0);
  return res;
}

bool DECL_HasVariableInputs(FUDeclaration* decl){
  bool res = (decl == BasicDeclaration::output);
  return res;
}

bool DECL_IsComposite(FUDeclaration* decl){
  bool res = (decl->type == DeclarationType_COMPOSITE ||
              decl->type == DeclarationType_MERGED);
  return res;
}

// ======================================
// Init

void DECL_Init(){
  static Arena arenaInst = InitArena(Megabyte(1));
  DECL_State.arena = &arenaInst;
  
  RegisterOperators();
  BasicDeclaration::input = RegisterCircuitInput();
  BasicDeclaration::output = RegisterCircuitOutput();
  RegisterLiteral();
}

// ======================================
// Helpers

Array<DECL_Param> DECL_Copy(Array<DECL_Param> in,Arena* out){
  Array<DECL_Param> res = PushArray<DECL_Param>(out,in.size);
  for(int i = 0; i < in.size; i++){
    res[i].name = PushString(out,in[i].name);
    res[i].val = in[i].val;
  }
  return res;
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
      p->val = converted.res;
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

  // TODO: We need to standardize this. We need to have all the params that are set by default in all the interfaces.
  //       If a standard external memory interface uses DATA_W, then we need to have DATA_W as a param otherwise
  //       the instantiation code does not know that we have DATA_W dependencies.
  Array<DECL_Param> params = PushArray<DECL_Param>(DECL_State.arena,6);
  params[0] = {"ADDR_W",SYM_32};
  params[1] = {"DATA_W",SYM_32};
  params[2] = {"LEN_W",SYM_32};
  params[3] = {"DELAY_W",SYM_32};
  params[4] = {"AXI_DATA_W",SYM_32};
  params[5] = {"AXI_ADDR_W",SYM_32};

  DECL_Meta* node = PushStruct<DECL_Meta>(DECL_State.arena);
  node->name = PushString(DECL_State.arena,name);

  node->type = DECL_MetaType_MODULE;
  if(module->type == SP_Type_MERGE_DECL){
    node->type = DECL_MetaType_MERGE;
  }

  node->compositeUnit = module;
  node->params = params;
  LL_Append(DECL_State.metaHead,DECL_State.metaTail,next,node);

  return node;
}

FUDeclaration* DECL_RegisterFU(String name,DeclarationType type){
  FUDeclarationNode* res = PushStruct<FUDeclarationNode>(DECL_State.arena);
  res->val = PushStruct<FUDeclaration>(DECL_State.arena);

  res->val->name = PushString(DECL_State.arena,name);
  res->val->type = type;

  LL_Append(DECL_State.head,DECL_State.tail,next,res);
  return res->val;
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
  } else {
    normalizedParams = params;
  }

  // Check if a declaration with same params already exists =====================
  int paramSize = defaultParams.size;
  for(FUDeclarationNode* ptr = DECL_State.head; ptr; ptr = ptr->next){
    if(ptr->val->name == name){
      Assert(ptr->val->parameters.size == defaultParams.size);

      bool allEqual = 1;
      for(int i = 0; i < paramSize; i++){
        DECL_Param our = normalizedParams[i];
        DECL_Param them = ptr->val->parameters[i];

        Assert(our.name == them.name);

        if(!SYM_Equal(our.val,them.val)){
          allEqual = 0;
        }
      }

      if(allEqual){
        res = ptr->val;
        break;
      }
    }
  }

  if(res == &FUDeclaration_Nil && metaExists){
    if(metaExists->type == DECL_MetaType_SIMPLE){
      res = DECL_InstantiateSimple(metaExists,normalizedParams);
    }
    if(metaExists->type == DECL_MetaType_MODULE){
      res = DECL_InstantiateModule(metaExists,normalizedParams);
    }
    if(metaExists->type == DECL_MetaType_MERGE){
      res = DECL_InstantiateMerge(metaExists,normalizedParams);
    }
  }

  if(res->error){
    res = &FUDeclaration_Nil;
  }

  return res;
}

FUDeclaration* DECL_GetBufferType(int delay){
  DECL_Param buffer = {};
  buffer.name = "AMOUNT";
  buffer.val = SYM_Lit(delay);
  Array<DECL_Param> params = {};
  params.data = &buffer;
  params.size = 1;
 
  FUDeclaration* res = DECL_GetType("FixedBuffer",params);
  return res;
}

FUDeclaration* DECL_GetVariableBufferType(){
  static FUDeclaration* res = DECL_GetType("Buffer",{});
  return res;
}

// ======================================
// Instantiation

FUDeclaration* DECL_InstantiateSimple(DECL_Meta* meta,Array<DECL_Param> normalizedParams){
  TEMP_REGION(temp,DECL_State.arena);

  V_Node* top = meta->simpleUnit;
  Assert(top->type = V_NodeType_MODULE);

  String moduleName = top->token.val;
  
  // Convert param into sym pair ================================================
  Array<SYM_Pair> params = PushArray<SYM_Pair>(temp,normalizedParams.size);
  for(int i = 0; i < normalizedParams.size; i++){
    params[i].name = normalizedParams[i].name;
    params[i].val = normalizedParams[i].val;
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
    Array<SYM_Expr> wiresSize;
  };

  enum DECL_SimpleType{
    DECL_SimpleType_NIL,
    DECL_SimpleType_INPUT,
    DECL_SimpleType_OUTPUT,
    DECL_SimpleType_INPUT_DELAY,
    DECL_SimpleType_OUTPUT_DELAY
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
    SYM_Expr size;
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
  int inputDelayCount = 0;
  int outputDelayCount = 0;
  int delayCount = 0;
  int configCount = 0;
  int stateCount = 0;
  int memoryMappedCount = 0;
  int externalMemoryCount = 0;
  int databusCount = 0;
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

    error |= topConv.error | bottomConv.error;

    // TODO: We must make sure that all the identifiers used by symbolic expressions are contained in the params.
    //       Otherwise the generated verilog code would depend on something that does not exist.
    SYM_Expr portSize = topConv.res - bottomConv.res + SYM_1;
    
    // Unpack attributes ==========================================================
    int versatLatency = 0;
    VersatStage stage = {};
    for(V_Node* attr = ptr->attributes; attr; attr = attr->next){
      Assert(attr->type == V_NodeType_ATTRIBUTE);

      String name = attr->token.val;
      V_Node* val = attr->childs;
    
      bool found = 0;
      if(!found && name == "versat_latency"){
        found = 1;

        V_SymConvResult asSym = V_ConvertToSym(val,params);
        SYM_EvaluateResult value = SYM_ConstantEvaluate(asSym.res);
        
        error |= (asSym.error | value.Error());
        versatLatency = value.result;
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
      if(!found && SubString(wireName,8) == "delay_in" && IsNumber(Cut(wireName,8,0))){
        found = 1;
        index = ParseInt(Cut(wireName,8,0));
        inputDelayCount = MAX(inputDelayCount,index + 1);
        type = DECL_SimpleType_INPUT_DELAY;
      }
      if(!found && SubString(wireName,9) == "delay_out" && IsNumber(Cut(wireName,9,0))){
        found = 1;
        index = ParseInt(Cut(wireName,9,0));
        outputDelayCount = MAX(outputDelayCount,index + 1);
        type = DECL_SimpleType_OUTPUT_DELAY;
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

        bool isExternalMemory = (HW_IsMemoryInterface(inter));
        bool isMemoryMapped = (HW_IsMemoryMapped(inter));
        bool isDatabus = (HW_IsDatabus(inter));

        if(!info){
          info = PushStruct<DECL_InterfaceInfo>(temp);
          info->wiresSize = PushArray<SYM_Expr>(temp,size * inter->maxPorts);
          info->wiresSeen = PushArray<bool>(temp,size * inter->maxPorts);
          info->interfaceIndex = res.interface;
          info->inter = inter;

          if(isExternalMemory){
            externalMemoryCount += 1;
          } else if(isMemoryMapped){
            memoryMappedCount += 1;
          } else if(isDatabus){
            databusCount += 1;
          } else {
            Assert(false && "Need to add logic to dependent on the type of interface being used");
          }

          LL_Append(complexHead,complexTail,next,info);
        }
      
        info->wiresSeen[res.port * size + res.wireIndex] = 1;
        info->wiresSize[res.port * size + res.wireIndex] = portSize;

        if(isMemoryMapped && (inter->wires[res.wireIndex].prop & HW_WireProperty_ADDR)){
          memoryMappedAddrSize = portSize;
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
    Array<bool> seenInputDelay = PushArray<bool>(temp,inputDelayCount);
    Array<bool> seenOutputDelay = PushArray<bool>(temp,outputDelayCount);
  
    for(DECL_SimpleWireInfo* ptr = simpleHead; ptr; ptr = ptr->next){
      if(ptr->type == DECL_SimpleType_INPUT){
        seenInput[ptr->index] = 1;
      }
      if(ptr->type == DECL_SimpleType_OUTPUT){
        seenOutput[ptr->index] = 1;
      }
      if(ptr->type == DECL_SimpleType_INPUT_DELAY){
        seenInputDelay[ptr->index] = 1;
      }
      if(ptr->type == DECL_SimpleType_OUTPUT_DELAY){
        seenOutputDelay[ptr->index] = 1;
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
    for(int i = 0; i < seenInputDelay.size; i++){
      if(!seenInputDelay[i]){
        error = 1;
        printf("Cannot have gaps in delay indexes, delay_in%d does not exist for unit: '%.*s'\n",i,UN(moduleName));
      }
      if(seenInputDelay[i] && !seenInput[i]){
        error = 1;
        printf("Cannot have input delay (delay_in%d) if input is missing: '%d'",i,i);
      }
    }
    for(int i = 0; i < seenOutputDelay.size; i++){
      if(!seenOutputDelay[i]){
        error = 1;
        printf("Cannot have gaps in delay indexes, delay_out%d does not exist for unit: '%.*s'\n",i,UN(moduleName));
      }
      if(seenOutputDelay[i] && !seenOutput[i]){
        error = 1;
        printf("Cannot have output delay (delay_out%d) if output is missing: '%d'",i,i);
      }
    }

    TrieMap<HW_Interface*,int>* interCount = PushTrieMap<HW_Interface*,int>(temp);
    for(DECL_InterfaceInfo* ptr = complexHead; ptr; ptr = ptr->next){
      int count = interCount->GetOrElse(ptr->inter,0);
      count = MAX(count,ptr->interfaceIndex + 1);
      interCount->Insert(ptr->inter,count);

      for(int i = 0; i < ptr->wiresSeen.size; i++){
        if(!ptr->wiresSeen[i]){
          int index = i % ptr->inter->wires.size;
          int port  = i / ptr->inter->wires.size;

          String repr = HW_GetWireRepresentation(ptr->inter,index,port,ptr->interfaceIndex,temp);
          printf("Cannot have missing wires in interface, '%.*s' does not exist for unit: '%.*s'\n",UN(repr),UN(moduleName));
          error = 1;
        }
      }
    }
  }

  // Pack config wires ==========================================================
  Arena* out = DECL_State.arena;

  Array<Wire> configs = PushArray<Wire>(out,configCount);
  Array<Wire> states  = PushArray<Wire>(out,stateCount);

  int configIndex = 0;
  int stateIndex = 0;
  for(DECL_ConfigOrState* ptr = configStateHead; ptr; ptr = ptr->next){
    if(ptr->isState){
      states[stateIndex].name = PushString(out,ptr->name);
      states[stateIndex].sizeExpr = ptr->size;
      states[stateIndex].stage = ptr->stage;
      stateIndex += 1;
    } else {
      configs[configIndex].name = PushString(out,ptr->name);
      configs[configIndex].sizeExpr = ptr->size;
      configs[configIndex].stage = ptr->stage;
      configIndex += 1;
    }
  }
  
  // Calculate address gen supported by unit ====================================
  auto FollowsInterface = [](Array<Wire> configs,Array<String> interfaceWireNames) -> bool{
    // No point in even checking
    if(configs.size < interfaceWireNames.size){
      return false;
    }
    
    for(String str : interfaceWireNames){
      bool found = false;
      for(Wire w : configs){
        if(CompareString(w.name,str)){
          found = true;
          break;
        }
      }

      if(!found){
        return false;
      }
    }

    return true;
  };

  Array<String> configNames = Extract(configs,temp,&Wire::name);
  auto CountLoops = [configNames](Array<String> extraLoopsFormats){
    TEMP_REGION(temp,nullptr);
    
    int loops = 2;

    while(true){
      bool containsAllNames = true;
      for(String format : extraLoopsFormats){
        String inst = PushString(temp,format.data,loops);

        if(!Contains(configNames,inst)){
          containsAllNames = false;
          break;
        }
      }

      if(containsAllNames){
        loops += 1;
      } else {
        break;
      }
    }

    return loops - 1;
  };

  bool isGenLike = FollowsInterface(configs,AddressGenBaseParameters);
  bool isExternLike = FollowsInterface(configs,AddressVParameters);
  bool isMemLike = FollowsInterface(configs,AddressMemParameters);
  
  AddressGenInst supportedAddressGen = {};
  if(isExternLike){
    supportedAddressGen.type = AddressGenType_READ;
    supportedAddressGen.loopsSupported = CountLoops(AddressGenExtraFormat);
  } else if(isGenLike){
    supportedAddressGen.type = AddressGenType_GEN;
    supportedAddressGen.loopsSupported = CountLoops(AddressGenExtraFormat);
  } else if(isMemLike){
    supportedAddressGen.type = AddressGenType_MEM;
    supportedAddressGen.loopsSupported = 1;
  }

  // Pack =======================================================================
  String name = moduleName; // TODO: Serialize if needed.
  FUDeclaration* res = DECL_RegisterFU(name,DeclarationType_SINGLE);

  res->meta = meta;
  res->parameters = DECL_Copy(normalizedParams,out);
  res->inputs = PushArray<DECL_PortInfo>(out,inputCount);
  res->outputs = PushArray<DECL_PortInfo>(out,outputCount);
  res->configs = configs;
  res->states = states;
  res->memoryMapped = PushArray<DECL_MMInfo>(out,memoryMappedCount);
  res->externalMemory = PushArray<HW_Instance>(out,externalMemoryCount);
  res->numberInputDelays = inputDelayCount;
  res->numberOutputDelays = outputDelayCount;
  res->numberDatabus = databusCount;
  res->numberDones = (simple & DECL_SingleInterface_DONE) ? 1 : 0;
  res->singleInterfaces = simple;
  res->supportedAddressGen = supportedAddressGen;
  res->error = error;

  for(DECL_SimpleWireInfo* ptr = simpleHead; ptr; ptr = ptr->next){
    int index = ptr->index;
    switch(ptr->type){
     case DECL_SimpleType_INPUT:{
       res->inputs[index].size = SYM_DataW;
       res->inputs[index].delay = ptr->latency;
     } break;
     case DECL_SimpleType_OUTPUT:{
       res->outputs[index].size = SYM_DataW;
       res->outputs[index].delay = ptr->latency;
     } break;
     case DECL_SimpleType_INPUT_DELAY:
     case DECL_SimpleType_OUTPUT_DELAY:{
       // Nothing
     } break;
    }
  }
  
  if(res->memoryMapped.size){
    res->memoryMapped[0].inst = HW_Instantiate(&HW_Default::HW_MM,{.addrSize = memoryMappedAddrSize},out);
    res->memoryMapped[0].addrName = {};
  }

  int externalIndex = 0;
  for(DECL_InterfaceInfo* ptr = complexHead; ptr; ptr = ptr->next){
    if(!HW_IsMemoryInterface(ptr->inter)){
      continue;
    }

    int totalWireSize = ptr->inter->wires.size * ptr->inter->maxPorts;
    
    res->externalMemory[externalIndex].inter = ptr->inter;
    res->externalMemory[externalIndex].index = externalIndex;
    res->externalMemory[externalIndex].wires = PushArray<HW_Wire>(out,totalWireSize);
    for(int i = 0; i < totalWireSize; i++){
      int interIndex = i % ptr->inter->wires.size;
      int port = i / ptr->inter->wires.size;
      
      res->externalMemory[externalIndex].wires[i] = HW_MakeWire(&ptr->inter->wires[interIndex],ptr->wiresSize[i],port);
    }

    externalIndex += 1;
  }

  if(res->error){
    printf("[ERROR] Problem instantiating: %.*s\n",UN(moduleName));
  }

  return res;
}

FUDeclaration* DECL_InstantiateModule(DECL_Meta* meta,Array<DECL_Param> normalizedParams){
  Arena* out = DECL_State.arena;
  TEMP_REGION(temp,out);

  DEBUG_TOP(meta->name);
  DEBUG_STAGE("Start");
  
  // Pack params and compile ====================================================
  TrieMap<String,SYM_Expr>* paramReplace = PushTrieMap<String,SYM_Expr>(temp);
  Array<COM_Param> comParams = PushArray<COM_Param>(temp,normalizedParams.size);
  for(int i = 0; i < normalizedParams.size; i++){
    comParams[i].name = normalizedParams[i].name;
    comParams[i].val = normalizedParams[i].val;

    paramReplace->Insert(normalizedParams[i].name,normalizedParams[i].val);
  }
  COM_Module module = COM_InstantiateModule(meta->compositeUnit,comParams,out);
  COM_Graph baseGraph = COM_Copy(module.graph,false,out).result;

  COM_Graph graph = module.graph;
  
  COM_UnitNodeList order = COM_CalculateDAGOrder(graph,temp);
  COM_DelayResult delays = COM_CalculateDelay(graph,order,temp);

  // Fix delays by inserting buffers where needed ===============================
  {
    int insertedBuffers = 0;
    for(COM_EdgeDelay* ptr = delays.edgeDelays; ptr; insertedBuffers++,ptr = ptr->next){
      FUDeclaration* decl = DECL_GetBufferType(ptr->delay);
      // TODO: Make sure that the name is free to be used.
      COM_Unit* buffer = COM_PushUnit(&graph,SF("_versat_buffer%d",insertedBuffers),decl,out);
      
      COM_Edge* edge = ptr->edge;
      edge->delay = 0;
      COM_InsertMiddle(&graph,edge,buffer,0,0,out);
    }
  }

  DEBUG_STAGE("AfterDelayFix");

  order = COM_CalculateDAGOrder(graph,temp);
  delays = COM_CalculateDelay(graph,order,temp);

  // Store port delays on units =================================================
  for(COM_Unit* unit = graph.units.head; unit; unit = unit->next){
    int totalDelays = unit->decl->numberInputDelays + unit->decl->numberOutputDelays;
    unit->extraDelay = PushArray<int>(out,totalDelays);
  }  
  for(COM_PortDelay* d = delays.portDelays; d; d = d->next){
    COM_Unit* unit = d->unit;
    FUDeclaration* decl = unit->decl;

    int offset = d->port;
    if(d->dir == Direction_OUTPUT){
      offset += unit->decl->numberInputDelays;
    }

    if(unit->extraDelay[offset] != 0){
      LOCATION();
      printf("Unit already has delays set\nCheck what is happening\nIt might happen for output delays but its also an error since we need edges to agree on the same delay.\n");
      continue;
    }

    unit->extraDelay[offset] = d->extraDelay;
  }

  Assert(delays.edgeDelays == nullptr);

  // TODO: Share and static
  TrieSet<DECL_StaticWire>* statics = PushTrieSet<DECL_StaticWire>(temp);
  int configCount = 0;
  int stateCount = 0;
  int inputDelayCount = 0;
  int outputDelayCount = 0;
  int donesCount = 0;
  int memMappedCount = 0;
  int externalCount = 0;
  int databusCount = 0;
  DECL_SingleInterface single = {};
  for(COM_Unit* ptr = graph.units.head; ptr; ptr = ptr->next){
    FUDeclaration* decl = ptr->decl;

    if(!ptr->isStatic){
      configCount += decl->configs.size;
    } else {
      for(Wire w : decl->configs){
        DECL_StaticWire s = {};
        s.wire = w;
        s.meta = meta;
        s.unitName = ptr->name;
        statics->Insert(s);
      }
    }

    for(DECL_StaticWire s : decl->statics){
      statics->Insert(s);
    }

    stateCount += decl->states.size;
    inputDelayCount += decl->numberInputDelays;
    outputDelayCount += decl->numberOutputDelays;
    donesCount += decl->numberDones;
    memMappedCount += decl->memoryMapped.size;
    externalCount += decl->externalMemory.size;
    databusCount += decl->numberDatabus;
    single |= decl->singleInterfaces;
  }

  // Pack =======================================================================
  String name = meta->name; // TODO: Serialize if needed.
  FUDeclaration* res = DECL_RegisterFU(name,DeclarationType_COMPOSITE);

  res->meta = meta;
  res->parameters = DECL_Copy(normalizedParams,out);

  // MARK

  res->configs = PushArray<Wire>(out,configCount);
  res->states = PushArray<Wire>(out,stateCount);
  res->inputs = PushArray<DECL_PortInfo>(out,delays.inputDelays.size);
  res->outputs = PushArray<DECL_PortInfo>(out,delays.outputLatencies.size);
  res->memoryMapped = PushArray<DECL_MMInfo>(out,memMappedCount);
  res->externalMemory = PushArray<HW_Instance>(out,externalCount);
  res->statics = PushArray(out,statics);

  res->singleInterfaces = single;
  res->numberInputDelays = inputDelayCount;
  res->numberOutputDelays = outputDelayCount;
  res->numberDatabus = databusCount;
  res->numberDones = donesCount;

  res->baseGraph = baseGraph;
  res->fixedDelays = graph;
  res->funcs = module.funcs;

  for(int i = 0; i < delays.inputDelays.size; i++){
    res->inputs[i].delay = delays.inputDelays[i];
    res->inputs[i].inputDependencies = {};
    res->inputs[i].size = SYM_DataW;
  }
  for(int i = 0; i < delays.outputLatencies.size; i++){
    res->outputs[i].delay = delays.outputLatencies[i];
    res->inputs[i].inputDependencies = {};
    res->inputs[i].size = SYM_DataW;
  }

  // TODO: We could change to store hierarchical names instead. That way we would have better information about the source of the config/state wire instead of collapsing the information immediatly.
  {
    int stateIndex = 0;
    int configIndex = 0;
    int memMappedIndex = 0;
    int externalIndex = 0;
    for(COM_Unit* ptr = graph.units.head; ptr; ptr = ptr->next){
      for(Wire w : ptr->decl->configs){
        res->configs[configIndex] = w;
        res->configs[configIndex].name = PushString(out,"%.*s_%.*s",UN(ptr->name),UN(res->configs[configIndex].name));
        res->configs[configIndex].sizeExpr = SYM_Replace(w.sizeExpr,paramReplace);
        configIndex += 1;
      }
      for(Wire w : ptr->decl->states){
        res->states[stateIndex] = w;
        res->states[stateIndex].name = PushString(out,"%.*s_%.*s",UN(ptr->name),UN(res->states[stateIndex].name));
        res->states[stateIndex].sizeExpr = SYM_Replace(w.sizeExpr,paramReplace);
        stateIndex += 1;
      }
      for(DECL_MMInfo e : ptr->decl->memoryMapped){
        res->memoryMapped[memMappedIndex].inst = HW_InstantiateParams(e.inst,paramReplace,out);

        if(ptr->decl->type == DeclarationType_SINGLE){
          res->memoryMapped[memMappedIndex].addrName = PushString(out,"%.*s_addr",UN(ptr->name));
        } else {
          res->memoryMapped[memMappedIndex].addrName = PushString(out,"%.*s_%.*s",UN(ptr->name),UN(e.addrName));
        }

        memMappedIndex += 1;
      }
      for(HW_Instance& inst : ptr->decl->externalMemory){
        res->externalMemory[externalIndex] = HW_InstantiateParams(inst,paramReplace,out);
        res->externalMemory[externalIndex].index = externalIndex;
        
        externalIndex += 1;
      }
    }
  }
  
  return res;
}

FUDeclaration* DECL_InstantiateMerge(DECL_Meta* meta,Array<DECL_Param> normalizedParams){
  Arena* out = DECL_State.arena;
  TEMP_REGION(temp,out);

  DEBUG_TOP(meta->name);
  DEBUG_STAGE("MergeCalc");
  
  SP_Node* top = meta->compositeUnit;

  int mergeCount = 0;
  for(SP_Node* ptr = top->childs; ptr; ptr = ptr->next){
    if(ptr->type == SP_Type_VAR_DECL){
      mergeCount += 1;
    }
  }
  Array<FUDeclaration*> types = PushArray<FUDeclaration*>(temp,mergeCount);

  int index = 0;
  for(SP_Node* ptr = top->childs; ptr; index += 1,ptr = ptr->next){
    if(ptr->type != SP_Type_VAR_DECL){
      continue;
    }

    types[index] = DECL_GetType(ptr->token.val,{});
  }

  // Pack params ================================================================
  TrieMap<String,SYM_Expr>* paramReplace = PushTrieMap<String,SYM_Expr>(temp);
  Array<COM_Param> comParams = PushArray<COM_Param>(temp,normalizedParams.size);
  for(int i = 0; i < normalizedParams.size; i++){
    comParams[i].name = normalizedParams[i].name;
    comParams[i].val = normalizedParams[i].val;

    paramReplace->Insert(normalizedParams[i].name,normalizedParams[i].val);
  }

  bool error = false;
  MERGE_Result result = MERGE_Merge(types,DECL_State.arena);
  error |= result.error;

  COM_Graph graph = result.mergedGraph;

  // TODO: nocheckin: copied from COM_InstantiateModule, refactor if possible
  FREE_ARENA(envArena);
  FREE_ARENA(envErrorArena);
  COM_Env envInst = {};
  COM_Env* env = &envInst;
  env->outArena = out;
  env->arena = envArena;
  env->errorArena = envErrorArena;

  for(SP_Node* ptr = top->childs; ptr; ptr = ptr->next){
    if(ptr->type == SP_Type_VAR_DECL){
      String typeName = ptr->token.val;
      String name = ptr->childs->token.val;

      COM_Ent* ent = COM_PushEnt(env,name,COM_EntType_MERGE_TYPE);
      ent->decl = DECL_GetType(typeName,{});
    }
  }

  COM_FunctionNode *funcHead = 0,*funcTail = 0;

  for(SP_Node* ptr = top->childs; ptr; ptr = ptr->next){
    bool isFunction = (ptr->type == SP_Type_FUNC_STATE ||
                       ptr->type == SP_Type_FUNC_MEM ||
                       ptr->type == SP_Type_FUNC_CONFIG);

    if(isFunction){
      COM_FunctionNode* func = COM_CompileFunction(env,ptr,out);
      LL_Append(funcHead,funcTail,next,func);
    }
  }

  // TODO: Share and Static
  // TODO: Copy from instantiateModule, reorganize.
  TrieSet<DECL_StaticWire>* statics = PushTrieSet<DECL_StaticWire>(temp);
  int configCount = 0;
  int stateCount = 0;
  int inputDelayCount = 0;
  int outputDelayCount = 0;
  int donesCount = 0;
  int memMappedCount = 0;
  int externalCount = 0;
  int databusCount = 0;
  DECL_SingleInterface single = {};
  for(COM_Unit* ptr = graph.units.head; ptr; ptr = ptr->next){
    FUDeclaration* decl = ptr->decl;

    if(!ptr->isStatic){
      configCount += decl->configs.size;
    } else {
      for(Wire w : decl->configs){
        DECL_StaticWire s = {};
        s.wire = w;
        s.meta = meta;
        s.unitName = ptr->name;
        statics->Insert(s);
      }
    }

    for(DECL_StaticWire s : decl->statics){
      statics->Insert(s);
    }

    stateCount += decl->states.size;
    inputDelayCount += decl->numberInputDelays;
    outputDelayCount += decl->numberOutputDelays;
    donesCount += decl->numberDones;
    memMappedCount += decl->memoryMapped.size;
    externalCount += decl->externalMemory.size;
    databusCount += decl->numberDatabus;
    single |= decl->singleInterfaces;
  }

  // Pack =======================================================================
  String name = PushString(out,meta->name); // TODO: Serialize if needed.
  FUDeclaration* res = DECL_RegisterFU(name,DeclarationType_MERGED);

  res->meta = meta;
  res->parameters = DECL_Copy(normalizedParams,out);

  // MARK
  res->inputs = {}; // TODO:
  res->outputs = {}; // TODO:

  res->configs = PushArray<Wire>(out,configCount);
  res->states  = PushArray<Wire>(out,stateCount);
  res->memoryMapped = PushArray<DECL_MMInfo>(out,memMappedCount);
  res->externalMemory = PushArray<HW_Instance>(out,externalCount);
  res->statics = PushArray(out,statics);
  res->singleInterfaces = single;
  res->numberInputDelays = inputDelayCount;
  res->numberOutputDelays = outputDelayCount;
  res->numberDatabus = databusCount;
  res->numberDones = donesCount;
  res->baseGraph = graph;
  res->fixedDelays = graph;
  res->singleInterfaces = single;
  res->funcs = funcHead;
  res->error = error;

  {
    int stateIndex = 0;
    int configIndex = 0;
    int memMappedIndex = 0;
    int externalIndex = 0;
    for(COM_Unit* ptr = graph.units.head; ptr; ptr = ptr->next){
      for(Wire w : ptr->decl->configs){
        res->configs[configIndex] = w;
        res->configs[configIndex].name = PushString(out,"%.*s_%.*s",UN(ptr->name),UN(res->configs[configIndex].name));
        res->configs[configIndex].sizeExpr = SYM_Replace(w.sizeExpr,paramReplace);
        configIndex += 1;
      }
      for(Wire w : ptr->decl->states){
        res->states[stateIndex] = w;
        res->states[stateIndex].name = PushString(out,"%.*s_%.*s",UN(ptr->name),UN(res->states[stateIndex].name));
        res->states[stateIndex].sizeExpr = SYM_Replace(w.sizeExpr,paramReplace);
        stateIndex += 1;
      }
      for(DECL_MMInfo e : ptr->decl->memoryMapped){
        res->memoryMapped[memMappedIndex].inst = HW_InstantiateParams(e.inst,paramReplace,out);
        if(ptr->decl->type == DeclarationType_SINGLE){
          res->memoryMapped[memMappedIndex].addrName = PushString(out,"%.*s_addr",UN(ptr->name));
        } else {
          res->memoryMapped[memMappedIndex].addrName = PushString(out,"%.*s_%.*s",UN(ptr->name),UN(e.addrName));
        }

        memMappedIndex += 1;
      }
      for(HW_Instance& inst : ptr->decl->externalMemory){
        res->externalMemory[externalIndex] = HW_InstantiateParams(inst,paramReplace,out);
        res->externalMemory[externalIndex].index = externalIndex;
        
        externalIndex += 1;
      }
    }
  }
  
  return res;
}
