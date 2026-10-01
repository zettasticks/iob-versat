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

      anyError |= converted.anyError;
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
      res = DECL_InstantiateSimple(metaExists,normalizedParams);
    }
  }

  return res;
}

// ======================================
// Instantiation

FUDeclaration* DECL_InstantiateSimple(DECL_Meta* meta,Array<DECL_Param> normalizedParams){
  TEMP_REGION(temp,DECL_State.arena);

  V_Node* top = meta->simpleUnit;
  
  String repr = V_Repr(top,temp);
  printf("%.*s\n",UN(repr));

  Assert(top->type = V_NodeType_MODULE);

  String name = top->token.val;
  
  for(V_Node* ptr = top->attributes; ptr; ptr = ptr->next){
    // TODO: attributes
  }

  // Convert param into sym pair ================================================
  Array<SYM_Pair> params = PushArray<SYM_Pair>(temp,normalizedParams.size);
  for(int i = 0; i < normalizedParams.size; i++){
    params[i].name = normalizedParams[i].name;
    params[i].val = normalizedParams[i].v;
  }

  bool anyError = 0;
  DECL_SingleInterface simple = {};
  for(V_Node* ptr = top->attributes; ptr; ptr = ptr->next){
    if(!V_IsPort(ptr->type)){
      continue;
    }

    bool output = (ptr->type == V_NodeType_OUTPUT);
    bool input  = (ptr->type == V_NodeType_INPUT);
    bool inout  = (ptr->type == V_NodeType_INOUT);
    
    V_Node* range = ptr->childs;
    if(!range){
      range = &V_Node_Range0;
    }
    
    V_SymConvResult topConv = V_ConvertToSym(range->first,params);
    V_SymConvResult bottomConv = V_ConvertToSym(range->second,params);
    
    SYM_EvaluateResult topEval = SYM_ConstantEvaluate(topConv.res);
    SYM_EvaluateResult bottomEval = SYM_ConstantEvaluate(bottomConv.res);

    anyError |= topConv.anyError | bottomConv.anyError | topEval.Error() | bottomEval.Error();
    
    int top = topEval.result;
    int bottom = bottomEval.result;
    int portSize = top - bottom + 1;

    String name = ptr->token.val;

    // MARK
    bool found = 0;
    if(!found){
      auto s = DECL_SingleInterafacesFromName(name);
      found = (s != DECL_SingleInterface_NIL);
      simple |= s;
    }

    - LEFT HERE - We might want to write some code to help define interfaces and to make it easier in the future to add new interfaces (make it easier to parse them and easier to generate verilog code that uses them).
    - Basically we might want to spend some time creating an interface data structure that can represent any type of interface and that offers the kinds of things that we expect in terms of parsing and verilog generation.

    if(!found && CheckFormat("ext_dp_%s_%d_port_%d",name)){
    }
  
    if(!found && CheckFormat("ext_2p_%s",name)){
    }

    if(!found && CheckFormat("ext_2p_%s",name)){
    }

    if(!found && CheckFormat("in%d",name)){
    }

    if(!found && CheckFormat("out%d",name)){
    }

    if(!found && CheckFormat("delay%d",name)){
    }

    if(!found && CheckFormat("in%d",name)){
    }
    
    if(!found && CheckFormat("databus_%s",name)){
    }

    if(!found && CheckFormat("rvalid",name)){
    }
    if(!found && CheckFormat("valid",name)){
    }
    if(!found && CheckFormat("addr",name)){
    }
    if(!found && CheckFormat("rdata",name)){
    }
    if(!found && CheckFormat("wdata",name)){
    }
    if(!found && CheckFormat("wstrb",name)){
    }

    if(!found && input){
      found = 1;
    }

    if(!found && output){
      found = 1;
    }

    if(!found){
      anyError = 1;

      if(inout){
        printf("Versat does not support inout ports");
      } else {
        printf("Error, something very strange happened processing port: %.*s\n",UN(name));
      }
    }
  }  

  FUDeclarationNode* res = PushStruct<FUDeclarationNode>(DECL_State.arena);
  
  res->val.name = PushString(DECL_State.arena,name);

  LL_Append(DECL_State.head,DECL_State.tail,next,res);
  return &res->val;
}





#if 0
ModuleInfo ExtractModuleInfo(Module& module,Arena* out){
  
  for(PortDeclaration decl : module.ports){
    String name = decl.name;
    
    if(CheckFormat("ext_dp_%s_%d_port_%d",decl.name)){
      Array<Value> values = ExtractValues("ext_dp_%s_%d_port_%d",decl.name,temp);

      ExternalMemoryID id = {};
      id.interface = values[1].number;
      id.type = ExternalMemoryType_DP;

      String wire = values[0].str;
      int port = values[2].number;

      Assert(port < 2);

      ExternalMemoryInfo* ext = external->GetOrInsert(id,{});
      if(CompareString(wire,"addr")){
        ext->dp[port].bitSize = decl.range; //SymbolicExpressionFromVerilog(decl.range,out); // decl.range;
      } else if(CompareString(wire,"out")){
        ext->dp[port].dataSizeOut = decl.range;
      } else if(CompareString(wire,"in")){
        ext->dp[port].dataSizeIn = decl.range;
      } else if(CompareString(wire,"write")){
        ext->dp[port].write = true;
      } else if(CompareString(wire,"enable")){
        ext->dp[port].enable = true;
      }
    } else if(CheckFormat("ext_2p_%s",decl.name)){
      ExternalMemoryID id = {};
      id.type = ExternalMemoryType_2P;

      String wire = {};
	  bool out = false;
      if(CheckFormat("ext_2p_%s_%s_%d",decl.name)){
        Array<Value> values = ExtractValues("ext_2p_%s_%s_%d",decl.name,temp);

        wire = values[0].str;
		String outOrIn = values[1].str;
		if(CompareString(outOrIn,"out")){
		  out = true;
		} else if(CompareString(outOrIn,"in")){
		  out = false;
		} else {
		  Assert(false && "Either out or in is mispelled or not present\n");
		}
        id.interface = values[2].number;
      } else if(CheckFormat("ext_2p_%s_%d",decl.name)){
        Array<Value> values = ExtractValues("ext_2p_%s_%d",decl.name,temp);

        wire = values[0].str;
        id.interface = values[1].number;
      } else {
        UNHANDLED_ERROR("TODO: Should be an handled error");
      }

      ExternalMemoryInfo* ext = external->GetOrInsert(id,{});

      if(CompareString(wire,"addr")){
		if(out){
		  ext->tp.bitSizeOut = decl.range;
		} else {
          ext->tp.bitSizeIn = decl.range; // We are using the second port to store the address despite the fact that it's only one port. It just has two addresses.
		}
      } else if(CompareString(wire,"data")){
		if(out){
          ext->tp.dataSizeOut = decl.range;
		} else {
          ext->tp.dataSizeIn = decl.range;
		}
      } else if(CompareString(wire,"write")){
        ext->tp.write = true;
      } else if(CompareString(wire,"read")){
        ext->tp.read = true;
      } else {
        UNHANDLED_ERROR("Should be an handled error");
      }
    } else if(CheckFormat("in%d",decl.name)){
      name = Offset(name,2);
      int index = ParseInt(name);
      Value* delayValue = decl.attributes->Get(VERSAT_LATENCY);

      int delay = 0;
      if(delayValue) delay = delayValue->number;

      inputs[index].delay = delay;
      inputs[index].range = decl.range;
    } else if(CheckFormat("out%d",decl.name)){
      name = Offset(name,3);
      int index = ParseInt(name);
      Value* latencyValue = decl.attributes->Get(VERSAT_LATENCY);

      int latency = 0;
      if(latencyValue) latency = latencyValue->number;

      outputs[index].delay = latency;
      outputs[index].range = decl.range;
    } else if(CheckFormat("delay%d",decl.name)){
      name = Offset(name,5);
      int delay = ParseInt(name);

      info.nDelays = MAX(info.nDelays,delay + 1);
    } else if(  CheckFormat("databus_ready_%d",decl.name)
				|| CheckFormat("databus_valid_%d",decl.name)
				|| CheckFormat("databus_addr_%d",decl.name)
				|| CheckFormat("databus_rdata_%d",decl.name)
				|| CheckFormat("databus_wdata_%d",decl.name)
				|| CheckFormat("databus_wstrb_%d",decl.name)
				|| CheckFormat("databus_len_%d",decl.name)
				|| CheckFormat("databus_last_%d",decl.name)){
      Array<Value> val = ExtractValues("databus_%s_%d",decl.name,temp);

      if(CheckFormat("databus_addr_%d",decl.name)){
        info.databusAddrSize = decl.range;
      }

      info.nIO = val[1].number;
      info.doesIO = true;
    } else if(CheckFormat("rvalid",decl.name)
		   || CheckFormat("valid",decl.name)
		   || CheckFormat("addr",decl.name)
		   || CheckFormat("rdata",decl.name)
		   || CheckFormat("wdata",decl.name)
		   || CheckFormat("wstrb",decl.name)){
      info.memoryMapped = true;

      if(CheckFormat("addr",decl.name)){
        info.memoryMappedBits = decl.range;
      }
    } else if(decl.type == WireDir_INPUT){ // Config
      WireExpression* wire = configs.PushElem();

      Value* stageValue = decl.attributes->Get(VERSAT_STAGE);

      VersatStage stage = VersatStage_COMPUTE;
      
      if(stageValue && stageValue->type == ValueType_STRING){
        String val = stageValue->str;

        if(CompareString(val,"Write")){
          stage = VersatStage_WRITE;
        } else if(CompareString(val,"Read")){
          stage = VersatStage_READ;
        } else {
          Assert(false);
        }
      }
      
      wire->bitSize = decl.range;
      wire->name = decl.name;
      wire->isStatic = decl.attributes->Exists(VERSAT_STATIC);
      wire->stage = stage;
    } else if(decl.type == WireDir_OUTPUT){ // State
      WireExpression* wire = states.PushElem();

      wire->bitSize = decl.range;
      wire->name = decl.name;
    } else {
      NOT_IMPLEMENTED("Implemented as needed, so far all if cases handles all cases so we should never reach here");
    }
  }

  info.configs = configs.AsArray();
  info.states = states.AsArray();
  info.inputs = inputs.AsArray();
  info.outputs = outputs.AsArray();

  if(info.doesIO){
    info.nIO += 1;
  }

  Array<ExternalMemoryInterfaceExpression> interfaces = PushArray<ExternalMemoryInterfaceExpression>(out,external->inserted);
  int index = 0;
  for(Pair<ExternalMemoryID,ExternalMemoryInfo> pair : external){
    ExternalMemoryInterfaceExpression& inter = interfaces[index++];

    inter.interface = pair.first.interface;
    inter.type = pair.first.type;

	switch(inter.type){
	case ExternalMemoryType::ExternalMemoryType_2P:{
	  inter.tp = pair.second.tp;
	} break;
	case ExternalMemoryType::ExternalMemoryType_DP:{
	  inter.dp[0] = pair.second.dp[0];
	  inter.dp[1] = pair.second.dp[1];
	}break;
	}
  }
  info.externalInterfaces = interfaces;

  return info;
}
#endif
