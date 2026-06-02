#include "versatSpecificationParser.hpp"

#include "accelerator.hpp"
#include "debug.hpp"
#include "declaration.hpp"
#include "embeddedData.hpp"
#include "globals.hpp"
#include "memory.hpp"
#include "symbolic.hpp"
#include "templateEngine.hpp"
#include "utils.hpp"
#include "utilsCore.hpp"
#include "userConfigs.hpp"

#include "parser.hpp"
#include "versat.hpp"

// ======================================
// Constants

// static readOnly SpecExpression SPEC_LITERAL_0 = {.val = 0,.type = SpecType_LITERAL};
static readOnly MathExpression MATH_LITERAL_0 = {.val = 0,.type = MathType_LITERAL};

readOnly Entity Entity_Nil = {.inst = &FUInstance_NilInst,.func = &ConfigFunction_Nil,.decl = &FUDeclaration_Nil};

// NOTE: Spec expression is more associated to the equality operator when defining the graph
//       Math expression is everything else
//       One of the biggest differences is that a SpecExpression can contain delay statements "ex: x{0}", while a math expression cannot.

// TODO: We could join these into a single one if we can abstract the differences (which are not a lot).
SpecExpression* ParseSpecExpression(Parser* parser,Arena* out,int bindingPower = 99);
MathExpression* ParseMathExpression(Parser* parser,Arena* out,int bindingPower = 99);

int 
GetUniqueIndex(String baseName,InstanceTable* names){
  TEMP_REGION(temp,nullptr);

  int counter = 0;
  String uniqueName = baseName;
  while(names->Exists(uniqueName)){
    uniqueName = PushString(temp,"%.*s_%d",UN(baseName),counter++);
  }

  return counter;
}

String GetUniqueName(String name,Arena* out,InstanceTable* names,int counterStart = 0){
  int counter = counterStart;
  String uniqueName = name;
  auto mark = MarkArena(out);
  while(names->Exists(uniqueName)){
    PopMark(mark);
    uniqueName = PushString(out,"%.*s_%d",UN(name),counter++);
  }

  names->Insert(uniqueName,nullptr);

  return uniqueName;
}

String GetActualArrayName(String baseName,int index,Arena* out){
  return PushString(out,"%.*s_%d",UN(baseName),index);
}

String GetActualArrayName(String baseName,Array<int> index,Arena* out){
  TEMP_REGION(temp,out);
  
  Array<String> asString = PushArray<String>(temp,index.size);
  for(int i = 0; i < index.size; i++){
    asString[i] = PushString(temp,"%d",index[i]);
  }
  String idPart = JoinStrings(asString,"_",temp);
  
  return PushString(out,"%.*s_%.*s",UN(baseName),UN(idPart));
}

FUDeclaration* InstantiateMerge(MergeDef def){
  TEMP_REGION(temp,nullptr);
  
  int size = def.declarations.size;
  
  Array<FUDeclaration*> decl = PushArray<FUDeclaration*>(temp,size);
  for(int i = 0; i <  size; i++){
    TypeAndInstance tp = def.declarations[i];
    FUDeclaration* d = GetTypeByNameOrFail(tp.typeName.identifier);
    decl[i] = d;
  }

  String name = PushString(globalPermanent,def.name.identifier);

  bool error = false;
  MergeModifier modifier = MergeModifier_NONE;

  for(Token t : def.mergeModifiers){
    Opt<MergeModifier> parsed = META_mergeModifiers_ReverseMap(t.identifier);

    if(!parsed.has_value()){
      printf("Error, merge does not support option: %.*s\n",UN(t.identifier));
      error = true;
    }

    modifier = (MergeModifier) (modifier | parsed.value());
  }

  if(error){
    return nullptr;
  }
  
  return Merge(decl,name,def.specifics,modifier);
}

int GetRangeCount(Env* env,Range<MathExpression*> range){
  int start = env->CalculateConstantExpression(range.start);
  int end = env->CalculateConstantExpression(range.end);
  
  Assert(end >= start);
  return (end - start + 1);
}

// Connection type and number of connections
Pair<PortRangeType,int> GetConnectionInfo(Env* env,Var var){
  Entity ent = env->GetEntity(var.name);

  int expectedRanges = 0;
  if(ent.type == EntityType_FU_ARRAY){
    expectedRanges = ent.dims.size; // arraySize
  }

  if(var.index.size != expectedRanges){
    env->ReportError(var.name,"Too many array subscriptions for this entity");
  }

  int totalRangeCount = 1;
  for(Range<MathExpression*> range : var.index){
    totalRangeCount *= GetRangeCount(env,range);
  }

  int indexCount = totalRangeCount;
  int portCount = GetRangeCount(env,var.extra.port);
  int delayCount = GetRangeCount(env,var.extra.delay);

  if(indexCount == 1 && portCount == 1 && delayCount == 1){
    return {PortRangeType_SINGLE,1};
  }

  // We cannot have more than one range at the same time because otherwise how do we decide how to connnect them?
  if((indexCount != 1 && portCount != 1) ||
     (indexCount != 1 && delayCount != 1) ||
     (portCount != 1 && delayCount != 1)){
    return {PortRangeType_ERROR,0};
  }
  
  if(indexCount != 1){
    return {PortRangeType_ARRAY_RANGE,indexCount};
  }

  if(portCount != 1){
    return {PortRangeType_PORT_RANGE,portCount};
  } else if(delayCount != 1){
    return {PortRangeType_DELAY_RANGE,delayCount};
  }
  
  NOT_POSSIBLE("Every condition should have been checked by now");
  return {PortRangeType_ERROR,0};
}

bool IsValidGroup(Env* env,VarGroup group){
  // TODO: Wether we can match the group or not.
  //       It depends on wether the ranges line up or not. 
  for(Var& var : group.vars){
    if(GetConnectionInfo(env,var).first == PortRangeType_ERROR){
      return false;
    }
  }

  return true;
}

int NumberOfConnections(Env* env,VarGroup group){
  if(!IsValidGroup(env,group)){
    return 0;
  }

  int count = 0;

  for(Var& var : group.vars){
    count += GetConnectionInfo(env,var).second;
  }

  return count;
}

// TODO: Merge this function with the RegisterSubUnit function. There is no purpose to having this be separated.
FUDeclaration* InstantiateModule(String content,ModuleDef def,Array<ParamNameAndValue> topLevelParams){
  Arena* perm = globalPermanent;
  TEMP_REGION(temp,perm);

  Accelerator* circuit = CreateAccelerator(def.name.identifier,AcceleratorPurpose_MODULE);

  FREE_ARENA(envArena);
  FREE_ARENA(envArena2);
  Env* env = StartEnvironment(envArena,envArena2);
  env->circuit = circuit;

  // Pass to check if out instance is used anywhere
  bool addOutputInstance = false;
  for(ConnectionDef& decl : def.connections){
    for(Var v : decl.input.vars){
      if(v.name.identifier == "out"){
        addOutputInstance = true;
        break;
      }
    }

    for(Var v : decl.output.vars){
      if(v.name.identifier == "out"){
        addOutputInstance = true;
        break;
      }
    }
  }
  if(addOutputInstance){
    FUInstance* outInst = CreateOrGetOutput(circuit);

    Entity outEnt = MakeEntity(outInst);
    env->PushEntity("out",outEnt);
    env->table->Insert("out",outInst);
  }

  auto paramList = PushList<ParameterDef>(temp);
  for(ParameterDeclaration param : def.params){
    int val = 0;
   
    bool topOverride = false;
    for(ParamNameAndValue p : topLevelParams){
      if(p.name == param.name.identifier){
        val = p.value;
        topOverride = true;
      }
    }

    if(!topOverride){
      val = env->CalculateConstantExpression(param.defaultValue);
    }

    env->AddParam(param.name,val);

    ParameterDef* def = paramList->PushElem();
    
    def->name = param.name.identifier;
    def->defaultValue = env->SymbolicFromMathExpression(param.defaultValue);
  }
  auto params = PushArray(temp,paramList);

  for(VarDeclaration& decl : def.inputs){
    env->AddInput(decl);
  }

  int shareIndex = 0;
  for(InstanceDeclaration& decl : def.declarations){
    if(decl.modifier == InstanceDeclarationType_SHARE_CONFIG){
      decl.shareIndex = shareIndex++;
    }
  }

  for(InstanceDeclaration& decl : def.declarations){
    for(VarDeclaration& var : decl.declarations){
      env->AddInstance(decl,var);
    }
  }

  for(ConnectionDef& decl : def.connections){
    Assert(decl.type != ConnectionType_NONE);
    if(decl.type == ConnectionType_EQUALITY){
      env->AddEquality(decl);
    } else if(decl.type == ConnectionType_CONNECTION){
      env->AddConnection(decl);
    }
  }
  
  FUDeclaration* res = RegisterSubUnit(circuit,params,SubUnitOptions_BAREBONES);
  
  {
    TEMP_REGION(temp,nullptr);
    auto list = PushList<ConfigFunction*>(temp);
    for(auto funcDecl : def.configs){
      *list->PushElem() = InstantiateConfigFunction(env,&funcDecl,res,content,globalPermanent);
    };
    
    if(res->info.infos.size){
      res->info.infos[0].userFunctions = PushArray(perm,list);
    }
  }

  for(String error : env->errors){
    printf("%.*s\n",UN(error));
  }
  
  return res;
}

bool IsModuleLike(ConstructDef def){
  FULL_SWITCH(def.type){
  case ConstructType_MODULE:
  case ConstructType_MERGE:
  case ConstructType_ITERATIVE:
    return true;
    break;
    break;
}

  Assert(false);
  return false;
}

Array<Token> TypesUsed(ConstructDef def,Arena* out){
  FULL_SWITCH(def.type){
  case ConstructType_MERGE: {
    // TODO: How do we deal with same types being used?
    //       Do we just ignore it?
    Array<Token> result = Extract(def.merge.declarations,out,&TypeAndInstance::typeName);
    
    return result;
  } break;
  case ConstructType_MODULE: {
    Array<Token> result = Extract(def.module.declarations,out,&InstanceDeclaration::typeName);

    // nocheckin: TODO: Check repetition
    return result;
    ///return Unique(result,out);
  } break;
  case ConstructType_ITERATIVE:{
    NOT_IMPLEMENTED("yet");
  };
}

  return {};
}

// TODO: Move this function to a better place, no reason to be inside the spec parser.
FUDeclaration* InstantiateSpecifications(String content,ConstructDef def){
  FULL_SWITCH(def.type){
  case ConstructType_MERGE: {
    return InstantiateMerge(def.merge);
  } break;
  case ConstructType_MODULE: {
    return InstantiateModule(content,def.module);
  } break;
  case ConstructType_ITERATIVE:{
    NOT_IMPLEMENTED("yet");
  }; 
  default: Assert(false);
  }

  return nullptr;
}

// ======================================
// Hierarchical access

Env* StartEnvironment(Arena* freeUse,Arena* freeUse2){
  Env* env = PushStruct<Env>(freeUse);
  env->scopeArena = freeUse;
  env->miscArena = freeUse2;
  env->scopes = PushArray<EnvScope*>(freeUse,99);

  env->currentScope = -1;
  env->PushScope(EnvScopeType_GLOBAL);

  env->errors = PushList<String>(freeUse2);
  env->table = PushTrieMap<String,FUInstance*>(freeUse2);

  return env;
}

void Env::ReportError(Token badToken,String msg){
  // TODO: More detailed error message
  String error = PushString(miscArena,"[%.*s] %.*s: '%.*s'",UN(circuit->name),UN(msg),UN(badToken.identifier));
  *this->errors->PushElem() = error;
}

void Env::PushScope(EnvScopeType type){
  this->currentScope += 1;

  ArenaMark mark = MarkArena(this->scopeArena);
  this->scopes[this->currentScope] = PushStruct<EnvScope>(this->scopeArena);
  this->scopes[this->currentScope]->type = type;
  this->scopes[this->currentScope]->mark = mark;
  this->scopes[this->currentScope]->variable = PushTrieMap<String,Entity>(this->scopeArena);
}

void Env::PopScope(){
  Assert(this->currentScope > 0);

  ArenaMark mark = this->scopes[this->currentScope]->mark;
  PopMark(mark);

  this->currentScope -= 1;
}

FUInstance* Env::CreateInstance(FUDeclaration* type,String name){
  // TODO-0 

  FUInstance* inst = CreateFUInstance(circuit,type,name);

  Entity ent = MakeEntity(inst);
  PushEntity(inst->name,ent);

  return inst;
}

FUInstance* Env::CreateFUInstanceWithDeclaration(FUDeclaration* type,String name,InstanceDeclaration decl){
  // TODO-0 

  FUInstance* inst = CreateFUInstance(circuit,type,name);
  
  for(auto pair : decl.parameters){
    SYM_Expr expr = SymbolicFromMathExpression(pair.second);
    bool result = SetParameter(inst,pair.first,expr);

    if(!result){
      printf("Warning: Parameter %.*s for instance %.*s in module %.*s does not exist\n",UN(pair.first),UN(inst->name),UN(circuit->name));
    }
  }

  return inst;
}

FUInstance* Env::GetFUInstance(Token name,int arrayIndexIfArray){
  TEMP_REGION(temp,nullptr);

  FUInstance* res = nullptr;
  Entity ent = GetEntity(name);

  String asStr = name.identifier;
  if(ent.type == EntityType_FU_ARRAY){
    asStr = GetActualArrayName(asStr,arrayIndexIfArray,temp);
  }

  res = table->GetOrElse(asStr,nullptr);
  return res;
}

FUInstance* Env::GetFUInstance(Token name,Array<int> arrayIndexIfArray){
  TEMP_REGION(temp,nullptr);

  FUInstance* res = nullptr;
  Entity ent = GetEntity(name);

  bool isArray = (ent.type == EntityType_FU_ARRAY);

  String asStr = name.identifier;
  if(isArray){
    asStr = GetActualArrayName(asStr,arrayIndexIfArray,temp);
  }

  res = table->GetOrElse(asStr,nullptr);
  
  if(!res){
    if(isArray){
      ReportError(name,"Out of bounds");
    }
  }

  return res;
}

FUInstance* Env::GetFUInstance(Var var){
  TEMP_REGION(temp,nullptr);
  
  FUInstance* res = nullptr;
  Entity ent = GetEntity(var.name);
    
  String name = var.name.identifier;
  if(ent.type == EntityType_FU_ARRAY){
    Array<int> index = ConvertRangeToIndex(var.index,temp);
    name = GetActualArrayName(name,index,temp);
  }

  res = table->GetOrElse(name,nullptr);
  return res;
}

Entity Env::GetEntity(Token name){
  for(int i = this->currentScope; i >= 0; i--){
    Entity* ent = this->scopes[i]->variable->Get(name.identifier);

    if(ent){
      return *ent;
    }
  }

  ReportError(name,"Entity does not exist");

  return Entity_Nil;
}

void Env::PushEntity(Token name,Entity ent){
  auto res = this->scopes[this->currentScope]->variable->GetOrAllocate(name.identifier);
  if(res.alreadyExisted){
    ReportError(name,"An entity with this name already exists");
    ENTER_DEBUG();
  }
  
  *res.data = ent;
}

void Env::PushEntity(String name,Entity ent){
  auto res = this->scopes[this->currentScope]->variable->GetOrAllocate(name);
  
  if(res.alreadyExisted){
    ENTER_DEBUG();
    ReportError({},"An entity with this name already exists. This should be a programmer error since we should have already verified that this is not possible");
  }
  
  *res.data = ent;
}

Entity Env::GetEntityFromAccess(Entity ent,Token accessName){
  Entity res = Entity_Nil;

  String access = accessName.identifier;

  bool found = false;
  FUDeclaration* decl = ent.decl;

  Direction dir = Direction_NONE;
  int port = 0;
  if(access == "out0"){
    dir = Direction_OUTPUT;
  }
  if(access == "out1"){
    dir = Direction_OUTPUT;
    port = 1;
  }
  if(access == "in0"){
    dir = Direction_INPUT;
  }
  if(access == "in1"){
    dir = Direction_INPUT;
    port = 1;
  }

  if(dir != Direction_NONE){
    res.type = EntityType_MEM_PORT;
    res.dir = dir;
    res.port = port;
    found = true;
  } 
  
  for(Wire& w : decl->configs){
    if(w.name == access){
      res.type = EntityType_CONFIG_WIRE;
      res.name = accessName;

      if(found){
        // TODO-1
        ReportError(accessName,"The identifier has a naming conflict");
      }
      found = true;
    }
  }

  for(Wire& w : decl->states){
    if(w.name == access){
      res.type = EntityType_STATE_WIRE;
      res.name = accessName;

      if(found){
        // TODO-1
        ReportError(accessName,"The identifier has a naming conflict");
      }
      found = true;
    }
  }

  Token funcName = accessName;

  for(MergePartition part : decl->info.infos){
    for(ConfigFunction* func : part.userFunctions){
      if(func->individualName == funcName.identifier){
        res.type = EntityType_FUNCTION;
        res.func = func;
            
        if(found){
          // TODO-1
          ReportError(funcName,"Multiple functions with same name detected\n");
        }
        found = true;
      }
    }
  }

  return res;
}

Array<Entity> Env::GetEntity(ConfigIdentifier* id,Arena* out){
  TEMP_REGION(temp,out);

  ArenaList<Entity>* accessList = PushList<Entity>(temp);

  Entity current = Entity_Nil;
  for(ConfigIdentifier* ptr = id; ptr; ptr = ptr->next){
    Entity next = Entity_Nil;

    FULL_SWITCH(ptr->type){
    case ConfigIdentifierType_BASE:{
      next = GetEntity(ptr->name);
    } break;
    case ConfigIdentifierType_ARRAY:{
      MathExpression* expr = ptr->arrayExpr;

      bool found = false;
      if(!found && current.type == EntityType_FU_ARRAY){
        found = true;
        
        int index = CalculateConstantExpression(expr);
        
        if(index < 0 || index >= current.dims[0]){
          ReportError({},"Outside array bounds");
        }

        String arrayName = PushString(out,"%.*s_%d",UN(current.name.identifier),index);
        
        Array<int> newDims = Offset(current.dims,1);

        if(newDims.size > 0){
          next.type = EntityType_FU_ARRAY;
          
          // TODO-2
          next.name = {};
          next.name.type = TokenType_IDENTIFIER;
          next.name.identifier = arrayName;
          next.name.originalData = arrayName;

          next.dims = newDims;
        } else {
          FUInstance** possibleInst = table->Get(arrayName);

          if(!possibleInst){
            ReportError({},"Inst does not exist");
          } else {
            next = MakeEntity(*possibleInst);
          }
        }
      }

      if(!found && current.type == EntityType_VARIABLE_INPUT){
        if(!found && current.flags == EntityVarFlags_ADDRESS){
          found = true;

          next.type = EntityType_ACCESS_EXPR;
          next.name = ptr->name;
          next.sym = SymbolicFromMathExpression(expr);
        }

        if(!found){
          NOT_IMPLEMENTED("Probably gonna need to generate the 'variable' name");
          ENTER_DEBUG();
        }
      }

      if(!found){
        found = true;
        
        next.type = EntityType_ACCESS_EXPR;
        next.sym = SymbolicFromMathExpression(expr);
      }
    } break;
    case ConfigIdentifierType_ACCESS:{
      Token accessName = ptr->name;
      String access = accessName.identifier;

      if(current.type != EntityType_FU){
        ReportError(accessName,"Trying to access entity that does not support member access");
      }

      if(current.type == EntityType_FU){
        next = GetEntityFromAccess(current,accessName);

        if(Nil(next)){
          ReportError(accessName,"Not found");
        }
      }
    } break;
    case ConfigIdentifierType_FUNC_CALL:{
      Token funcName = ptr->functionName;
      FUDeclaration* decl = current.decl;

      bool found = false;
      for(MergePartition part : decl->info.infos){
        for(ConfigFunction* func : part.userFunctions){
          if(func->individualName == funcName.identifier){
            next.type = EntityType_FUNCTION;
            next.func = func;
            
            if(found){
              // TODO-1
              ReportError(funcName,"Multiple functions with same name detected\n");
            }
            found = true;
          }
        }
      }
    } break;
    }

    if(!Nil(next)){
      *accessList->PushElem() = next;
      current = next;
    } else {
      ENTER_DEBUG();
    }
  }

  Array<Entity> accesses = PushArray<Entity>(out,accessList);
  return accesses;
}

Array<Entity> Env::GetEntity(MathExpression* spec,Arena* out){
  TEMP_REGION(temp,out);

  bool anyIllegalType = false;
  ArenaList<Entity>* accessList = PushList<Entity>(temp);

  auto Recurse = [this,&anyIllegalType,accessList,temp,out](auto Recurse,MathExpression* top,bool topFrame) -> Entity{
    Entity ent = Entity_Nil;
    
    bool save = false;
    FULL_SWITCH(top->type){
    case MathType_NAME: {
      ent = GetEntity(top->name);
      save = true;

      if(ent.type == EntityType_GEN_VALUE){
        save = false;
        ent.type = EntityType_SYM;
        ent.sym = SYM_Lit(ent.val);
      }  
      if(ent.type == EntityType_PARAM){
        save = false;
        ent.type = EntityType_SYM;
        ent.sym = SYM_Lit(ent.val);
      }
      
      bool isVar = false;
      isVar |= (ent.type == EntityType_VARIABLE_INPUT);

      if(isVar) {
        save = false;
        ent.type = EntityType_SYM;
        ent.sym = SYM_Var(top->name.identifier);
      }

    } break;
    case MathType_SINGLE_ACCESS: {
      Entity child = Recurse(Recurse,top->expressions[0],false);
      ent = GetEntityFromAccess(child,top->name);
      save = true;
    } break;
    
    // We only care about the type of entity that makes the array. We already know that the access is an
    // expression and we do not care about it.
    case MathType_ARRAY_ACCESS: {
      ent = GetEntity(top->name);
      Entity indexOrSym = Recurse(Recurse,top->expressions[0],false);

      bool found = false;
      if(!found && ent.type == EntityType_VARIABLE_INPUT){
        bool isAddress = (ent.flags & EntityVarFlags_ADDRESS);
        bool isArray = ent.arrayDims > 0;

        if(!isAddress && isArray){
          int val = 0;
          bool found2 = false;
          if(indexOrSym.type == EntityType_GEN_VALUE){
            found2 = true;
            save = false;

            val = indexOrSym.val;
          }
          if(indexOrSym.type == EntityType_SYM){
            SYM_EvaluateResult res = SYM_ConstantEvaluate(indexOrSym.sym);

            if(!res.Error()){
              found2 = true;
              save = false;

              val = res.result;
            }
          }

          if(found2){
            found = true;
            String trueName = PushString(temp,"%.*s[%d]",UN(ent.name.identifier),val);
            ent.type = EntityType_SYM;
            ent.sym = SYM_Var(trueName);
          }
        }
      }

      if(!found && indexOrSym.type == EntityType_SYM) {
        found = true;
        
        // NOTE: Saving the actual FU first.
        // TODO: Rewrite this code, kinda confusing.
        *accessList->PushElem() = ent;

        ent.type = EntityType_ACCESS_EXPR;
        ent.name = ent.name;
        ent.sym = indexOrSym.sym;
      }
    } break;

    case MathType_OPERATION: {
      save = false;

      Entity left = Recurse(Recurse,top->expressions[0],false);
      Entity right = Recurse(Recurse,top->expressions[1],false);

      SYM_Expr symVal = SYM_Nil;

      FULL_SWITCH(top->op){
      case MathOperation_NIL:{
        // Nothing
      } break;
      case MathOperation_ADD:{
        symVal = left.sym + right.sym;
      } break;
      case MathOperation_SUB:{
        symVal = left.sym - right.sym;
      } break;
      case MathOperation_MUL:{
        symVal = left.sym * right.sym;
      } break;
      case MathOperation_DIV:{
        symVal = left.sym / right.sym;
      } break;
    };

      ent.type = EntityType_SYM;
      ent.sym = symVal;
    } break;

    case MathType_LITERAL: {
      save = false;

      ent.type = EntityType_SYM;
      ent.sym = SYM_Lit(top->val);
    } break;
    case MathType_FUNCTION_CALL:{
      anyIllegalType = true;
    } break;
  }

    if(topFrame){
      save = true;
    }

    if(save && !Nil(ent)){
      *accessList->PushElem() = ent;
    }

    return ent;
  };
 
  Recurse(Recurse,spec,true);

  // TODO: Currently we just return empty but we probably wanna put some kind of error report in here.
  //       Just need to start trying to make some examples that force this in order to check what is going on.
  if(anyIllegalType){
    return {};
  }

  Array<Entity> accesses = PushArray<Entity>(out,accessList);
  return accesses;
}

FUAccess Env::ResolveFU(MathExpression* ptr,Arena* out){
  TEMP_REGION(temp,out);

  Entity entity = Entity_Nil;

  auto list = PushList<MathExpression*>(temp);
  
  FULL_SWITCH(ptr->type){
  case MathType_NAME: {
    entity = GetEntity(ptr->name);
  } break;
  case MathType_ARRAY_ACCESS: {
    entity = GetEntity(ptr->name);

    bool outsideArray = false;
    for(MathExpression* expr : ptr->expressions){
      // If we stopped seeing array expressions to peel away a FU array than just
      // push everything that remains into the leftovers array
      if(entity.type != EntityType_FU_ARRAY){
        outsideArray = true;
      }
      if(outsideArray){
        *list->PushElem() = expr;
        continue;
      }

      if(entity.type == EntityType_FU_ARRAY){
        if(MathType_IsSymbolic(expr->type)){
          int index = CalculateConstantExpression(expr);
        
          if(index < 0 || index >= entity.dims[0]){
            ReportError({},"Outside array bounds");
          }

          String arrayName = PushString(out,"%.*s_%d",UN(entity.name.identifier),index);
        
          Array<int> newDims = Offset(entity.dims,1);

          if(newDims.size > 0){
            entity.type = EntityType_FU_ARRAY;
          
            // TODO-2
            entity.name = {};
            entity.name.type = TokenType_IDENTIFIER;
            entity.name.identifier = arrayName;
            entity.name.originalData = arrayName;

            entity.dims = newDims;
          } else {
            FUInstance** possibleInst = table->Get(arrayName);

            if(!possibleInst){
              ReportError({},"Inst does not exist");
            } else {
              entity = MakeEntity(*possibleInst);
            }
          }
        } else {
          outsideArray = true;
          *list->PushElem() = expr;
        }
      }
    }
  } break;
  case MathType_LITERAL:
  case MathType_OPERATION:
  case MathType_SINGLE_ACCESS:
  case MathType_FUNCTION_CALL: {
  } break;
  }

  FUAccess result = {};
  result.entity = entity;
  result.leftovers = PushArray(out,list);
  return result;
}

Array<int> Env::CalculateArraySize(Array<MathExpression*> exprs){
  if(exprs.size <= 0){
    Assert(false); // Not an error. Programmer cannot call this if empty (not an array)
  }

  Array<int> res = PushArray<int>(scopeArena,exprs.size);

  for(int i = 0; i <  exprs.size; i++){
    MathExpression* expr = exprs[i];
    res[i] = CalculateConstantExpression(expr);
  }

  return res;
}

int Env::CalculateConstantExpression(MathExpression* top){
  TEMP_REGION(temp,nullptr);

  // TODO: Need to report error if we find a non constant value in here.
  //       And then return 0

  SYM_Expr expr = SymbolicFromMathExpression(top);
  SYM_EvaluateResult eval = SYM_ConstantEvaluate(expr);

  return eval.result;
}

Array<int> Env::ConvertRangeToStart(Array<Range<MathExpression*>> range,Arena* out){
  Array<int> res = PushArray<int>(out,range.size);

  for(int i = 0; i <  range.size; i++){
    Range<MathExpression*> r = range[i];
    res[i] = CalculateConstantExpression(r.start);
  }

  return res;
}

Array<int> Env::ConvertRangeToEnd(Array<Range<MathExpression*>> range,Arena* out){
  Array<int> res = PushArray<int>(out,range.size);

  for(int i = 0; i <  range.size; i++){
    Range<MathExpression*> r = range[i];
    res[i] = CalculateConstantExpression(r.end);
  }

  return res;
}
 

Array<int> Env::ConvertRangeToIndex(Array<Range<MathExpression*>> range,Arena* out){
  Array<int> res = PushArray<int>(out,range.size);

  for(int i = 0; i <  range.size; i++){
    Range<MathExpression*> r = range[i];
    
    if(r.start != r.end){
      ReportError({},"Did not expect a range at this point");
    }

    res[i] = CalculateConstantExpression(r.start);
  }

  return res;
}

void Env::AddInput(VarDeclaration var){
  TEMP_REGION(temp,nullptr);

  Entity ent = Entity_Nil;

  if(var.arrayDims.size){
    ent.type = EntityType_FU_ARRAY;
    ent.name = var.name;
    ent.dims = CalculateArraySize(var.arrayDims);

    auto zeroArray = PushArray<int>(temp,ent.dims.size);

    for(auto iter = StartIteration(ent.dims,zeroArray,temp); iter->IsValid(); iter->Advance()){
      Array<int> index = iter->Current();

      String actualName = GetActualArrayName(var.name.identifier,index,temp);
      FUInstance* input = CreateOrGetInput(circuit,actualName,insertedInputs++);
      table->Insert(input->name,input);
    }
  } else {
    FUInstance* input = CreateOrGetInput(circuit,var.name.identifier,insertedInputs++);
    table->Insert(input->name,input);
    
    ent.type = EntityType_FU;
    ent.inst = input;
  }

  ent.isInput = true;

  PushEntity(ent.name,ent);
}

void Env::AddInstance(InstanceDeclaration decl,VarDeclaration var){
  TEMP_REGION(temp,nullptr);

  auto l = PushList<ParamNameAndValue>(temp);
  for(Pair<String,MathExpression*> p : decl.parameters){
    String paramName = p.first;

    bool topLevelOverride = false;

    int val = 0;
    val = CalculateConstantExpression(p.second);

    ParamNameAndValue* v = l->PushElem();
    v->name = paramName;
    v->value = val;
  }
  Array<ParamNameAndValue> params = PushArray(temp,l);

  FUDeclaration* type = GetTypeByName(decl.typeName.identifier,params);
  
  if(!type){
    ReportError(decl.typeName,"Typename does not exist");
    Assert(false);
  }

  Entity ent = Entity_Nil;

  if(var.arrayDims.size){
    ent.type = EntityType_FU_ARRAY;
    ent.name = var.name;
    ent.dims = CalculateArraySize(var.arrayDims);

    auto zeroArray = PushArray<int>(temp,ent.dims.size);

    for(auto iter = StartIteration(ent.dims,zeroArray,temp); iter->IsValid(); iter->Advance()){
      Array<int> index = iter->Current();

      String actualName = GetActualArrayName(var.name.identifier,index,temp);
      FUInstance* inst = CreateFUInstanceWithDeclaration(type,actualName,decl);
      table->Insert(inst->name,inst);
    }
  } else {
    FUInstance* inst = CreateFUInstanceWithDeclaration(type,var.name.identifier,decl);
    table->Insert(inst->name,inst);

    ent = MakeEntity(inst);
  }

  for(auto iter = StartIteration(this,ent,temp); iter.IsValid(); iter.Advance()){
    FUInstance* inst = iter.Current();
    
    switch(decl.modifier){
    case InstanceDeclarationType_NONE: break;
    case InstanceDeclarationType_SHARE_CONFIG:{
      ShareInstanceConfig(inst,decl.shareIndex);
      
      for(Token partialShareName : decl.shareNames){
        bool foundOne = false;
        for(int ii = 0; ii < inst->declaration->configs.size; ii++){
          if(inst->declaration->configs[ii].name == partialShareName.identifier){
            inst->isSpecificConfigShared[ii] = false;
            foundOne = true;
          }
        }

        if(!foundOne){
          TEMP_REGION(temp,nullptr);
          String errorMsg = PushString(temp,"Cannot share config wire since it does not exist for instance '%.*s' of type '%.*s'",UN(inst->name),UN(decl.typeName.identifier));
          ReportError(partialShareName,errorMsg);
        }
      }
    } break;
    case InstanceDeclarationType_STATIC:{
      SetStatic(inst);
    } break;
    }
  }

  PushEntity(ent.name,ent);
}

void Env::AddConnection(ConnectionDef decl){
  Assert(decl.type == ConnectionType_CONNECTION);

  TEMP_REGION(temp,nullptr);

  GroupIterator out = IterateGroup(this,&decl.output,temp);
  GroupIterator in  = IterateGroup(this,&decl.input,temp);

  if(!out.IsValid() || !in.IsValid()){
    return;
  }

  if(out.Size() != in.Size()){
    ReportError(decl.output.vars[0].name,"Different size on connection");
    ReportError(decl.input.vars[0].name,"Different size on connection");
    printf("Left side has size: %d\n",out.Size());
    printf("Right side has size: %d\n",in.Size());
    ENTER_DEBUG();
    return;
  }

  int count = 0;
  while(out.IsValid() && in.IsValid()){
    Connection outVar = out.Current();
    Connection inVar = in.Current();

    FUInstance* outInstance = GetFUInstance(outVar.name,outVar.arrayIndex);
    FUInstance* inInstance = GetFUInstance(inVar.name,inVar.arrayIndex);

    if(outInstance && inInstance){
      int outPort = outVar.port;
      int inPort  = inVar.port;
      ConnectUnits(outInstance,outPort,inInstance,inPort,outVar.delay);
    }

    count += 1;
    out.Advance();
    in.Advance();
  }

  Assert(out.IsValid() == in.IsValid());
}

void Env::AddEquality(ConnectionDef decl){
  TEMP_REGION(temp,nullptr);
  
  Assert(decl.type == ConnectionType_EQUALITY);

  // Only allow one for equality, for now
  Assert(decl.output.vars.size == 1);

  Var outVar = decl.output.vars[0];
  PortExpression portSpecExpression = InstantiateSpecExpression(decl.expression);

  // When dealing with equality, we can just increase array size by accessing higher and higher values.
  if(outVar.IsArrayAccess()){
    NOT_IMPLEMENTED("Need to check if we are actually creating a new since need to allocate dims");
#if 0
    auto res = this->scopes[this->currentScope]->variable->GetOrAllocate(outVar.name.identifier);

    //if(


    Entity* ent = res.data;

    ent->type = EntityType_FU_ARRAY;
    ent->name = outVar.name;
    ent->dims[0] = MAX(ent->dims[0],CalculateConstantExpression(outVar.index[0].high));
#endif
  }

  String name = outVar.name.identifier;
  if(outVar.IsArrayAccess()){
    Array<int> index = ConvertRangeToIndex(outVar.index,temp);
    name = GetActualArrayName(name,index,temp);
  }

  FUInstance* inst = portSpecExpression.inst;
  String uniqueName = GetUniqueName(name,globalPermanent,table);
  inst->name = PushString(globalPermanent,uniqueName);

  Entity ent = MakeEntity(inst);
  PushEntity(inst->name,ent);
  table->Insert(inst->name,inst);
}

void Env::AddParam(Token name,int val){
  Entity ent = Entity_Nil;
  ent.type = EntityType_PARAM;
  ent.name = name;
  ent.val = val;
  PushEntity(name,ent);
}

void Env::AddVariable(Token name,MathExpression* arraySize,EntityVarFlags flags){
  Entity ent = Entity_Nil;
  ent.type = EntityType_VARIABLE_INPUT;
  ent.name = name;
  ent.flags = flags;

  if(arraySize){
    ent.arrayDims = CalculateConstantExpression(arraySize);
  }

  PushEntity(name,ent);
}

#define RESERVED_COMP_TMPL "VERSAT_COMP_%d"

Entity Env::AddComputation(String functionName,Array<SYM_Expr> expressions){
  TEMP_REGION(temp,scopeArena);

  int compScope = -1;

  for(int i = this->currentScope; i >= 0; i--){
    if(this->scopes[i]->type == EnvScopeType_FUNCTION){
      compScope = i;
    }
  }

  Assert(compScope != -1 && "We always associate a computation entity to a function scope");

  EnvScope* scope = this->scopes[compScope];

  /// Check if entity already exists =============================================
  for(int index = 0; index < scope->currentComputationIndex; index++){
    String name = PushString(temp,RESERVED_COMP_TMPL,index);

    Entity* alreadyExists = scope->variable->Get(name);
    if(!alreadyExists){
      continue;
    }

    // Check if parameters match, otherwise not equal =============================
    bool found = true;
    if(found && alreadyExists->type != EntityType_RUNTIME_COMPUTATION){
      ReportError({},"Reserved name detected for something not allowed");
      found = false;
    }
    if(found && alreadyExists->functionName != functionName){
      // TODO: If we do end up implementing partial functions we need to put the logic in here.
      found = false;
    }
    if(found && alreadyExists->args.size != expressions.size){
      // TODO: If we do end up implementing partial functions we need to put the logic in here.
      found = false;
    }
    if(found){
      for(int ii = 0; ii < alreadyExists->args.size; ii++){
        if(alreadyExists->args[ii] != expressions[ii]){
          found = false;
        }
      }
    }

    if(found){
      return *alreadyExists;
    }
  }

  int currentCompIndex = scope->currentComputationIndex++;
  String entityName = PushString(scopeArena,RESERVED_COMP_TMPL,currentCompIndex);

  Entity newEntity = Entity_Nil;
  newEntity.type = EntityType_RUNTIME_COMPUTATION;
  newEntity.name.identifier = entityName;
  newEntity.name.originalData = entityName;
  newEntity.args = CopyArray(expressions,scopeArena);
  newEntity.functionName = PushString(scopeArena,functionName);

  Entity* res = scope->variable->Insert(entityName,newEntity);
  return *res;
}

Array<Entity> Env::GetAllComputations(Arena* out){
  TEMP_REGION(temp,scopeArena);
  
  int compScope = -1;

  for(int i = this->currentScope; i >= 0; i--){
    if(this->scopes[i]->type == EnvScopeType_FUNCTION){
      compScope = i;
    }
  }

  Assert(compScope != -1 && "We always associate a computation entity to a function scope");

  EnvScope* scope = this->scopes[compScope];

  auto list = PushList<Entity>(temp);
  for(int index = 0; index < scope->currentComputationIndex; index++){
    String name = PushString(temp,RESERVED_COMP_TMPL,index);

    Entity ent = scope->variable->GetOrFail(name);
    *list->PushElem() = ent;
  }

  Array<Entity> res = PushArray(out,list);
  return res;
}

void Env::SetGenVariable(Token name,int value){
  Entity* alreadyExists;
  for(int i = this->currentScope; i >= 0; i--){
    alreadyExists = this->scopes[i]->variable->Get(name.identifier);

    if(alreadyExists && alreadyExists->type == EntityType_GEN_VALUE){
      break;
    }
  }

  if(alreadyExists){
    alreadyExists->val = value;
  } else {
    Entity ent = Entity_Nil;
    ent.type = EntityType_GEN_VALUE;
    ent.val = value;

    PushEntity(name,ent);
  }
}

void FUInstanceIterator::Advance(){
  if(iter){
    iter->Advance();
  } else {
    used = true;
  }
}

bool FUInstanceIterator::IsValid(){
  if(iter){
    return iter->IsValid();
  } else {
    return !used;
  }
}

FUInstance* FUInstanceIterator::Current(){
  Assert(IsValid());

  FUInstance* inst = ent.inst;
  if(iter){
    TEMP_REGION(temp,nullptr);
    String baseName = ent.name.identifier;
    String actualName = GetActualArrayName(baseName,iter->Current(),temp);

    inst = GetUnit(this->env->circuit,actualName);
  }
  
  return inst;
}

FUInstanceIterator StartIteration(Env* env,Entity ent,Arena* out){
  TEMP_REGION(temp,out);

  FUInstanceIterator iter = {};
  iter.env = env;
  iter.ent = ent;

  auto zeroArray = PushArray<int>(temp,ent.dims.size);
  if(ent.type == EntityType_FU_ARRAY){
    iter.iter = StartIteration(ent.dims,zeroArray,out);
  } else {
    iter.iter = nullptr;
  }

  return iter;
}

PortExpression Env::InstantiateSpecExpression(SpecExpression* root){
  TEMP_REGION(temp,nullptr);

  Arena* perm = globalPermanent;
  PortExpression res = {};

  switch(root->type){
    // Just to remove warnings. TODO: Change expression so that multiple locations have their own expression struct, instead of reusing the same one.
  case SpecType_LITERAL:{
    int number = root->val;

    TEMP_REGION(temp,perm);
    String toSearch = PushString(temp,"N%d",number);

    FUInstance** found = table->Get(toSearch);

    if(!found){
      String uniqueName = GetUniqueName(toSearch,perm,table);

      FUInstance* digitInst = (FUInstance*) CreateInstance(GetTypeByName("Literal"),uniqueName);
      digitInst->literal = number;
      table->Insert(digitInst->name,digitInst);
      res.inst = digitInst;
    } else {
      res.inst = *found;
    }
  }break;
  case SpecType_VAR:{
    Var var = root->var;  
    String name = var.name.identifier;

    if(var.IsArrayAccess()){
      Array<int> index = ConvertRangeToIndex(var.index,temp);

      name = GetActualArrayName(var.name.identifier,index,globalPermanent);
    }
    
    FUInstance* inst = table->GetOrFail(name);

    res.inst = inst;
    res.extra = var.extra;
  } break;
  case SpecType_OPERATION:{
    bool isReduceForm = (root->expressions.size == 0);
    bool isUnary = (root->expressions.size == 1);
    bool isBinary = (root->expressions.size == 2);

    PortExpression expr0 = {};
    if(isUnary || isBinary){
      expr0 = InstantiateSpecExpression(root->expressions[0]);
    }
    PortExpression expr1 = {};
    if(isBinary){
      expr1 = InstantiateSpecExpression(root->expressions[1]);
    }

    String typeName = {};
    FULL_SWITCH(root->op){
    case SpecOperation_NIL:{
      // Nothing. Parser should report error already so no need to do anything more in here.
    } break;
    case SpecOperation_ADD:{
      typeName = "ADD";
    } break;
    case SpecOperation_SUB:{
      typeName = "SUB";
    } break;
    case SpecOperation_MUL:{
      typeName = "MUL";
    } break;
    case SpecOperation_DIV:{
      typeName = "DIV";
    } break;
    case SpecOperation_NOT:{
      typeName = "NOT";
    } break;
    case SpecOperation_AND:{
      typeName = "AND";
    } break;
    case SpecOperation_OR:{
      typeName = "OR";
    } break;
    case SpecOperation_XOR:{
      typeName = "XOR";
    } break;
    case SpecOperation_RHR:{
      typeName = "RHR";
    } break;
    case SpecOperation_SHR:{
      typeName = "SHR";
    } break;
    case SpecOperation_RHL:{
      typeName = "RHL";
    } break;
    case SpecOperation_SHL:{
      typeName = "SHL";
    } break;
    }

    if(isReduceForm){
      Assert(root->op == SpecOperation_ADD || root->op == SpecOperation_MUL);

      Var var = root->var;
      FUDeclaration* type = GetTypeByName(typeName);
      
      int start = CalculateConstantExpression(var.index[0].start);
      int end = CalculateConstantExpression(var.index[0].end);
      int size = end - start;

      int uniqueIndex = GetUniqueIndex(typeName,table);
      Array<FUInstance*> buffer = PushArray<FUInstance*>(temp,size);

      for(int i = 0; i < size; i++){
        String name = GetActualArrayName(var.name.identifier,i,globalPermanent);
        buffer[i] = table->GetOrFail(name);
      }
      
      Array<FUInstance*> buffer2 = PushArray<FUInstance*>(temp,size);
      
      // Tree shaped instanciation of units.
      int amountOfUnits = size;
      while(amountOfUnits > 1){
        int newAmountOfUnits = 0;
        
        int index = 0;
        while(index < amountOfUnits){
          if(index + 2 <= amountOfUnits){
            FUInstance* first = buffer[index];
            FUInstance* second = buffer[index + 1];

            String uniqueName = GetUniqueName(typeName,perm,table,uniqueIndex);
            FUInstance* newUnit = CreateInstance(type,uniqueName);
            
            ConnectUnits(first,0,newUnit,0);
            ConnectUnits(second,0,newUnit,1);
          
            buffer2[newAmountOfUnits++] = newUnit;
            index += 2;
          } else {
            buffer2[newAmountOfUnits++] = buffer[index];
            index += 1;
          }
        }
        
        amountOfUnits = newAmountOfUnits;
        buffer = buffer2;
      }
      
      res.inst = buffer[0];
      res.extra.port.end  = res.extra.port.start  = &MATH_LITERAL_0;
      res.extra.delay.end = res.extra.delay.start = &MATH_LITERAL_0;
    }

    if(isUnary){
      Assert(root->op == SpecOperation_NOT || root->op == SpecOperation_SUB);
      
      String permName = GetUniqueName(typeName,perm,table);
      FUInstance* inst = CreateInstance(GetTypeByName(typeName),permName);
      table->Insert(inst->name,inst);

      int start = CalculateConstantExpression(expr0.extra.port.start);
      int delay = CalculateConstantExpression(expr0.extra.delay.start);
      ConnectUnits(expr0.inst,start,inst,0,delay);

      res.inst = inst;
      res.extra.port.end  = res.extra.port.start  = &MATH_LITERAL_0;
      res.extra.delay.end = res.extra.delay.start = &MATH_LITERAL_0;
    }

    if(isBinary){
      // Assuming right now very simple cases, no port range and no delay range
      Assert(expr1.extra.port.start == expr1.extra.port.end);
      Assert(expr1.extra.delay.start == expr1.extra.delay.end);

      String typeStr = typeName;
      FUDeclaration* type = GetTypeByName(typeStr);
      String uniqueName = GetUniqueName(type->name,perm,table);

      FUInstance* inst = CreateInstance(type,uniqueName);
      table->Insert(inst->name,inst);

      int start0 = CalculateConstantExpression(expr0.extra.port.start);
      int delay0 = CalculateConstantExpression(expr0.extra.delay.start);

      int start1 = CalculateConstantExpression(expr1.extra.port.start);
      int delay1 = CalculateConstantExpression(expr1.extra.delay.start);

      ConnectUnits(expr0.inst,start0,inst,0,delay0);
      ConnectUnits(expr1.inst,start1,inst,1,delay1);

      res.inst = inst;
      res.extra.port.end  = res.extra.port.start  = &MATH_LITERAL_0;
      res.extra.delay.end = res.extra.delay.start = &MATH_LITERAL_0;
    }
  } break;
  }

  Assert(res.inst);
  return res;
}

SYM_Expr Env::SymbolicFromMathExpression(MathExpression* spec){
  TEMP_REGION(temp,nullptr);

  auto Recurse = [temp,this](auto Recurse,MathExpression* top) -> SYM_Expr{
    SYM_Expr res = SYM_Nil;

    switch(top->type){
    case MathType_OPERATION:{
      if(top->expressions.size == 1){
        if(top->op == MathOperation_SUB){
          SYM_Expr left  = Recurse(Recurse,top->expressions[0]);
          res = -left;
        } else {
          NOT_IMPLEMENTED();
        }
      } else {
        SYM_Expr left  = Recurse(Recurse,top->expressions[0]);
        SYM_Expr right = Recurse(Recurse,top->expressions[1]);

        FULL_SWITCH(top->op){
        case MathOperation_NIL:{
          // Nothing
        } break;
        case MathOperation_ADD:{
          res = left + right;
        } break;
        case MathOperation_SUB:{
          res = left - right;
        } break;
        case MathOperation_MUL:{
          res = left * right;
        } break;
        case MathOperation_DIV:{
          res = left / right;
        } break;
        }
      }
    } break;
    case MathType_NAME:{
      Entity ent = GetEntity(top->name);

      bool found = false;
      if(!found && ent.type == EntityType_GEN_VALUE){
        found = true;
        res = SYM_Lit(ent.val);
      }
      if(!found && ent.type == EntityType_PARAM){
        found = true;
        res = SYM_Lit(ent.val);
      }

      if(!found){
        res = SYM_Var(top->name.identifier);
      }
    } break;
    case MathType_LITERAL:{
      res = SYM_Lit(top->val);
    } break;
    case MathType_ARRAY_ACCESS: {
      Entity ent = GetEntity(top->name);

      if(ent.type == EntityType_VARIABLE_INPUT){
        // TODO: Technically we should recurse but I do not want to convert from Sym_expr back to an integer 
        //       when we can just access this directly.
        MathExpression* index = top->expressions[0];
        Token indexName = index->name;
        Entity indexVal = GetEntity(indexName);

        String fullName = PushString(temp,"%.*s[%d]",UN(top->name.originalData),indexVal.val);
        res = SYM_Var(fullName);
      }
    } break;

    case MathType_FUNCTION_CALL: {
      Array<MathExpression*> args = top->expressions;

      Array<SYM_Expr> expressions = PushArray<SYM_Expr>(temp,args.size);
      for(int i = 0; i <  args.size; i++){
        expressions[i] = SymbolicFromMathExpression(args[i]);
      }

      Entity funcEnt = AddComputation(top->name.identifier,expressions);
      res = SYM_Var(funcEnt.name.identifier);
    } break;

    case MathType_SINGLE_ACCESS:  Assert(false);
    }

    return res;
  };

  SYM_Expr res = Recurse(Recurse,spec);
  return res;
}

MathExpression* ParseNumberOnly(Parser* parser,Arena* out){
  MathExpression* res = PushStruct<MathExpression>(out);

  res->val = parser->ExpectNext(TokenType_NUMBER).number;
  res->type = MathType_LITERAL;

  return res;
}

MathExpression* ParseNumberOrSingleIdentifier(Parser* parser,Arena* out){
  MathExpression* res = PushStruct<MathExpression>(out);

  Token tok = parser->NextToken();

  if(tok.type != TokenType_IDENTIFIER && tok.type != TokenType_NUMBER){
    parser->ReportError("Unexpected token");
  }

  if(tok.type == TokenType_IDENTIFIER){
    res->name = tok;
    res->type = MathType_NAME;
  }
  if(tok.type == TokenType_NUMBER){
    res->val = tok.number;
    res->type = MathType_LITERAL;
  }

  return res;
}

Range<MathExpression*> ParseRange(Parser* parser,Arena* out){
  Range<MathExpression*> res = {};

  MathExpression* n1 = ParseNumberOnly(parser,out);

  res.start = n1;
  res.end = n1;

  if(parser->IfNextToken(TokenType_DOUBLE_DOT)){
    res.end = ParseNumberOnly(parser,out);
  }
  
  return res;
}

Range<MathExpression*> ParseExprRange(Parser* parser,Arena* out){
  Range<MathExpression*> res = {};

  MathExpression* n1 = ParseMathExpression(parser,out);

  res.start = n1;
  res.end = n1;

  if(parser->IfNextToken(TokenType_DOUBLE_DOT)){
    res.end = ParseMathExpression(parser,out);
  }
  
  return res;
}

Var ParseVar(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);
  
  Token name = parser->ExpectNext(TokenType_IDENTIFIER);

  auto list = PushList<Range<MathExpression*>>(temp); 
  while(parser->IfNextToken('[')){
    Range<MathExpression*> range = ParseExprRange(parser,out);
    *list->PushElem() = range;
    parser->ExpectNext(']');
  }
  
  MathExpression* delayStart = &MATH_LITERAL_0;
  MathExpression* delayEnd = &MATH_LITERAL_0;
  if(parser->IfNextToken('{')){
    Range<MathExpression*> range = ParseExprRange(parser,out);
    delayStart = range.start;
    delayEnd = range.end;

    parser->ExpectNext('}');
  }

  MathExpression* portStart = &MATH_LITERAL_0;
  MathExpression* portEnd = &MATH_LITERAL_0;
  if(parser->IfNextToken(':')){
    Range<MathExpression*> range = ParseRange(parser,out);

    portStart = range.start;
    portEnd = range.end;
  }

  Var var = {};
  var.name = name;
  var.index = PushArray(out,list);
  var.extra.delay = {delayStart,delayEnd};
  var.extra.port = {portStart,portEnd};

  return var;
}

VarDeclaration ParseVarDeclaration(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);

  VarDeclaration res = {};

  res.name = parser->ExpectNext(TokenType_IDENTIFIER);
  
  // TODO: We should integrate the array parsing logic with this one
  auto list = PushList<MathExpression*>(temp);

  while(parser->IfNextToken('[')){
    MathExpression* arraySize = ParseMathExpression(parser,out);

    parser->ExpectNext(']');

    *list->PushElem() = arraySize;
  }

  res.arrayDims = PushArray(out,list);
  
  return res;
}

Array<VarDeclaration> ParseModuleInputDeclaration(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);

  auto vars = PushList<VarDeclaration>(temp);

  parser->ExpectNext('(');

  if(parser->IfNextToken(')')){
    return {};
  }
  
  while(!parser->Done()){
    VarDeclaration var = ParseVarDeclaration(parser,out);
    *vars->PushElem() = var;
    
    if(parser->IfNextToken(',')){
      continue;
    } else {
      break;
    }
  }

  parser->ExpectNext(')');

  Array<VarDeclaration> res = PushArray(out,vars);
  return res;
}

InstanceDeclaration ParseInstanceDeclaration(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);
  InstanceDeclaration res = {};

  InstanceDeclarationType modifier = InstanceDeclarationType_NONE;

  // TODO: Is it correct if we see a bunch of repeated modifiers? Think 'static static static'.
  while(1){
    Token potentialModifier = parser->PeekToken();

    InstanceDeclarationType parsedModifier = InstanceDeclarationType_NONE;
    if(potentialModifier.type == TokenType_KEYWORD_DEBUG){
      parser->NextToken();

      res.debug = true;
    } else if(potentialModifier.type == TokenType_KEYWORD_STATIC){
      parser->NextToken();
      parsedModifier = InstanceDeclarationType_STATIC;
    } else if(potentialModifier.type == TokenType_KEYWORD_SHARE){
      parser->NextToken();
      parser->ExpectNext('(');
      parser->ExpectNext(TokenType_KEYWORD_CONFIG);
      parser->ExpectNext(')');

      parsedModifier = InstanceDeclarationType_SHARE_CONFIG;
      
      res.typeName = parser->ExpectNext(TokenType_IDENTIFIER);

      if(parser->IfNextToken('(')){
        // TODO: For now, we assume that every wire specified inside the spec file is a negative (remove share).
        auto toShare = PushList<Token>(temp);
        while(!parser->Done()){
          Token name = parser->ExpectNext(TokenType_IDENTIFIER);

          *toShare->PushElem() = name;

          if(parser->IfNextToken(',')){
            continue;
          } else {
            break;
          }
        }
      
        parser->ExpectNext(')');

        res.shareNames = PushArray(out,toShare);
      }

      parser->ExpectNext('{');

      auto varList = PushList<VarDeclaration>(temp);
    
      while(!parser->Done()){
        Token peek = parser->PeekToken();

        if(peek.type == '}'){
          break;
        }

        *varList->PushElem() = ParseVarDeclaration(parser,out);
      
        parser->ExpectNext(';');
      }
      res.declarations = PushArray(out,varList);
    
      parser->ExpectNext('}');
      res.modifier = parsedModifier;
      // TODO: Weird logic. Already caused a bug, potentially need to rewrite this.
      return res;
    } else {
      break;
    }

    if(modifier == InstanceDeclarationType_NONE){
      modifier = parsedModifier;
    }
  }

  res.typeName = parser->ExpectNext(TokenType_IDENTIFIER);

  Token possibleParameters = parser->PeekToken();
  auto list = PushList<Pair<String,MathExpression*>>(temp);
  if(possibleParameters.type == '#'){
    parser->NextToken();
    parser->ExpectNext('(');

    while(!parser->Done()){
      parser->ExpectNext('.');
      Token parameterName = parser->ExpectNext(TokenType_IDENTIFIER);

      parser->ExpectNext('(');

      MathExpression* expr = ParseMathExpression(parser,out);

      parser->ExpectNext(')');
      
      String savedParameter = PushString(out,parameterName.identifier);
      *list->PushElem() = {savedParameter,expr}; 

      if(parser->IfNextToken(',')){
        continue;
      }

      break;
    }
    parser->ExpectNext(')');

    res.parameters = PushArray(out,list);
  }

  VarDeclaration varDecl = ParseVarDeclaration(parser,out);

  res.declarations = PushArray<VarDeclaration>(out,1);
  res.declarations[0] = varDecl;
  res.modifier = modifier;

  parser->ExpectNext(';');

  return res;
}

// A group can also be a single var. It does not necessary mean that it must be of the form {...}
VarGroup ParseVarGroup(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);
  
  if(parser->IfNextToken('{')){
    auto vars = PushList<Var>(temp);

    while(!parser->Done()){
      Var var = ParseVar(parser,out);

      *vars->PushElem() = var;

      Token sepOrEnd = parser->NextToken();
      if(sepOrEnd.type == ','){
        continue;
      } else if(sepOrEnd.type == '}'){
        break;
      } else {
        parser->ReportUnexpectedToken(sepOrEnd,{TOK_TYPE(','),TOK_TYPE('}')});
      }
    }

    VarGroup res = {};
    res.vars = PushArray(out,vars);
    return res;
  } else {
    Var var = ParseVar(parser,out);

    VarGroup res = {};
    res.vars = PushArray<Var>(out,1);
    res.vars[0] = var;
    return res;
  }
}

MathExpression* ParseMathExpression(Parser* parser,Arena* out,int bindingPower){
  MathExpression* topUnary = nullptr;
  MathExpression* innerMostUnary = nullptr;

  MathExpression* res = nullptr;
  
  // Parse unary
  while(!parser->Done()){
    MathExpression* parsed = nullptr;
    if(!parsed &&  parser->IfNextToken('-')){
      parsed = PushStruct<MathExpression>(out);
      parsed->op = MathOperation_SUB;
    }

    if(parsed){
      parsed->type = MathType_OPERATION;
    }

    if(parsed && !topUnary){
      topUnary = parsed;
      innerMostUnary = parsed;
      continue;
    }

    if(parsed){
      innerMostUnary->expressions = PushArray<MathExpression*>(out,1);
      innerMostUnary->expressions[0] = parsed;
      innerMostUnary = parsed;
      continue;
    }

    break;
  }

  // Parse atom
  Token atom = parser->PeekToken();
  if(atom.type == '('){
    parser->ExpectNext('(');

    res = ParseMathExpression(parser,out);

    parser->ExpectNext(')');
  } else if(atom.type == TokenType_NUMBER){
    Token number = parser->ExpectNext(TokenType_NUMBER);
    res = PushStruct<MathExpression>(out);

    res->type = MathType_LITERAL;
    res->val = number.number;
  } else if(atom.type == TokenType_IDENTIFIER){
    TEMP_REGION(temp,out);

    Token name = parser->ExpectNext(TokenType_IDENTIFIER);

    res = PushStruct<MathExpression>(out);
    res->name = name;
    res->type = MathType_NAME;

    if(parser->IfPeekToken('[')){
      auto accesses = PushList<MathExpression*>(temp);       
      
      while(parser->IfNextToken('[')){
        MathExpression* insideArray = ParseMathExpression(parser,out);

        *accesses->PushElem() = insideArray;

        parser->ExpectNext(']');
      }

      res->expressions = PushArray(out,accesses);
      res->type = MathType_ARRAY_ACCESS;
    }

    // TODO: This is mostly for state right side.
    //       We might eventually just separate this into different parsing functions.
    if(parser->IfNextToken('.')){
      Token singleAccessName = parser->ExpectNext(TokenType_IDENTIFIER);
      
      MathExpression* singleAccess = PushStruct<MathExpression>(out);
      singleAccess->type = MathType_SINGLE_ACCESS; //MathType_NAME;
      singleAccess->name = singleAccessName;
      singleAccess->expressions = PushArray<MathExpression*>(out,1);
      singleAccess->expressions[0] = res;

      res = singleAccess;
    }

    if(parser->IfNextToken('(')){
      auto args = PushList<MathExpression*>(temp);       
      
      while(!parser->Done()){
        MathExpression* arg = ParseMathExpression(parser,out);

        *args->PushElem() = arg;
        
        if(parser->IfNextToken(',')){
          continue;
        }
        
        break;
      }

      parser->ExpectNext(')');
      
      res->expressions = PushArray(out,args);
      res->type = MathType_FUNCTION_CALL;
    }
  } else {
    // TODO: Better error reporting
    parser->ReportUnexpectedToken(atom,{});
  }

  if(topUnary){
    innerMostUnary->expressions = PushArray<MathExpression*>(out,1);
    innerMostUnary->expressions[0] = res;

    res = topUnary;
  }

  struct OpInfo{
    TokenType type;
    int bindingPower;
    MathOperation op;
  };

  // TODO: This should be outside the function itself.
  TEMP_REGION(temp,out);

  // TODO: Need to double check binding power
  auto infos = PushArray<OpInfo>(temp,4);
  infos[0] = {TOK_TYPE('/'),0,MathOperation_DIV};
  infos[1] = {TOK_TYPE('*'),0,MathOperation_MUL};

  infos[2] = {TOK_TYPE('+'),1,MathOperation_ADD};
  infos[3] = {TOK_TYPE('-'),1,MathOperation_SUB};
  
  // Parse binary ops.
  while(!parser->Done()){
    Token peek = parser->PeekToken();

    bool continueOuter = false;
    for(OpInfo info : infos){
      if(peek.type == info.type){
        if(info.bindingPower < bindingPower){
          parser->NextToken();

          MathExpression* right = ParseMathExpression(parser,out,info.bindingPower);
      
          MathExpression* op = PushStruct<MathExpression>(out);

          op->op = info.op;
          op->expressions = PushArray<MathExpression*>(out,2);
          op->expressions[0] = res;
          op->expressions[1] = right;

          res = op;
          continueOuter = true;
          break;
        }
      }
    }

    if(continueOuter){
      continue;
    }

    break;
  }
  
  return res;
}

SpecExpression* ParseSpecExpression(Parser* parser,Arena* out,int bindingPower){
  SpecExpression* topUnary = nullptr;
  SpecExpression* innerMostUnary = nullptr;

  SpecExpression* res = nullptr;
  
  // Parse unary
  while(!parser->Done()){
    SpecExpression* parsed = nullptr;
    if(!parsed && parser->IfNextToken('~')){
      parsed = PushStruct<SpecExpression>(out);
      parsed->op = SpecOperation_NOT;
    }
    if(!parsed && parser->IfNextToken('-')){
      parsed = PushStruct<SpecExpression>(out);
      parsed->op = SpecOperation_SUB;
    }

    if(parsed){
      parsed->type = SpecType_OPERATION;
    }

    if(parsed && !topUnary){
      topUnary = parsed;
      innerMostUnary = parsed;
      continue;
    }

    if(parsed){
      innerMostUnary->expressions = PushArray<SpecExpression*>(out,1);
      innerMostUnary->expressions[0] = parsed;
      innerMostUnary = parsed;
      continue;
    }

    break;
  }

  // Parse atom
  Token atom = parser->PeekToken();

  if(atom.type == '+' || atom.type == '*'){
    // Unary addition
    Token peek = parser->PeekToken(0);
    Token peek2 = parser->PeekToken(1);

    if(peek.type == '+' && peek2.type == '('){
      parser->NextToken();
      parser->NextToken();

      res = PushStruct<SpecExpression>(out);
      res->op = SpecOperation_ADD;
      res->var = ParseVar(parser,out);

      parser->ExpectNext(')');
    }
    if(peek.type == '*' && peek2.type == '('){
      parser->NextToken();
      parser->NextToken();

      res = PushStruct<SpecExpression>(out);
      res->op = SpecOperation_MUL;
      res->var = ParseVar(parser,out);
        
      parser->ExpectNext(')');
    }
  } else if(atom.type == '('){
    parser->ExpectNext('(');

    res = ParseSpecExpression(parser,out);

    parser->ExpectNext(')');
  } else if(atom.type == TokenType_NUMBER){
    Token number = parser->ExpectNext(TokenType_NUMBER);
    res = PushStruct<SpecExpression>(out);

    res->type = SpecType_LITERAL;
    res->val = number.number;
  } else if(atom.type == TokenType_IDENTIFIER){
    TEMP_REGION(temp,out);

    Var var = ParseVar(parser,out);
    
    res = PushStruct<SpecExpression>(out);
    res->var = var;
    res->type = SpecType_VAR;
  } else {
    // TODO: Better error reporting
    parser->ReportUnexpectedToken(atom,{});
  }

  if(topUnary){
    innerMostUnary->expressions = PushArray<SpecExpression*>(out,1);
    innerMostUnary->expressions[0] = res;

    res = topUnary;
  }

  struct OpInfo{
    TokenType type;
    int bindingPower;
    SpecOperation op;
  };

  // TODO: This should be outside the function itself.
  TEMP_REGION(temp,out);
  auto infos = PushArray<OpInfo>(temp,11);

  // TODO: Need to double check binding power
  infos[0] = {TOK_TYPE('&'),0,SpecOperation_AND};
  infos[1] = {TOK_TYPE('|'),0,SpecOperation_OR};
  infos[2] = {TOK_TYPE('^'),0,SpecOperation_XOR};

  infos[3] = {TokenType_ROTATE_LEFT,1,SpecOperation_RHL};
  infos[4] = {TokenType_ROTATE_RIGHT,1,SpecOperation_RHR};
  infos[5] = {TokenType_SHIFT_LEFT,1,SpecOperation_SHL};
  infos[6] = {TokenType_SHIFT_RIGHT,1,SpecOperation_SHR};

  infos[7]  = {TOK_TYPE('*'),2,SpecOperation_MUL};
  infos[8] = {TOK_TYPE('/'),2,SpecOperation_DIV};

  infos[9] = {TOK_TYPE('+'),3,SpecOperation_ADD};
  infos[10] = {TOK_TYPE('-'),3,SpecOperation_SUB};
  
  // Parse binary ops.
  while(!parser->Done()){
    Token peek = parser->PeekToken();

    bool continueOuter = false;
    for(OpInfo info : infos){
      if(peek.type == info.type){
        if(info.bindingPower < bindingPower){
          parser->NextToken();

          SpecExpression* right = ParseSpecExpression(parser,out,info.bindingPower);
      
          SpecExpression* op = PushStruct<SpecExpression>(out);

          op->op = info.op;
          op->expressions = PushArray<SpecExpression*>(out,2);
          op->expressions[0] = res;
          op->expressions[1] = right;

          res = op;
          continueOuter = true;
          break;
        }
      }
    }

    if(continueOuter){
      continue;
    }

    break;
  }
  
  return res;
}

ConnectionDef ParseConnection(Parser* parser,Arena* out){
  VarGroup outPortion = ParseVarGroup(parser,out);

  ConnectionType type = ConnectionType_NONE;
  
  if(parser->IfNextToken('=')){
    type = ConnectionType_EQUALITY;
  } else if(parser->IfNextToken(TokenType_XOR_EQUAL)){
    // TODO: We parse it but we do not use it. We probably wanna remove the testcase that uses this.
    type = ConnectionType_EQUALITY;
  } else if(parser->IfNextToken(TokenType_ARROW)){
    type = ConnectionType_CONNECTION;
  } else {
    parser->ReportUnexpectedToken(parser->NextToken(),{TOK_TYPE('='),TokenType_ARROW});
  }

  ConnectionDef def = {};
  SpecExpression* expr = nullptr;
  VarGroup inPortion = {};

  if(type == ConnectionType_EQUALITY){
    expr = ParseSpecExpression(parser,out);
  } else if(type == ConnectionType_CONNECTION){
    inPortion = ParseVarGroup(parser,out);
  }

  parser->ExpectNext(';');
  
  def.type = type;
  def.expression = expr;
  def.output = outPortion;
  def.input = inPortion;

  return def;
}

// TODO: nocheckin - remove forward decl
ConfigFunctionDef* ParseConfigFunction(Parser* parser,Arena* out);

ParameterDeclaration ParseParameterDeclaration(Parser* parser,Arena* out){
  ParameterDeclaration res = {};

  res.name = parser->ExpectNext(TokenType_IDENTIFIER);
  
  if(parser->IfNextToken('=')){
    res.defaultValue = ParseMathExpression(parser,out);
  }

  return res;
}

// Any returned String points to tokenizer content.
// As long as tokenizer is valid, strings returned by this function are also valid.
ModuleDef ParseModuleDef(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);

  parser->ExpectNext(TokenType_KEYWORD_MODULE);

  Token name = parser->ExpectNext(TokenType_IDENTIFIER);

  Array<ParameterDeclaration> params = {};
  if(parser->IfNextToken('#')){
    parser->ExpectNext('(');

    auto paramList = PushList<ParameterDeclaration>(temp);
    
    while(!parser->Done()){
      ParameterDeclaration param = ParseParameterDeclaration(parser,out);
      *paramList->PushElem() = param;
    
      if(parser->IfNextToken(',')){
        continue;
      } else {
        break;
      }
    }

    parser->ExpectNext(')');

    params = PushArray(out,paramList);
  }

  Array<VarDeclaration> vars = ParseModuleInputDeclaration(parser,out);

  //Token outputs = {};
  if(parser->IfNextToken(TokenType_ARROW)){
    // TODO: Right now we do not care about output info being given by the user.
    //       We probably should. We still parse it, we just ignore it for now.
    /* outputs = */ parser->ExpectNext(TokenType_NUMBER);
  }
  
  ArenaList<InstanceDeclaration>* decls = PushList<InstanceDeclaration>(temp);
  parser->ExpectNext('{');
  
  while(!parser->Done()){
    Token peek = parser->PeekToken();

    if(peek.type == ';'){
      parser->NextToken();
      continue;
    }

    if(peek.type == '#' || peek.type == TokenType_DOUBLE_HASHTAG || peek.type == '}'){
      break;
    }
    
    InstanceDeclaration decl = ParseInstanceDeclaration(parser,out);
    *decls->PushElem() = decl;
  }
  Array<InstanceDeclaration> declarations = PushArray(out,decls);

  ArenaList<ConnectionDef>* cons = PushList<ConnectionDef>(temp);
  if(parser->IfNextToken('#')){
    while(!parser->Done()){
      Token peek = parser->PeekToken();

      if(peek.type == ';'){
        parser->NextToken();
        continue;
      }

      if(peek.type == '}' || peek.type == TokenType_DOUBLE_HASHTAG){
        break;
      }

      ConnectionDef con = ParseConnection(parser,out);
 
      *cons->PushElem() = con;
    }
  }

  auto configFunctions = PushList<ConfigFunctionDef>(temp);

  // nocheckin
  if(parser->IfNextToken(TokenType_DOUBLE_HASHTAG)){
    while(!parser->Done()){
      // nocheckin TODO: Probably remove this and move the logic from the function to here
      
      bool isConfigFunctionStart = false;
      
      isConfigFunctionStart |= parser->IfPeekToken(TokenType_KEYWORD_CONFIG);
      isConfigFunctionStart |= parser->IfPeekToken(TokenType_KEYWORD_STATE);
      isConfigFunctionStart |= parser->IfPeekToken(TokenType_KEYWORD_MEM);

      if(isConfigFunctionStart){
        ConfigFunctionDef* func = ParseConfigFunction(parser,out);

        if(func){
          *configFunctions->PushElem() = *func;
        } else {
          printf("Error parsing user function\n");
        }
      } else {
        break;
      }
    }
  }
  
  parser->ExpectNext('}');

  ModuleDef def ={};

  def.name = name;
  def.params = params;
  def.inputs = vars;
  def.declarations = declarations;
  def.connections = PushArray(out,cons);
  def.configs = PushArray(out,configFunctions);
  
  return def;
}

TypeAndInstance ParseTypeAndInstance(Parser* parser){
  Token typeName = parser->ExpectNext(TokenType_IDENTIFIER);

  Token instanceName = {};
  if(parser->IfNextToken(':')){
    instanceName = parser->ExpectNext(TokenType_IDENTIFIER);
  }

  TypeAndInstance res = {};
  res.typeName = typeName;
  res.instanceName = instanceName;
  
  return res;
}

HierarchicalName ParseHierarchicalName(Parser* parser,Arena* out){
  Token topInstance = parser->ExpectNext(TokenType_IDENTIFIER);

  parser->ExpectNext('.');

  Var var = ParseVar(parser,out);

  HierarchicalName res = {};
  res.instanceName = topInstance;
  res.subInstance = var;

  return res;
}

MergeDef ParseMerge(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);
  
  parser->ExpectNext(TokenType_KEYWORD_MERGE);

  Array<Token> mergeModifiers = {};
  if(parser->IfNextToken('(')){
    auto tokenList = PushList<Token>(temp);
    
    while(!parser->Done()){
      Token peek = parser->PeekToken();
      
      if(peek.type == ')'){
        break;
      }

      *tokenList->PushElem() = parser->ExpectNext(TokenType_IDENTIFIER);

      parser->IfNextToken(',');
    }

    mergeModifiers = PushArray(out,tokenList);
    
    parser->ExpectNext(')');
  }

  Token mergeName = parser->ExpectNext(TokenType_IDENTIFIER);
  
  parser->ExpectNext('=');

  ArenaList<TypeAndInstance>* declarationList = PushList<TypeAndInstance>(temp);
  while(!parser->Done()){
    TypeAndInstance typeInst = ParseTypeAndInstance(parser);

    *declarationList->PushElem() = typeInst;

    Token peek = parser->PeekToken();
    if(peek.type == '|'){
      parser->NextToken();
      continue;
    } else if(peek.type == '{'){
      break;
    } else if(peek.type == ';'){
      parser->NextToken();
      break;
    }
  }
  Array<TypeAndInstance> declarations = PushArray(out,declarationList);

  Array<SpecNode> specNodes = {};
  if(parser->IfNextToken('{')){
    ArenaList<SpecNode>* specList = PushList<SpecNode>(temp);
    while(!parser->Done()){
      Token peek = parser->PeekToken();
      if(peek.type == '}'){
        break;
      }

      HierarchicalName leftSide = ParseHierarchicalName(parser,out);
      parser->ExpectNext('-');
      HierarchicalName rightSide = ParseHierarchicalName(parser,out);
      parser->ExpectNext(';');

      *specList->PushElem() = {leftSide,rightSide};
    }
    specNodes = PushArray(out,specList);

    parser->ExpectNext('}');
  }
  
  auto specificsNodes = PushList<SpecificMergeNode>(temp);
  for(SpecNode node : specNodes){
    // TODO: We do not do this in here. Parser only parses, semantic checks are performed later.
    int firstIndex = -1;
    int secondIndex = -1;
    for(int i = 0; i < declarations.size; i++){
      TypeAndInstance& decl = declarations[i];
      if(CompareString(node.first.instanceName.identifier,decl.instanceName.identifier)){
        firstIndex = i;
      } 
      if(CompareString(node.second.instanceName.identifier,decl.instanceName.identifier)){
        secondIndex = i;
      } 
    }

#if 0
    if(firstIndex == -1){
      Assert(false);
      // ReportError
    }
    if(secondIndex == -1){
      Assert(false);
      // ReportError
    }
#endif

    *specificsNodes->PushElem() = {firstIndex,node.first.subInstance.name.identifier,secondIndex,node.second.subInstance.name.identifier};
  }
  Array<SpecificMergeNode> specifics = PushArray(out,specificsNodes);

  MergeDef result = {};
  result.name = mergeName;
  result.declarations = declarations;
  result.specifics = specifics;
  result.mergeModifiers = mergeModifiers;
  
  return result;
}

#include "filesystem.hpp"

Array<ConstructDef> ParseVersatSpecification(String content,Arena* out){
  TEMP_REGION(temp,out);

  auto TokenizeFunction = [](void* tokenizerState) -> Token{
    DefaultTokenizerState* state = (DefaultTokenizerState*) tokenizerState;
    
    const char* start = state->ptr;
    const char* end = state->end;

    if(start >= end){
      Token token = {};
      token.type = TokenType_EOF;
      return token;
    }

    TokenizeResult res = ParseWhitespace(start,end);
    res |= ParseComments(start,end);

    res |= ParseMultiSymbol(start,end,">><",TokenType_ROTATE_RIGHT);
    res |= ParseMultiSymbol(start,end,"><<",TokenType_ROTATE_LEFT);

    res |= ParseMultiSymbol(start,end,"..",TokenType_DOUBLE_DOT);
    res |= ParseMultiSymbol(start,end,"##",TokenType_DOUBLE_HASHTAG);
    res |= ParseMultiSymbol(start,end,"->",TokenType_ARROW);
    res |= ParseMultiSymbol(start,end,">>",TokenType_SHIFT_RIGHT);
    res |= ParseMultiSymbol(start,end,"<<",TokenType_SHIFT_LEFT);
    res |= ParseMultiSymbol(start,end,"^=",TokenType_XOR_EQUAL);

    res |= ParseSymbols(start,end);
    res |= ParseNumber(start,end);

    res |= ParseIdentifier(start,end);

    if(res.token.type == TokenType_IDENTIFIER){
      String id = res.token.identifier;
      
      TokenType type = TokenType_INVALID;

      if(id == "module")     type = TokenType_KEYWORD_MODULE;
      if(id == "merge")      type = TokenType_KEYWORD_MERGE;
      if(id == "share")      type = TokenType_KEYWORD_SHARE;
      if(id == "static")     type = TokenType_KEYWORD_STATIC;
      if(id == "debug")      type = TokenType_KEYWORD_DEBUG;
      if(id == "config")     type = TokenType_KEYWORD_CONFIG;
      if(id == "state")      type = TokenType_KEYWORD_STATE;
      if(id == "mem")        type = TokenType_KEYWORD_MEM;
      if(id == "gen")        type = TokenType_KEYWORD_GEN;
      if(id == "for")        type = TokenType_KEYWORD_FOR;

      if(type != TokenType_INVALID){
        res.token.type = type;
      }
    }

    int size = res.bytesParsed;
    if(size <= 0 && state->ptr != state->end){
      size = 1;
    }

    state->ptr += size;

    // NOTE: Something very bad must happen to the point where the file is 1 byte after the end.
    //       We expect it to only reach file->end, not file->end + 1
    Assert(state->ptr < state->end + 1);

    Token ret = res.token;
    ret.originalFile = state->content;

    return ret;
  };

  FREE_ARENA(parseArena);
  Parser* parser = StartParsing(TokenizeFunction,content,parseArena,ParsingOptions_DEFAULT);

  // TODO:
  // nocheckin: Kinda hacky way of doing this.
  //            We cannot put filesystem stuff on the parser since parser is common.
  DefaultTokenizerState* state = (DefaultTokenizerState*) parser->tokenizerState;
  FileContent contentAsFile = FILE_GetFileContentFromString(content);
  state->content = contentAsFile;

  ArenaList<ConstructDef>* typeList = PushList<ConstructDef>(temp);

  while(!parser->Done()){
    Token tok = parser->PeekToken();

    ConstructDef def = {};
    if(tok.type == TokenType_KEYWORD_MODULE){
      def.type = ConstructType_MODULE;
      def.module = ParseModuleDef(parser,out);
    } else if(tok.type == TokenType_KEYWORD_MERGE){
      def.type = ConstructType_MERGE;
      def.merge = ParseMerge(parser,out);
    } else {
      parser->ReportUnexpectedToken(tok,{TokenType_KEYWORD_MODULE,TokenType_KEYWORD_MERGE});
      parser->Synch({TokenType_KEYWORD_MODULE,TokenType_KEYWORD_MERGE});
    }

    *typeList->PushElem() = def;
  }

  if(!Empty(parser->errors)){
    for(String str : parser->errors){
      printf("%.*s\n",UN(str));
    }
    exit(-1);
  }

  Array<ConstructDef> defs = PushArray(out,typeList);

  return defs;
}

static ConfigIdentifier* ParseConfigIdentifier(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);
  
  Token id = parser->ExpectNext(TokenType_IDENTIFIER);
  
  ConfigIdentifier* base = PushStruct<ConfigIdentifier>(out);
  base->type = ConfigIdentifierType_BASE;
  base->name = id;

  ConfigIdentifier* ptr = base;

  while(!parser->Done()){
    ConfigIdentifier* parsed = nullptr;

    if(!parsed && parser->IfNextToken('.')){
      Token access = parser->ExpectNext(TokenType_IDENTIFIER);

      // Function call syntax
      if(parser->IfNextToken('(')){
        auto args = PushList<MathExpression*>(temp);       
      
        while(!parser->Done()){
          MathExpression* arg = ParseMathExpression(parser,out);

          *args->PushElem() = arg;
        
          if(parser->IfNextToken(',')){
            continue;
          }
        
          break;
        }

        parser->ExpectNext(')');

        parsed = PushStruct<ConfigIdentifier>(out);
        parsed->type = ConfigIdentifierType_FUNC_CALL;
        parsed->functionName = access;
        parsed->arguments = PushArray(out,args);
      } else {
        parsed = PushStruct<ConfigIdentifier>(out);
        parsed->type = ConfigIdentifierType_ACCESS;
        parsed->name = access;
      }
    }

    if(!parsed && parser->IfNextToken('[')){
      MathExpression* expr = ParseMathExpression(parser,out);

      parser->ExpectNext(']');

      parsed = PushStruct<ConfigIdentifier>(out);
      parsed->type = ConfigIdentifierType_ARRAY;

      parsed->arrayExpr = expr;
    }

    if(parsed){
      ptr->next = parsed;
      ptr = parsed;
      continue;
    }

    break;
  }

  return base;
}

static ConfigStatement* ParseConfigStatement(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);
  
  ConfigStatement* stmt = PushStruct<ConfigStatement>(out);

  if(parser->IfNextToken(TokenType_KEYWORD_GEN)){
    Token loopVariable = parser->ExpectNext(TokenType_IDENTIFIER);
 
    MathExpression* start = ParseMathExpression(parser,out);
    parser->ExpectNext(TokenType_DOUBLE_DOT);
    MathExpression* end = ParseMathExpression(parser,out);

    parser->ExpectNext('{');

    auto list = PushList<ConfigStatement*>(temp);
    while(!parser->Done()){
      ConfigStatement* child = ParseConfigStatement(parser,out);
      *list->PushElem() = child;

      if(parser->IfPeekToken('}')){
        break;
      }
    }

    parser->ExpectNext('}');
    
    stmt->def.loopVariable = loopVariable;
    stmt->def.startSym = start;
    stmt->def.endSym = end;
    stmt->childs = PushArray(out,list);
    stmt->type = ConfigStatementType_GEN_LOOP;
  } else if(parser->IfNextToken(TokenType_KEYWORD_FOR)){
    Token loopVariable = parser->ExpectNext(TokenType_IDENTIFIER);
 
    MathExpression* start = ParseMathExpression(parser,out);
    parser->ExpectNext(TokenType_DOUBLE_DOT);
    MathExpression* end = ParseMathExpression(parser,out);

    parser->ExpectNext('{');

    auto list = PushList<ConfigStatement*>(temp);
    while(!parser->Done()){
      ConfigStatement* child = ParseConfigStatement(parser,out);
      *list->PushElem() = child;

      if(parser->IfPeekToken('}')){
        break;
      }
    }

    parser->ExpectNext('}');
    
    stmt->def.loopVariable = loopVariable;
    stmt->def.startSym = start;
    stmt->def.endSym = end;
    stmt->childs = PushArray(out,list);
    stmt->type = ConfigStatementType_FOR_LOOP;
  } else if(parser->IfPeekToken(TokenType_IDENTIFIER)) {
    stmt->lhs = ParseConfigIdentifier(parser,out);

    if(parser->IfNextToken('=')){
      stmt->rhs = ParseMathExpression(parser,out);
      parser->ExpectNext(';');
      stmt->type = ConfigStatementType_EQUALITY;
    } else {
      parser->ExpectNext(';');
      stmt->type = ConfigStatementType_FUNCTION_CALL;
    }
  } else {
    parser->ReportUnexpectedToken(parser->NextToken(),{});
  }

  return stmt;
}

static ConfigVarDeclaration ParseConfigVarDeclaration(Parser* parser,Arena* out){
  ConfigVarDeclaration res = {};

  res.name = parser->ExpectNext(TokenType_IDENTIFIER);

  if(parser->IfNextToken('[')){
    MathExpression* arraySize = ParseMathExpression(parser,out);
    parser->ExpectNext(']');
    res.arraySize = arraySize;
  }

  Token type = {};
  if(parser->IfNextToken(':')){
    type = parser->NextToken();
  }

  res.type = type;
  
  return res;
}

static Array<ConfigVarDeclaration> ParseConfigFunctionArguments(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);

  auto list = PushList<ConfigVarDeclaration>(temp);

  parser->ExpectNext('(');
  
  while(!parser->Done()){
    if(parser->IfPeekToken(')')){
      break;
    }

    ConfigVarDeclaration var = ParseConfigVarDeclaration(parser,out);

    *list->PushElem() = var;
    
    if(parser->IfNextToken(',')){
      continue;
    } else {
      break;
    }
  }

  parser->ExpectNext(')');

  return PushArray(out,list);
}

ConfigFunctionDef* ParseConfigFunction(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);

  UserConfigType type = UserConfigType_NONE;
  if(parser->IfNextToken(TokenType_KEYWORD_CONFIG)){
    type = UserConfigType_CONFIG;
  } else if(parser->IfNextToken(TokenType_KEYWORD_MEM)){
    type = UserConfigType_MEM;
  } else if(parser->IfNextToken(TokenType_KEYWORD_STATE)){
    type = UserConfigType_STATE;
  }

  if(type == UserConfigType_NONE){
    return nullptr;
  }

  bool debug = parser->IfNextToken(TokenType_KEYWORD_DEBUG);
    
  Token configName = parser->ExpectNext(TokenType_IDENTIFIER);

  Array<ConfigVarDeclaration> functionVars = {};
  if(type == UserConfigType_MEM || type == UserConfigType_CONFIG){
    functionVars = ParseConfigFunctionArguments(parser,out);
  }

  parser->ExpectNext('{');

  auto stmts = PushList<ConfigStatement*>(temp);
  while(!parser->Done()){
    Token peek = parser->PeekToken();

    if(peek.type == '}'){
      break;
    }
        
    ConfigStatement* config = ParseConfigStatement(parser,out);
    *stmts->PushElem() = config;
  }

  parser->ExpectNext('}');

  ConfigFunctionDef* res = PushStruct<ConfigFunctionDef>(out);
  res->type = type;
  res->name = configName;
  res->variables = functionVars;
  res->statements = PushArray(out,stmts);
  res->debug = debug;

  return res;
}

Array<Token> AccumTokens(MathExpression* top,Arena* out){
  TEMP_REGION(temp,out);

  auto AccumTokens = [](auto AccumTokens,MathExpression* top,ArenaList<Token>* list) -> void {
    switch(top->type){
    case MathType_LITERAL: break;
    case MathType_NAME: {
      *list->PushElem() = top->name;
    } break;
    case MathType_OPERATION: {
    } break;
    case MathType_SINGLE_ACCESS: {
      *list->PushElem() = top->name;
    } break;
    case MathType_ARRAY_ACCESS: {
      *list->PushElem() = top->name;
    } break;
    case MathType_FUNCTION_CALL: {
      *list->PushElem() = top->name;
    } break;
    }
    for(MathExpression* child : top->expressions){
      AccumTokens(AccumTokens,child,list);
    }
  };

  auto list = PushList<Token>(temp);
  AccumTokens(AccumTokens,top,list);
  return PushArray(out,list);
}

void ArrayIndexIncrementInPlace(Array<int> dims,Array<int> startValue,Array<int> index){
  Assert(dims.size == index.size);
  int size = dims.size;

  for(int i = size - 1; i >= 0; i--){
    index[i] += 1;

    if(i != 0 && index[i] >= dims[i]){
      index[i] = startValue[i];
      continue;
    }

    break;
  }
}

int ArrayIndexToInteger(Array<int> dims,Array<int> index){
  Assert(dims.size == index.size);
  int size = dims.size;

  int res = 0;
  for(int i = 0; i < size; i++){
    int val = index[i];
    for(int j = i + 1; j < size; j++){
      val *= dims[j];
    }

    res += val;
  }

  return res;
}

Array<int> IntegerToArrayIndex(Array<int> dims,int index,Arena* out){
  int size = dims.size;

  Array<int> res = PushArray<int>(out,size);

  int value = index;
  for(int i = 0; i < index; i++){
    int dimTotalSize = 1;
    for(int j = i + 1; j < size; j++){
      dimTotalSize *= dims[j];
    }

    res[i] = value / dimTotalSize;
    value = value % dimTotalSize;
  }

  return res;
}

DimIterator* StartIteration(Array<int> dimensions,Array<int> startValues,Arena* out){
  Assert(dimensions.size > 0);

  DimIterator* res = PushStruct<DimIterator>(out);

  res->dim = CopyArray(dimensions,out);
  res->startValue = CopyArray(startValues,out);
  res->current = CopyArray(startValues,out);

  return res;
}

DimIterator* StartIteration(int size,Arena* out){
  DimIterator* res = PushStruct<DimIterator>(out);
  
  res->dim = PushArray<int>(out,1);
  res->startValue = PushArray<int>(out,1);
  res->current = PushArray<int>(out,1);

  res->dim[0] = size;

  return res;
}

int DimIterator::Size(){
  int size = 1;

  for(int i = 0; i < dim.size; i++){
    size *= MAX(1,dim[i] - startValue[i]);
  }

  return size;
}

void DimIterator::Invalidate(){
  current[0] = dim[0];
}

void DimIterator::Advance(){
  ArrayIndexIncrementInPlace(dim,startValue,current);
}

bool DimIterator::IsValid(){
  if(current[0] >= dim[0]){
    return false;
  }
 
  return true;
}

Array<int> DimIterator::Current(){
  return current;
}

VarIterator* StartIteration(Env* env,Var var,Arena* out){
  TEMP_REGION(temp,out);

  VarIterator* res = PushStruct<VarIterator>(out);

  Entity ent = env->GetEntity(var.name);

  int expectedIndexSize = 0;
  if(ent.type == EntityType_FU_ARRAY){
    expectedIndexSize = ent.dims.size;
  }

  res->name = var.name;

  res->startDelay = env->CalculateConstantExpression(var.extra.delay.start);
  res->startPort  = env->CalculateConstantExpression(var.extra.port.start);

  res->endDelay = env->CalculateConstantExpression(var.extra.delay.end) + 1;
  res->endPort  = env->CalculateConstantExpression(var.extra.port.end) + 1;

  res->currentDelay = res->startDelay;
  res->currentPort = res->startPort;

  if(var.index.size){
    auto start = env->ConvertRangeToStart(var.index,temp);
    auto end = env->ConvertRangeToEnd(var.index,temp);

    for(int& i : end){
      i += 1;
    }

    res->arrayIndex = StartIteration(end,start,out);
  } else {
    res->arrayIndex = StartIteration(1,out);
  }

  if(ent.type != EntityType_FU_ARRAY && var.index.size){
    env->ReportError({},"Error, var is not an array and does not support array subscriptions");
    res->Invalidate();
    return res;
  }

  if(var.index.size != expectedIndexSize){
    env->ReportError({},"Error, var has more or less accesses than needed");
    res->Invalidate();
    return res;
  }

  // TODO: We can put more error checking in here.
  //       If ports are bigger than the declaration allows.
  //       If the iteration dims are bigger than the declaration.
  //       And so on.

  return res;
}

int VarIterator::Size(){
  int size = 1;

  size *= MAX(1,(endDelay - startDelay));
  size *= MAX(1,(endPort - startPort));
  size *= arrayIndex->Size();
  
  return size;
}

void VarIterator::Invalidate(){
  arrayIndex->Invalidate();
}

void VarIterator::Advance(){
  currentPort += 1;

  if(currentPort >= endPort){
    currentPort = startPort;
  } else {
    return;
  }

  currentDelay += 1;

  if(currentDelay >= endDelay){
    currentDelay = startDelay;
  } else {
    return;
  }

  arrayIndex->Advance();
}

bool VarIterator::IsValid(){
  return arrayIndex->IsValid();
}

Connection VarIterator::Current(){
  Connection con = {};

  con.name = name;
  con.port = currentPort;
  con.delay = currentDelay;
  con.arrayIndex = arrayIndex->Current();

  return con;
}

GroupIterator IterateGroup(Env* env,VarGroup* group,Arena* out){
  GroupIterator iter = {};
  iter.env = env;
  
  int size = group->vars.size;

  iter.group = group;
  iter.innerIters = PushArray<VarIterator*>(out,size);
  for(int i = 0; i < size; i++){
    iter.innerIters[i] = StartIteration(env,group->vars[i],out);
  }

  for(int i = 0; i < size; i++){
    if(!iter.innerIters[i]->IsValid()){
      iter.Invalidate();
    }
  }

  return iter;
}

int GroupIterator::Size(){
  int size = 0;

  for(VarIterator* iter : innerIters){
    size += iter->Size();
  }

  return size;
}

void GroupIterator::Invalidate(){
  currentIter = innerIters.size;
}

bool GroupIterator::IsValid(){
  if(currentIter < innerIters.size){
    return true;
  }

  return false;
}

void GroupIterator::Advance(){
  Assert(IsValid());

  innerIters[currentIter]->Advance();
  
  if(!innerIters[currentIter]->IsValid()){
    currentIter += 1;
  }
}

Connection GroupIterator::Current(){
  TEMP_REGION(temp,nullptr);

  Assert(IsValid());

  Connection con = innerIters[currentIter]->Current();
  return con;
}

Entity MakeEntity(FUInstance* inst){
  Entity res = Entity_Nil;
  res.type = EntityType_FU;
  res.inst = inst;
  res.decl = inst->declaration;

  // TODO-2
  res.name = {};
  res.name.type = TokenType_IDENTIFIER;
  res.name.identifier = inst->name;
  res.name.originalData = inst->name;

  return res;
}

bool Nil(Entity ent){
  bool res = (ent.type == EntityType_NIL);
  return res;
}
