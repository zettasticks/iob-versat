#include "compiler.hpp"

#include "declaration.hpp"
#include "debugVersat.hpp"

// ======================================
// Constants

readonly COM_Unit COM_Unit_Nil = {.decl = &FUDeclaration_Nil};
readonly COM_Port COM_Port_Nil = {.unit = &COM_Unit_Nil};
readonly COM_Ent COM_Ent_Nil = {.unit = &COM_Unit_Nil};
COM_EntPort COM_EntPort_Nil = {.ent = COM_Ent_Nil};
COM_ConnectInfo COM_ConnectInfo_Nil = {.ent = COM_Ent_Nil};

// ======================================
// Type stuff

bool IsNil(COM_Ent ent){
  bool res = (ent.type == COM_EntType_NIL);
  return res;
}

bool IsNil(COM_ConnectInfo* con){
  bool res = (con == nullptr || con == &COM_ConnectInfo_Nil || IsNil(con->ent));
  return res;
}

bool COM_Ent_IsVar(COM_EntType in){
  bool res = (in == COM_EntType_ARG_DYN || 
              in == COM_EntType_ARG_NONE || 
              in == COM_EntType_ARG_FIXED || 
              in == COM_EntType_ARG_BUFFER);
  return res;
}


bool COM_Ent_IsArray(COM_EntType in){
  bool res = (in == COM_EntType_MODULE_UNIT_ARRAY);
  return res;
}

bool COM_Ent_IsExpr(COM_EntType in){
  bool res = (in == COM_EntType_MODULE_INPUT ||
              in == COM_EntType_PARAM ||
              in == COM_EntType_ARG_NONE ||
              in == COM_EntType_ARG_FIXED ||
              in == COM_EntType_ARG_DYN ||
              in == COM_EntType_ARG_BUFFER);

  return res;
}

bool COM_Ent_IsWire(COM_EntType in){
  bool res = (in == COM_EntType_VAR_WITH_STATE ||
              in == COM_EntType_VAR_WITH_CONFIG ||
              in == COM_EntType_VAR_WITH_VIRTUAL_MEM);
  return res;
}

COM_CopyResult COM_Copy(COM_Graph in,bool computeMapping,Arena* out){
  COM_UnitList units = {};
  COM_EdgeList edges = {};

  for(COM_Unit* unit = in.units.head; unit; unit = unit->next){
    COM_Unit* copied = PushStruct<COM_Unit>(out);
    *copied = *unit;
    copied->name = PushString(out,unit->name);

    copied->next = copied->prev = nullptr;

    units.count += 1;
    LL_Append(units.head,units.tail,next,copied);

    unit->general = copied;
  }

  for(COM_Edge* edge = in.edges.head; edge; edge = edge->next){
    COM_Unit* outUnit = (COM_Unit*) edge->out->general;
    COM_Unit* inUnit = (COM_Unit*) edge->in->general;
    
    COM_Edge* newEdge = PushStruct<COM_Edge>(out);
    *newEdge = *edge;
    newEdge->next = 0;
    newEdge->out = outUnit;
    newEdge->in = inUnit;

    edges.count += 1;
    LL_Append(edges.head,edges.tail,next,newEdge);

    edge->general = newEdge;
  }

  // Calculate mapping ==========================================================
  COM_UnitMap* unitMapping = 0;
  COM_EdgeMap* edgeMapping = 0;

  if(computeMapping){
    COM_UnitMap* unitTail = 0;
    COM_EdgeMap* edgeTail = 0;
    
    for(COM_Unit* unit = in.units.head; unit; unit = unit->next){
      COM_Unit* mapped = (COM_Unit*) unit->general;
      
      COM_UnitMap* map = PushStruct<COM_UnitMap>(out);
      map->unit0 = unit;
      map->unit1 = mapped;
      LL_Append(unitMapping,unitTail,next,map);
    }

    for(COM_Edge* edge = in.edges.head; edge; edge = edge->next){
      COM_Edge* mapped = (COM_Edge*) edge->general;
      
      COM_EdgeMap* map = PushStruct<COM_EdgeMap>(out);
      map->edge0 = edge;
      map->edge1 = mapped;
      LL_Append(edgeMapping,edgeTail,next,map);
    }
  }

  COM_CopyResult res = {};
  res.result.edges = edges;
  res.result.units = units;
  res.unitMapping = unitMapping;
  res.edgeMapping = edgeMapping;

  COM_ClearGeneralPtr(in.units.head);
  COM_ClearGeneralPtr(in.edges.head);

  return res;
}

// ======================================
// Constant expressions and computations

SYM_Expr COM_SymbolicFromExpression(COM_Env* env,SP_Node* node){
  TEMP_REGION(temp,nullptr);

  auto Recurse = [temp,env](auto Recurse,SP_Node* top) -> SYM_Expr{
    SYM_Expr res = SYM_Nil;
    
    int opCount = SP_Type_OpCount(top->type);
    
    if(top->type == SP_Type_RANGE){
      opCount = 2;
    }

    SYM_Expr left = {};
    SYM_Expr right = {};

    if(opCount >= 1){
      left = Recurse(Recurse,top->first);
    }
    if(opCount >= 2){
      if(top->second){
        right = Recurse(Recurse,top->second);
      } else {
        opCount = 1;
      }
    }
    Assert(opCount <= 2);

    bool handled = 1;
    switch(top->type){
    case SP_Type_ADD:{
      res = left + right;
    } break;
    case SP_Type_SUB:{
      if(opCount == 1){
        res = -right;
      } else {
        res = left - right;
      }
    } break;
    case SP_Type_MUL:{
      res = left * right;
    } break;
    case SP_Type_DIV:{
      res = left / right;
    } break;
      
    case SP_Type_VAR:{
      COM_Ent ent = COM_GetEnt(env,top->token,true);

      int val = ent.currentValue;
      bool found = 1;
      switch(ent.type){
      case COM_EntType_PARAM:{
        // Nothing
      } break;
      default: found = 0;
      }

      if(!found){
        res = SYM_Var(top->token.val);
      } else {
        res = SYM_Lit(val);
      }

#if 0
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
#endif

    } break;

    case SP_Type_RANGE:{
      // NOTE: Handling range in here is kinda sketchy since we shouldnt produce a range
      //       in the first place. Simplifies parser but
      if(!SYM_Equal(left,right)){
        Assert(false && "Range cannot appear in expressions except if first equals second");
      } else {
        res = left;
      }
    } break;

    case SP_Type_LITERAL:{
      res = SYM_Lit(ParseInt(top->token.val));
    } break;

#if 0
    case SP_Node_ARRAY_ACCESS: {
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

    case SP_Node_FUNCTION_CALL: {
      Array<MathExpression*> args = top->expressions;

      if(top->name.identifier == "Duty"){
        res = SYM_Duty(Recurse(Recurse,top->expressions[0]),Recurse(Recurse,top->expressions[1]));
      } else {
        Array<SYM_Expr> expressions = PushArray<SYM_Expr>(temp,args.size);
        for(int i = 0; i <  args.size; i++){
          expressions[i] = SymbolicFromMathExpression(args[i]);
        }

        Entity funcEnt = AddComputation(top->name.identifier,expressions);
        res = SYM_Var(funcEnt.name.identifier);
      }
    } break;

    case SP_Node_ACCESS:  Assert(false);
#endif

    default: handled = 0;
    }

    Assert(handled);

    return res;
  };

  SP_Node* expr = SP_UnpackExpr(node);
  SYM_Expr res = Recurse(Recurse,expr);

  return res;
}

COM_ConstantResult COM_ComputeConstantValue(COM_Env* env,SP_Node* node){
  SYM_Expr asSym = COM_SymbolicFromExpression(env,node);
  SYM_EvaluateResult eval = SYM_ConstantEvaluate(asSym);

  COM_ConstantResult res = {};
  res.value = eval.result;
  res.nonConstant = eval.nonConstantValue;
  res.divByZero = eval.divByZero;

  res.anyError = (res.nonConstant || res.divByZero);
  return res;
}

// ======================================
// Compilation helpers

COM_ConnectInfoList COM_UnpackVarGroup(COM_Env* env,SP_Node* top,Arena* out){
  COM_Ent var = COM_Ent_Nil;

  int size = 0;
  COM_ConnectInfo* head = 0;
  COM_ConnectInfo* tail = 0;
    
  for(SP_Node* ptr = top->childs; ptr; ptr = ptr->next){
    COM_ConnectInfo* storedTail = tail;

    if(ptr->type == SP_Type_VAR){
      var = COM_GetEnt(env,ptr->token,false);
    } else {
      Assert(ptr->childs->type == SP_Type_VAR);
      
      var = COM_GetEnt(env,ptr->childs->token,false);
    }

    switch(ptr->type){
    case SP_Type_VAR:{
      head = tail = PushStruct<COM_ConnectInfo>(out);
      head->ent = var;
      size += 1;
    } break;
    case SP_Type_PORT_ACCESS:
    case SP_Type_DELAY_ACCESS:
    case SP_Type_RANGE_ACCESS:{
      SP_Node* expr = ptr->childs->next;
      COM_RangeValues range = COM_CalculateRange(env,expr);
      
      for(int i = range.low; i < range.high + 1; i++){
        COM_ConnectInfo* con = PushStruct<COM_ConnectInfo>(out);

        con->ent = var;
        
        switch(ptr->type){
        case SP_Type_PORT_ACCESS: con->port = i; break;
        case SP_Type_DELAY_ACCESS: con->delay = i; break;
        case SP_Type_RANGE_ACCESS: con->ent = COM_ArrayAccess(env,var,i); break;
        }

        size += 1;
        LL_Append(head,tail,next,con);
      }
    } break;
    };
      
    // NOTE: We must always add a new connection otherwise something wrong ========
    if(storedTail == tail){
      COM_ReportError(env,"Problem computing access");
    }
  }

  if(!head){
    COM_ReportError(env,"Problem computing access");
  }

  COM_ConnectInfoList res = {};
  res.head = head;
  res.tail = tail;
  res.size = size;
  return res;
};

COM_RangeValues COM_CalculateRange(COM_Env* env,SP_Node* node){
  SP_Node* range = SP_UnpackExpr(node);
  Assert(range->type == SP_Type_RANGE);

  COM_ConstantResult lowVal = COM_ComputeConstantValue(env,range->first);
  COM_ConstantResult highVal = COM_ComputeConstantValue(env,range->second);

  if(lowVal.divByZero || highVal.divByZero){
    COM_ReportError(env,"Div by zero detected",range);
  }

  if(lowVal.nonConstant || highVal.nonConstant){
    COM_ReportError(env,"Range must be a constant expression",range);
  }

  COM_RangeValues res = {};
  res.low = lowVal.value;
  res.high = highVal.value;
  res.error = lowVal.divByZero || highVal.divByZero;
  res.constant = !(lowVal.nonConstant || highVal.nonConstant);
  return res;
}

COM_Ent COM_ResolveEntity(COM_Env* env,SP_Node* varAccessNode,bool canFail){
  SP_Node* node = varAccessNode;
  bool isVarStuff = SP_Type_IsVarAccess(node->type);
  Assert(isVarStuff && "Can only call this function with valid access expressions");

  COM_Ent res = COM_Ent_Nil;

  bool handled = 1;
  switch(node->type){
  case SP_Type_VAR:{
    res = COM_GetEnt(env,node->token,canFail);
  } break;
  case SP_Type_HIER_ACCESS:{
    COM_Ent varSide = COM_ResolveEntity(env,node->childs,false);

    Token access = node->token;

    if(varSide.type != COM_EntType_MODULE_UNIT){
      COM_ReportError(env,"Cannot access a non unit entity",node);
    } else {
      FUDeclaration* decl = varSide.unit->decl;
    
      String name = access.val;

      bool found = 0;
      COM_EntType type = {};
      String wireName = {};

      Direction dir = Direction_NONE;
      int port = 0;
      if(name == "out0"){ dir = Direction_OUTPUT; port = 0; }
      if(name == "out1"){ dir = Direction_OUTPUT; port = 1; }
      if(name == "in0") { dir = Direction_INPUT;  port = 0; }
      if(name == "in1") { dir = Direction_INPUT;  port = 1; }

      if(dir != Direction_NONE){
        type = COM_EntType_VAR_WITH_VIRTUAL_MEM;
        found = 1;
      }
      
      if(!found){
        for(Wire w : decl->configs){
          if(w.name == name){
            type = COM_EntType_VAR_WITH_CONFIG;
            wireName = name;
            found = true;
            break;
          }
        }
      }

      if(!found){
        for(Wire w : decl->states){
          if(w.name == name){
            type = COM_EntType_VAR_WITH_STATE;
            wireName = name;
            found = true;
            break;
          }
        }
      }

      if(!found){
        COM_ReportError(env,"Entity does not contain wire",access);
      }
      
      res.type = type;
      res.wireName = wireName;
      res.dir = dir;
      res.port = port;
    }
  } break;
  case SP_Type_RANGE_ACCESS:{
    COM_Ent varSide = COM_ResolveEntity(env,node->childs,false);
    SP_Node* accessExpr = node->childs->next;
    
    COM_ConstantResult val = COM_ComputeConstantValue(env,accessExpr);

    if(!val.anyError){
      bool constant = !val.nonConstant;

      if(constant){
        if(COM_Ent_IsArray(varSide.type)){
          NOT_IMPLEMENTED("TODO: Array stuff");
        } else {
          res = varSide;
          res.type = COM_EntType_VAR_WITH_LEFTOVER_RANGE;
          res.node = accessExpr;
        }
      }

      if(!constant){
        if(COM_Ent_IsArray(varSide.type)){
          COM_ReportError(env,"Cannot have a non constant expression inside an array access",accessExpr);
        } else {
          res = varSide;
          res.type = COM_EntType_VAR_WITH_LEFTOVER_RANGE;
          res.node = accessExpr;
        }
      }
    }
  } break;
  default: handled = 0;
  }
  Assert(handled);
  
  return res;
}

COM_EntPort COM_InstantiateExpression(COM_Env* env,SP_Node* top,Arena* out){
  bool isExpr = SP_Type_IsExpr(top->type);

  // MARK
  COM_Ent ent = COM_Ent_Nil;
  int port = 0;
  int delay = 0;

  bool handled = 1;
  if(isExpr){
    int opCount = SP_Type_OpCount(top->type);
    String typeName = SP_Type_Name(top->type);

    COM_EntPort lhs = COM_EntPort_Nil;
    COM_EntPort rhs = COM_EntPort_Nil;

    if(opCount >= 1){
      lhs = COM_InstantiateExpression(env,top->first,out);
    }
    if(opCount >= 2){
      rhs = COM_InstantiateExpression(env,top->second,out);
    }

    FUDeclaration* decl = DECL_GetType(typeName,{});
    if(IsNil(decl)){
      COM_ReportError(env,SF("Did not find declaration '%.*s'",UN(typeName)));
    }
    
    COM_Unit* unit = COM_PushUnit(&env->graph,SF("%.*s_%d",UN(typeName),env->tempIndex++),decl,env->outArena);

    COM_Ent* entPtr = COM_PushEnt(env,unit->name,COM_EntType_GENERATED_UNIT);
    entPtr->unit = unit;
    ent = *entPtr;

    COM_Connect(env,lhs.ent,lhs.port,ent,0,lhs.delay);
    COM_Connect(env,rhs.ent,rhs.port,ent,1,rhs.delay);
  } else {
    switch(top->type){
    case SP_Type_FUNCTION_CALL:{
      NOT_IMPLEMENTED("TODO");
    } break;
    case SP_Type_LITERAL:{
      NOT_IMPLEMENTED("TODO");
    } break;
    case SP_Type_VAR:{
      ent = COM_GetEnt(env,top->token,false);
    } break;
    case SP_Type_DELAY_ACCESS:{
      COM_EntPort entPort = COM_InstantiateExpression(env,top->first,out);
      SP_Node* expr = top->second;

      COM_ConstantResult delayConst = COM_ComputeConstantValue(env,expr);

      if(delayConst.anyError){
        COM_ReportError(env,"Error evaluating delay value, needs to be constant",expr);
      } else {
        ent = entPort.ent;
        port = entPort.port;
        delay = delayConst.value;
      }
    } break;
    case SP_Type_PORT_ACCESS:{
      SP_Node* var = top->childs;
      Assert(var->type == SP_Type_VAR);

      SP_Node* expr = top->childs->next;
      
      COM_RangeValues range = COM_CalculateRange(env,expr);

      if(!range.error && range.low != range.high){
        COM_ReportError(env,"Cannot have range expressions inside expressions",expr);
      }
      
      ent = COM_GetEnt(env,var->token,false);
      port = range.low;
    } break;

    default: handled = 0;
    }
  }

  Assert(handled);

  COM_EntPort res = {};
  res.ent = ent;
  res.port = port;
  res.delay = delay;

  return res;
};

COM_UnpackedExpr COM_UnpackExpr(COM_Env* env,SP_Node* node){
  COM_ExprType type = {};
  COM_Ent ent = {};
  SP_Node* expr = 0;
  String name = {};

  if(node){
    bool isExpr = SP_Type_IsExpr(node->type);
    bool isVarStuff = SP_Type_IsVarAccess(node->type);
          
    bool found = 0;
    if(!found && isVarStuff){
      ent = COM_ResolveEntity(env,node,true);
      bool isWire = COM_Ent_IsWire(ent.type);
      bool isExpr = COM_Ent_IsExpr(ent.type);
      
      if(!found && ent.type == COM_EntType_VAR_WITH_LEFTOVER_RANGE){
        type = COM_ExprType_ARRAY_ACCESS;
        expr = ent.node;
        found = 1;
      }

      if(!found && isWire){
        type = COM_ExprType_WIRE;
        name = ent.wireName;
        found = 1;
      }

      if(!found && isExpr){
        type = COM_ExprType_EXPR;
        expr = ent.node;
        found = 1;
      }

      if(!found && ent.type == COM_EntType_NIL && node->type == SP_Type_VAR){
        type = COM_ExprType_NAME;
        name = node->token.val;
        found = 1;
      }
    }

    if(!found && node->type == SP_Type_FUNCTION_CALL){
      NOT_IMPLEMENTED("TODO");
      type = COM_ExprType_FUNC_CALL;
      found = 1;
    }
          
    if(isExpr){
      expr = node;
      type = COM_ExprType_EXPR;
      found = 1;
    }
          
    // Missing some handling.
    Assert(found && "Missing some form of handling");
  }

  COM_UnpackedExpr res = {};
  res.type = type;
  res.expr = expr;
  res.ent = ent;
  res.name = name;
  
  return res;
}

// ======================================
// Compilation

COM_Module COM_InstantiateModule(SP_Node* moduleDef,Array<COM_Param> topLevelParams,Arena* out){
  Assert(moduleDef->type = SP_Type_MODULE_DECL);

  String moduleName = moduleDef->token.val;

  DEBUG_TOP(moduleName);

  FREE_ARENA(envArena);
  FREE_ARENA(envErrorArena);
  COM_Env envInst = {};
  COM_Env* env = &envInst;
  env->outArena = out;
  env->arena = envArena;
  env->errorArena = envErrorArena;

  COM_FunctionNode* funcHead = 0;
  COM_FunctionNode* funcTail = 0;

  COM_Unit* outputUnit = 0;

  int inputIndex = 0;
  for(SP_Node* ptr = moduleDef->childs; ptr; ptr = ptr->next){
    bool handled = 1;
    switch(ptr->type){
    case SP_Type_INPUT_DECL:{
      Token name = ptr->token;

      COM_Unit* in = COM_PushUnit(&env->graph,name.val,BasicDeclaration::input,env->outArena);
      in->special = inputIndex++;

      COM_Ent* ent = COM_PushEnt(env,name,COM_EntType_MODULE_INPUT);
      ent->unit = in;
    } break;
    case SP_Type_PARAM_DECL:{
      Token paramName = ptr->token;
      SP_Node* expr = ptr->childs;

      COM_Ent* var = COM_PushEnt(env,paramName,COM_EntType_PARAM);
      COM_ConstantResult valuation = COM_ComputeConstantValue(env,expr);

      if(valuation.anyError){
        COM_ReportError(env,"Error evaluation expression, needs to be constant but it is not",expr);
      }

      // TODO: Override value with topLevelParams
            
      var->currentValue = valuation.value;
    } break;
      
    case SP_Type_VARIABLE_DECL:{
      TEMP_REGION(temp,out);

      String typeName = ptr->token.val;

      ArenaList<DECL_Param>* paramList = PushList<DECL_Param>(temp);
      Array<DECL_Param> params = {};
      FUDeclaration* decl = nullptr;

      for(SP_Node* child = ptr->childs; child; child = child->next){
        bool modDebug = 0;
        bool modStatic = 0;
        bool modShare = 0;
        int shareIndex = 0;
        
        bool handled = 1;
        switch(child->type){
          case SP_Type_MODIFIER_DEBUG:{
            modDebug = 1;
          } break;
          case SP_Type_MODIFIER_STATIC:{
            modStatic = 1;
          } break;
          case SP_Type_MODIFIER_SHARE:{
            modShare = 1;
            shareIndex = env->shareIndex++; 
          } break;
          case SP_Type_PARAM:{
            Token paramName = child->token;
            SP_Node* expr = child->childs;

            COM_ConstantResult valuation = COM_ComputeConstantValue(env,expr);

            if(valuation.anyError){
              COM_ReportError(env,"Error evaluation expression, needs to be constant but it is not",expr);
            }

            DECL_Param* val = paramList->PushElem();
            val->name = paramName.val;
            val->val = SYM_Lit(valuation.value);
          } break;
          case SP_Type_VAR_DECL:{
            Token name = child->token;

            if(params.size == 0 && !Empty(paramList)){
              params = PushArray(temp,paramList);
            }

            if(!decl){
              decl = DECL_GetType(typeName,params);
            }

            COM_Unit* unit = COM_PushUnit(&env->graph,name.val,decl,env->outArena);
            unit->debug = modDebug;
            unit->isShared = modShare;
            unit->isStatic = modStatic;
            unit->sharedIndex = shareIndex;
            
            COM_Ent* var = COM_PushEnt(env,name,COM_EntType_MODULE_UNIT);
            var->unit = unit;
          } break;

          default: handled = 0; break;
        }
        
        Assert(handled);
      }
    } break;
    case SP_Type_CONNECTION:{
      TEMP_REGION(temp,out);
      
      SP_Node* lhsVarGroup = ptr->childs;
      SP_Node* rhsVarGroup = lhsVarGroup->next;

      // Add output unit if found on rhs side. Lhs is always a error so no need to check =
      if(!outputUnit){
        SP_NodeNode* flatten = SP_Flatten(rhsVarGroup,temp);
        
        for(SP_NodeNode* ptr = flatten; ptr; ptr = ptr->next){
          SP_Node* node = ptr->node;

          if(node->type == SP_Type_VAR && node->token.val == "out"){
            String name = "out";

            COM_Unit* unit = COM_PushUnit(&env->graph,name,BasicDeclaration::output,env->outArena);
            COM_Ent* var = COM_PushEnt(env,node->token,COM_EntType_MODULE_UNIT);
            var->unit = unit;
            
            outputUnit = unit;
            break;
          }
        }
      }

      COM_ConnectInfoList lhsList = COM_UnpackVarGroup(env,lhsVarGroup,temp);
      COM_ConnectInfoList rhsList = COM_UnpackVarGroup(env,rhsVarGroup,temp);

      int lhsSize = lhsList.size;
      int rhsSize = rhsList.size;

      int broadcastLhs = (lhsSize == 1 && rhsSize != 1);
      int broadcastRhs = (lhsSize != 1 && rhsSize == 1);

      if(!(lhsSize == rhsSize || broadcastLhs || broadcastRhs)){
        COM_ReportError(env,SF("Missmatch in connection sizes, lhs is: %d, rhs is: %d",lhsSize,rhsSize),ptr);
      }

      COM_ConnectInfo* lhs = lhsList.head;
      COM_ConnectInfo* rhs = rhsList.head;

      while(lhs && rhs){
        COM_Connect(env,lhs->ent,lhs->port,rhs->ent,rhs->port,lhs->delay);

        if(broadcastLhs){
          rhs = rhs->next;
        } else if(broadcastRhs){
          lhs = lhs->next;
        } else {
          rhs = rhs->next;
          lhs = lhs->next;
        }
      }
    } break;

    case SP_Type_EQUALITY:{
      SP_Node* lhsVarGroup = ptr->childs;
      SP_Node* lhsVar = lhsVarGroup->childs;

      SP_Node* rhsExpr = lhsVarGroup->next;

      if(lhsVarGroup->type != SP_Type_VAR_GROUP || !lhsVar || lhsVar->type != SP_Type_VAR){
        COM_ReportError(env,"LHS of assignment can only contain a var without any access modifiers",lhsVarGroup);
      }

      // TODO: Check if parser already guarantees this.
      if(!rhsExpr || rhsExpr->type != SP_Type_EXPR){
        COM_ReportError(env,"RHS of assignment must be an expression",rhsExpr);
      }
      
      SP_Node* expr = SP_UnpackExpr(rhsExpr);

      COM_EntPort exprInst = COM_InstantiateExpression(env,expr,out);
      COM_Ent* lhs = COM_PushEnt(env,lhsVar->token,COM_EntType_MODULE_UNIT);
      lhs->unit = exprInst.ent.unit;
    } break;

    case SP_Type_FUNC_STATE:
    case SP_Type_FUNC_MEM:
    case SP_Type_FUNC_CONFIG:{
      TEMP_REGION(temp,out);

      Token name = ptr->token;

      bool isState  = (ptr->type == SP_Type_FUNC_STATE);
      bool isConfig = (ptr->type == SP_Type_FUNC_CONFIG);
      bool isMem =    (ptr->type == SP_Type_FUNC_MEM);
      
      bool debug = 0;
      bool sim = 0;

      // Start new env scope ========================================================
      COM_PushScope(env);
      defer{COM_PopScope(env);};

      SP_Node* stmtGroup = nullptr;
      for(SP_Node* input = ptr->childs; input; input = input->next){
        bool handled = 1;
        
        COM_EntType type = {};
        
        switch(input->type){
        case SP_Type_MODIFIER_DEBUG: debug = 1; break;
        case SP_Type_MODIFIER_SIM: sim = 1; break;

        case SP_Type_FUNC_TYPE_DYN:     if(type == COM_EntType_NIL) type = COM_EntType_ARG_DYN; // fallthrough
        case SP_Type_FUNC_TYPE_NONE:    if(type == COM_EntType_NIL) type = COM_EntType_ARG_NONE; // fallthrough
        case SP_Type_FUNC_TYPE_FIXED:   if(type == COM_EntType_NIL) type = COM_EntType_ARG_FIXED; // fallthrough
        case SP_Type_FUNC_TYPE_BUFFER:{ if(type == COM_EntType_NIL) type = COM_EntType_ARG_BUFFER; // fallthrough
          Token varName = input->token;
          COM_PushEnt(env,varName,type);
        } break;
        case SP_Type_STMT_LIST: stmtGroup = input; break;
        default: handled = 0;
        };
        Assert(handled);

        if(stmtGroup){
          break;
        }
      }
      
      // NOTE We always have a stmt group even if function is empty =================
      Assert(stmtGroup->next == nullptr);

      // Iterative way of breaking apart for loops into individual statements =======
      struct Work{
        Work* next;
        Work* parent;
        SP_Node* node;
      };

      Work start = {.node = stmtGroup};

      Work* head = &start;
      Work* tail = &start;

      // NOTE: Only contain leaf nodes, loops can be accesssed by following parent node
      Work* equalHead = 0;
      Work* equalTail = 0;

      while(head){
        Work* work = head;
        LL_PopFront(head,tail,next);
        SP_Node* node = work->node;

        if(node->type == SP_Type_EQUALITY ||
           node->type == SP_Type_FUNCTION_CALL){
          work->next = 0;
          LL_Append(equalHead,equalTail,next,work);
        }

        if(node->type == SP_Type_FOR_LOOP){
          SP_Node* stmtList = node->childs->next;
          Assert(stmtList->type == SP_Type_STMT_LIST);

          Work* newWork = PushStruct<Work>(temp);
          newWork->parent = work;
          newWork->node = stmtList;
          LL_Append(head,tail,next,newWork);
        }

        if(node->type == SP_Type_STMT_LIST){
          for(SP_Node* child = node->childs; child; child = child->next){
            Work* newWork = PushStruct<Work>(temp);
            newWork->parent = work->parent; // NOTE: We do not preserve SMTM_LIST, only EQUALITY and FOR_LOOP.
            newWork->node = child;
            LL_Append(head,tail,next,newWork);
          }
        }
      }

      COM_Stmt* stmtHead = 0;
      COM_Stmt* stmtTail = 0;

      // Unpack statements ==========================================================
      for(Work* work = equalHead; work; work = work->next){
        COM_PushScope(env);
        defer{COM_PopScope(env);};

        // Pack loops =================================================================
        bool insideFor = (work->parent != 0);
        Array<AddressGenForDef2> forLoops = {};
        if(insideFor){
          int forCount = 0;
          for(Work* ptr = work->parent; ptr; ptr = ptr->parent){
            forCount += 1;
          }

          forLoops = PushArray<AddressGenForDef2>(temp,forCount);

          int i = 0;
          for(Work* ptr = work->parent; ptr; ptr = ptr->parent,i += 1){
            SP_Node* forLoop = ptr->node;
            Token loopVar = forLoop->token;

            SP_Node* rangeExpr = SP_UnpackExpr(forLoop->childs);

            Assert(rangeExpr->type == SP_Type_RANGE);

            SP_Node* start = rangeExpr->first;
            SP_Node* end = rangeExpr->second;
            
            forLoops[i].loopVariable = loopVar.val;
            forLoops[i].startSym = COM_SymbolicFromExpression(env,start);
            forLoops[i].endSym = COM_SymbolicFromExpression(env,end);
          }
        }
        
        SP_Node* equalityOrFunction = work->node;

        SP_Node* lhsNode = SP_UnpackExpr(equalityOrFunction->childs);
        SP_Node* rhsNode = SP_UnpackExpr(equalityOrFunction->childs->next);

        COM_UnpackedExpr lhs = COM_UnpackExpr(env,lhsNode);
        COM_UnpackedExpr rhs = COM_UnpackExpr(env,rhsNode);

        if(isState){
          if(lhs.type != COM_ExprType_NAME){
            COM_ReportError(env,"State functions can only have simple variables on left side of assignments",lhsNode);
          }
        }

#if 1
        Assert(rhs.type != COM_ExprType_FUNC_CALL);

        COM_Stmt* stmt = nullptr;
        
        if(isState){
          stmt = PushStruct<COM_Stmt>(out);
          stmt->type = COM_StmtType_ASSIGN;
          stmt->lhs = PushString(out,lhs.name);
          stmt->rhs = PushString(out,rhs.name);
        } else {
          // We need to downgrade one of these into an expression =======================
          if(lhs.type == COM_ExprType_VAR && 
             rhs.type == COM_ExprType_VAR){

            bool found = 0;
            if(lhs.ent.type == COM_EntType_VAR_WITH_CONFIG){
              rhs.type = COM_ExprType_EXPR;
              rhs.expr = rhs.expr;
              found = 1;
            }

            Assert(found && "Can we handle Var - Var ???");
          }

          // MARK
          if(lhs.type == COM_ExprType_FUNC_CALL){
            
          }
        
          // 
          if(lhs.type == COM_ExprType_ARRAY_ACCESS){
            if(isConfig){
              SYM_Expr expr = COM_SymbolicFromExpression(env,lhs.expr);

              AddressAccess* access = CompileAddressGen2(forLoops,expr);

              stmt = PushStruct<COM_Stmt>(out);
              stmt->type = COM_StmtType_ADDR_GEN;
              stmt->rhs = rhs.ent.unit->name;
              stmt->access = access;
            }
          }
          if(rhs.type == COM_ExprType_ARRAY_ACCESS){
            if(isConfig){
              SYM_Expr expr = COM_SymbolicFromExpression(env,rhs.expr);

              AddressAccess* access = CompileAddressGen2(forLoops,expr);

              stmt = PushStruct<COM_Stmt>(out);
              stmt->type = COM_StmtType_ADDR_GEN;
              stmt->lhs = lhs.ent.unit->name;
              stmt->access = access;
            }
          }
        }
        
        if(stmt){
          LL_Append(stmtHead,stmtTail,next,stmt);
        }
#endif
      }

      // Pack into function =========================================================
      COM_FunctionNode* func = PushStruct<COM_FunctionNode>(out);

      LL_Append(funcHead,funcTail,next,func);
    } break;

    default: handled = 0;
    };
    Assert(handled);
  }

  if(env->anyError){
     exit(-1);
  }

  // Update id ==================================================================
  int id = 0;
  for(COM_Unit* ptr = env->graph.units.head; ptr; ptr = ptr->next){
    ptr->id = id++;
  }

  // Update number inputs
  for(COM_Edge* edge = env->graph.edges.head; edge; edge = edge->next){
    COM_Unit* in = edge->in;
    if(DECL_HasVariableInputs(edge->in->decl)){
      in->numberInputs = MAX(in->numberInputs,edge->inPort + 1);
    } else {
      in->numberInputs = in->decl->inputs.size;
    }
  }

  // Pack =======================================================================
  COM_Module mod = {}; 
  mod.name = PushString(out,moduleName);
  mod.graph = env->graph;
  mod.funcs = funcHead;
  
  return mod;
}

// ====================================== 
// Env

COM_Ent* COM_PushEnt(COM_Env* env,String name,COM_EntType type){
  for(COM_EntNode* ptr = env->entHead; ptr; ptr = ptr->next){
    if(ptr->v.name == name){
      return &ptr->v;
    }
  }

  COM_EntNode* res = 0;
  if(env->entFreeList){
    res = env->entFreeList;
    LL_PopFrontNoTail(env->entFreeList,next);
  } else {
    res = PushStruct<COM_EntNode>(env->arena);
  }
  
  res->v.name = name;
  res->v.type = type;
  res->v.scope = env->scope;

  LL_Append(env->entHead,env->entTail,next,res);

  printf("Added entity: %.*s\n",UN(name));

  // TODO: Error report on trying to insert out.

  return &res->v;
}

COM_Ent* COM_PushEnt(COM_Env* env,Token token,COM_EntType type){
  String name = token.val;

  COM_Ent* ent = COM_PushEnt(env,name,type);
  ent->token = token;

  return ent;
}

COM_Ent COM_GetEnt(COM_Env* env,Token name,bool canFail){
  for(COM_EntNode* ptr = env->entHead; ptr; ptr = ptr->next){
    if(ptr->v.name == name.val){
      return ptr->v;
    }
  }

  if(!canFail){
    COM_ReportError(env,"Did not find entity named",name);
  }

  return COM_Ent_Nil;
}

COM_Ent COM_ArrayAccess(COM_Env* env,COM_Ent array,int index){
  if(IsNil(array)){
    return COM_Ent_Nil;
  }

  NOT_IMPLEMENTED("TODO");
  return COM_Ent_Nil;
}

void COM_PushScope(COM_Env* env){
  printf("Push scope\n");
  env->scope += 1;
}

void COM_PopScope(COM_Env* env){
  printf("Pop scope\n");

  env->scope -= 1;
  if(env->scope < 0){
    Assert(false && "Too many pop scopes");
  }

  COM_EntNode* next = 0;
  COM_EntNode* prev = 0;
  for(COM_EntNode* ptr = env->entHead; ptr; ptr = next){
    next = ptr->next;

    if(ptr->v.scope > env->scope){
      LL_Remove(env->entHead,env->entTail,next,prev,ptr);
      LL_PushNoTail(env->entFreeList,next,ptr);
    } else {
      prev = ptr;
    }
  }
}

// ======================================
// Env Connections

void COM_Connect(COM_Env* env,COM_Ent out,int outPort,COM_Ent in,int inPort,int delay){
  FUDeclaration* outDecl = out.unit->decl;
  FUDeclaration* inDecl = in.unit->decl;

  if(!(IsNil(outDecl) || IsNil(inDecl))){
    int outPortCount = outDecl->outputs.size;
    int inPortCount = inDecl->inputs.size;
          
    // TODO: We can also show the offending expression since we could get the node representation
    if(outPort >= outPortCount && outDecl != BasicDeclaration::input){
      COM_ReportError(env,SF("Unit does not contain port index: %d",outPort),out.token);
    }
    // TODO: We can also show the offending expression since we could get the node representation
    if(inPort >= inPortCount && inDecl != BasicDeclaration::output){
      COM_ReportError(env,SF("Unit does not contain port index: %d",inPort),in.token);
    }

    COM_Edge* con = PushStruct<COM_Edge>(env->outArena);
    con->out = out.unit;
    con->outPort = outPort;
    con->in = in.unit;
    con->inPort = inPort;
    con->delay = delay;

    env->graph.edges.count += 1;
    LL_Append(env->graph.edges.head,env->graph.edges.tail,next,con);
  }
}

// ======================================
// Env error reporting

void COM_ReportError(COM_Env* env,String msg){
  printf("%.*s\n",UN(msg));
  env->anyError = 1;
}

void COM_ReportError(COM_Env* env,String msg,SP_Node* top){
  // TODO: Need to figure out all the text that the top node "encodes" and do a proper error report
  printf("%.*s\n",UN(msg));
  env->anyError = 1;
}

void COM_ReportError(COM_Env* env,String msg,Token token){
  printf("%.*s: %.*s\n",UN(msg),UN(token.val));
  env->anyError = 1;
}

// ======================================
// Repr

String COM_Repr(COM_Unit* top,Arena* out){
  TEMP_REGION(temp,out);

  auto b = StartString(temp);
  for(COM_Unit* ptr = top; ptr; ptr = ptr->next){
    b->PushString("%.*s\n",UN(ptr->name));
  }
  String res = EndString(out,b);
  return res;
}

void COM_DebugPushDotGraph(String filename,COM_Unit* top,COM_Edge* edges){
  NOT_IMPLEMENTED();
#if 0
  TEMP_REGION(temp,nullptr);

  auto l = PushList<GraphPrintingNodeInfo>(temp);
  for(COM_Unit* ptr = top; ptr; ptr = ptr->next){
    GraphPrintingNodeInfo* info = l->PushElem();
    info->name = PushString(temp,"%p",ptr);
    info->content = PushString(temp,ptr->name);
    info->color = Color_BLUE;
  }

  auto e = PushList<GraphPrintingEdgeInfo>(temp);
  for(COM_Edge* con = edges; con; con = con->next){
    GraphPrintingEdgeInfo* info = e->PushElem();

    info->firstNode = PushString(temp,"%p",con->out);
    info->secondNode = PushString(temp,"%p",con->in);
    info->content = PushString(temp,"%d -> %d",con->outPort,con->inPort);
    info->color = Color_BLACK;
  }

  GraphPrintingContent content = {};
  content.graphLabel = filename;
  content.nodes = PushArray(temp,l);
  content.edges = PushArray(temp,e);

  String result = GenerateDotGraph(content,temp);

  String filePath = GetDebugRegionFilepath(filename,temp);
  OutputContentToFile(filePath,result);
#endif
}

// ======================================
// Graph helpers

COM_Unit* COM_PushUnit(COM_Graph* graph,String name,FUDeclaration* decl,Arena* out){
  COM_Unit* unit = PushStruct<COM_Unit>(out);
  unit->name = PushString(out,name);
  unit->decl = decl;
  unit->numberInputs = decl->inputs.size;

  graph->units.count += 1;
  DLL_Append(graph->units.head,graph->units.tail,next,prev,unit);
  return unit;
}

void COM_InsertMiddle(COM_Graph* graph,COM_Edge* edge,COM_Unit* toInsert,int inPort,int outPort,Arena* out){
  COM_Unit* savedIn = edge->in;
  int savedInPort = edge->inPort;

  edge->in = toInsert;
  edge->inPort = inPort;

  COM_Edge* newEdge = {};
  if(graph->freeEdges){
    newEdge = graph->freeEdges;
    LL_PopFrontNoTail(graph->freeEdges,next);
    *newEdge = {};
  } else {
    newEdge = PushStruct<COM_Edge>(out);
  }

  newEdge->out = toInsert;
  newEdge->outPort = outPort;
  newEdge->in = savedIn;
  newEdge->inPort = savedInPort;

  graph->edges.count += 1;
  LL_Append(graph->edges.head,graph->edges.tail,next,newEdge);
}

void COM_ClearGeneralPtr(COM_Unit* units){
  for(COM_Unit* ptr = units; ptr; ptr = ptr->next){
    ptr->general = 0;
  }
}

void COM_ClearGeneralPtr(COM_Edge* edges){
  for(COM_Edge* ptr = edges; ptr; ptr = ptr->next){
    ptr->general = 0;
  }
}

// ======================================
// Graph algorithms

COM_UnitNodeList COM_CalculateDAGOrder(COM_Graph graph,Arena* outArena){
  TEMP_REGION(temp,outArena);

  COM_Unit* units = graph.units.head;
  COM_Edge* edges = graph.edges.head;

  int MARK_NIL    = 0;
  int MARK_TEMP   = 1;
  int MARK_PERM   = 2;
  int MARK_RETURN = 3;
  
  struct DAG_Data{
    int mark;
    int order;
  };

  for(COM_Unit* ptr = units; ptr; ptr = ptr->next){
    DAG_Data* data = PushStruct<DAG_Data>(temp);
    data->order = -1;
    ptr->general = (void*) data;
  }

  struct Work{
    Work* next;
    Work* prev;
    Work* parent;
    COM_Unit* unit;
    bool isReturn;
  };

  // Depth first approach (starting from sources until reaching sinks) ==========
  COM_UnitNode* nodeHead = 0;
  COM_UnitNode* nodeTail = 0;
  int count = 0;

  bool error = 0;
  for(COM_Unit* unit = units; unit; unit = unit->next){
    DAG_Data* unitData = (DAG_Data*) unit->general;
    
    if(unitData->mark == MARK_PERM){
      continue;
    }

    Work* head = 0;
    Work* tail = 0;

    Work top = {};
    top.unit = unit;
    DLL_Append(head,tail,next,prev,&top);
    
    while(tail){
      Work* w = tail;
      DLL_Remove(head,tail,next,prev,w);

      COM_Unit* u = w->unit;
      DAG_Data* d = (DAG_Data*) u->general;

      if(w->isReturn){
        d->mark = MARK_PERM;
        
        COM_UnitNode* node = PushStruct<COM_UnitNode>(outArena);
        node->v = u;
        LL_Push(nodeHead,nodeTail,next,node);
        count += 1;
      } else {
        if(d->mark == MARK_PERM){
          continue;
        }
        if(d->mark == MARK_TEMP){
          if(DECL_IsSink(unit->decl)){
            d->mark = MARK_PERM;

            COM_UnitNode* node = PushStruct<COM_UnitNode>(outArena);
            node->v = u;
            LL_Append(nodeHead,nodeTail,next,node);
            count += 1;

            continue;
          } else {
            error = 1;

            printf("Graph is not a DAG, the loop is:\n");
            for(Work* p = w; p; p = p->parent){
              printf("%.*s\n",UN(p->unit->name));
            }
            ENTER_DEBUG();
          }
        }

        d->mark = MARK_TEMP;

        Work* ret = PushStruct<Work>(temp);
        ret->unit = u;
        ret->isReturn = 1;
        ret->parent = w->parent;

        DLL_Append(head,tail,next,prev,ret);

        for(COM_Edge* edge = edges; edge; edge = edge->next){
          if(edge->out == u){
            Work* node = PushStruct<Work>(temp);
            node->parent = w;
            node->unit = edge->in;
            DLL_Append(head,tail,next,prev,node);
          }
        }
      }
    }
  }

  // Generate debug info ========================================================
  {
    {
      int count = 0;
      for(COM_UnitNode* node = nodeHead; node; count++,node = node->next){
        COM_Unit* ptr = node->v;
        DAG_Data* data = (DAG_Data*) ptr->general;
      
        data->order = count;
      }
    }    
    auto l = PushList<GraphPrintingNodeInfo>(temp);
    for(COM_Unit* ptr = units; ptr; ptr = ptr->next){
      GraphPrintingNodeInfo* info = l->PushElem();
      DAG_Data* data = (DAG_Data*) ptr->general;
      info->name = PushString(temp,"%p",ptr);
      info->content = PushString(temp,"%.*s (%d)",UN(ptr->name),data->order);
      info->color = Color_BLUE;
    }

    auto e = PushList<GraphPrintingEdgeInfo>(temp);
    for(COM_Edge* con = edges; con; con = con->next){
      GraphPrintingEdgeInfo* info = e->PushElem();

      info->firstNode = PushString(temp,"%p",con->out);
      info->secondNode = PushString(temp,"%p",con->in);
      info->content = PushString(temp,"%d -> %d",con->outPort,con->inPort);
      info->color = Color_BLACK;
    }

    GraphPrintingContent content = {};
    content.graphLabel = "Test";
    content.nodes = PushArray(temp,l);
    content.edges = PushArray(temp,e);

    String result = GenerateDotGraph(content,temp);

    String filePath = DEBUG_GetDebugRegionFilepath("DAG.dot",temp);
    OutputContentToFile(filePath,result);
  }
  
  for(COM_Unit* ptr = units; ptr; ptr = ptr->next){
    DAG_Data* data = (DAG_Data*) ptr->general;
    Assert(data->mark != MARK_NIL);
    ptr->general = 0;
  }

  COM_UnitNodeList res = {};
  res.head = nodeHead;
  res.tail = nodeTail;
  res.count = count;

  return res;
}

COM_DelayResult COM_CalculateDelay(COM_Graph graph,COM_UnitNodeList order,Arena* outArena){
  TEMP_REGION(temp,outArena);

  COM_Unit* units = graph.units.head;
  COM_Edge* edges = graph.edges.head;

  struct DELAY_Data{
    Array<int> globalInputLatency;
    Array<int> globalOutputLatency;
    Array<int> inputExtraDelay;
    Array<int> outputExtraDelay;
  };

  int count = 0;
  int inputCount = 0;
  int outputCount = 0;
  for(COM_UnitNode* ptr = order.head; ptr; ptr = ptr->next){
    COM_Unit* unit = ptr->v;
    DELAY_Data* data = PushStruct<DELAY_Data>(temp);
    unit->general = data;

    if(unit->decl == BasicDeclaration::input){
      inputCount += 1;
    }
    if(unit->decl == BasicDeclaration::output){
      outputCount = unit->numberInputs;
    }
  }

  // Initialize =================================================================
  for(COM_UnitNode* ptr = order.head; ptr; ptr = ptr->next){
    COM_Unit* unit = ptr->v;
    FUDeclaration* decl = unit->decl;
    DELAY_Data* data = (DELAY_Data*) unit->general;

    data->globalInputLatency = PushArray<int>(temp,unit->numberInputs);
    data->globalOutputLatency = PushArray<int>(temp,decl->outputs.size);
    data->inputExtraDelay = PushArray<int>(temp,unit->numberInputs);
    data->outputExtraDelay = PushArray<int>(temp,decl->outputs.size);
  }
  
  // Calculate global latency ===================================================
  for(COM_UnitNode* ptr = order.head; ptr; ptr = ptr->next){
    COM_Unit* unit = ptr->v;
    FUDeclaration* decl = unit->decl;
    DELAY_Data* data = (DELAY_Data*) unit->general;

    // Calculate output global latency from input latency =========================
    for(int outPort = 0; outPort < data->globalOutputLatency.size; outPort++){
      DECL_PortInfo outInfo = decl->outputs[outPort];

      Array<int> dependPorts = {};
      if(outInfo.inputDependencies.size > 0){
        dependPorts = outInfo.inputDependencies;
      } else {
        dependPorts = PushArrayRange(0,data->globalInputLatency.size,temp);
      }
      
      int max = INT_MIN;
      for(int index : dependPorts){
        max = MAX(max,data->globalInputLatency[index]);
      }

      if(max == INT_MIN){
        max = 0;
      }

      // Output latency is the maximum of input latency of dependent nodes plus the units own output delay
      data->globalOutputLatency[outPort] = max + decl->outputs[outPort].delay;

      // Align inputs so that they arrive at the unit at the same time ==============
      for(int index : dependPorts){
        data->globalInputLatency[index] = max;
        
        
        //int val = max - data->globalInputLatency[index];
        //Assert(val >= 0 && "If this fails then we need to add delays to outputs because of the fact that now ports can have individual delays");
        //data->inputExtraDelay[index] = val;
      }
    }

    // Propagate global latency ===================================================
    for(COM_Edge* edge = edges; edge; edge = edge->next){
      if(edge->out == unit){
        COM_Unit* other = edge->in;
        DELAY_Data* otherData = (DELAY_Data*) other->general;

        otherData->globalInputLatency[edge->inPort] = data->globalOutputLatency[edge->outPort] + edge->delay;
      }
    }
  }
  
  // TODO: Handle any delay edges in order to support 
  auto OutputDotGraph = [&](){
    BLOCK_REGION(temp);
    // Generate debug graph
    auto l = PushList<GraphPrintingNodeInfo>(temp);
    for(COM_Unit* ptr = units; ptr; ptr = ptr->next){
      DELAY_Data* data = (DELAY_Data*) ptr->general;
      int nodeDelay = INT_MAX;
      for(int delay : data->globalInputLatency){
        nodeDelay = MIN(nodeDelay,delay);
      }
      if(nodeDelay == INT_MAX){
        nodeDelay = 0;
      }

      GraphPrintingNodeInfo* info = l->PushElem();
      info->name = PushString(temp,"%p",ptr);
      info->content = PushString(temp,"%.*s (%d)",UN(ptr->name),nodeDelay);
      info->color = Color_BLUE;
    }

    auto e = PushList<GraphPrintingEdgeInfo>(temp);
    for(COM_Edge* edge = edges; edge; edge = edge->next){
      GraphPrintingEdgeInfo* info = e->PushElem();

      DELAY_Data* outData = (DELAY_Data*) edge->out->general;
      DELAY_Data* inData = (DELAY_Data*) edge->in->general;

      int extraOut = outData->outputExtraDelay[edge->outPort];
      int extraIn = inData->inputExtraDelay[edge->inPort];

      info->firstNode = PushString(temp,"%p",edge->out);
      info->secondNode = PushString(temp,"%p",edge->in);
      info->content = PushString(temp,"%d - %d | %d - %d | (%d -> %d)",outData->globalOutputLatency[edge->outPort],inData->globalInputLatency[edge->inPort],extraOut,extraIn,edge->outPort,edge->inPort);
      info->color = Color_BLACK;
    }

    GraphPrintingContent content = {};
    content.graphLabel = "Global latency, Node: Contains the minimum latency of every input edge. Edges: format is latency from OutPort to InPort, numbers inside parenthesis are the ports. [In a proper delay fixed graph the OutPort and InPort numbers match for all edges";
    content.nodes = PushArray(temp,l);
    content.edges = PushArray(temp,e);

    String result = GenerateDotGraph(content,temp);

    String filePath = DEBUG_GetDebugRegionFilepath("GlobalLatency.dot",temp);
    OutputContentToFile(filePath,result);
  };

  OutputDotGraph();
  
  // Pack result ================================================================
  Array<int> inputDelays = PushArray<int>(outArena,inputCount);
  Array<int> outputLatencies = {};
  
  for(COM_Unit* ptr = units; ptr; ptr = ptr->next){
    FUDeclaration* decl = ptr->decl;
    DELAY_Data* data = (DELAY_Data*) ptr->general;
    
    if(decl == BasicDeclaration::input){
      inputDelays[ptr->special] = data->outputExtraDelay[0];
    }
    if(decl == BasicDeclaration::output){
      outputLatencies = CopyArray(data->globalInputLatency,outArena);
    }
  }

  COM_EdgeDelay* head = 0;
  COM_EdgeDelay* tail = 0;
  for(COM_Edge* edge = edges; edge; edge = edge->next){
    DELAY_Data* outData = (DELAY_Data*) edge->out->general;
    DELAY_Data* inData = (DELAY_Data*) edge->in->general;

    int outputLatency = outData->globalOutputLatency[edge->outPort] + outData->outputExtraDelay[edge->outPort];
    int inputLatency = inData->globalInputLatency[edge->outPort] + inData->inputExtraDelay[edge->inPort];

    int edgeExtraDelay = inputLatency - outputLatency;
    Assert(edgeExtraDelay >= 0);

    // TODO: We need to implement pushing delays to source and sink units.
    //       I think that I also would like to formalize the fact that units can associate a delay port
    //       to an input or output port. The delay wire is always bound to either a set of ports or to a single port.
    //       If we do this right then we should also be able to have units like Reg having two delays, one for the
    //       input and another for the output which allow us to better control delay calculations.
    //       Also the example of memories connected to eachother probably requires something like this.
#if 0
    if(edge->out->decl == BasicDeclaration::input ||
       edge->in->decl  == BasicDeclaration::output){
      edgeExtraDelay = 0;
    }
#endif

    if(edgeExtraDelay){
      COM_EdgeDelay* node = PushStruct<COM_EdgeDelay>(outArena);
      node->delay = edgeExtraDelay;
      node->edge = edge;
      
      LL_Append(head,tail,next,node);
    }
  }
  
  COM_ClearGeneralPtr(units);

  COM_DelayResult res = {};
  res.edgeDelays = head;
  res.inputDelays = inputDelays;
  res.outputLatencies = outputLatencies;

  return res;
}
