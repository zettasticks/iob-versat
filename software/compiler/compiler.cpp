#include "compiler.hpp"

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

bool COM_IsVar(COM_EntType in){
  bool res = (in == COM_EntType_ARG_DYN || 
              in == COM_EntType_ARG_NONE || 
              in == COM_EntType_ARG_FIXED || 
              in == COM_EntType_ARG_BUFFER);
  return res;
}

// ======================================
// Constant expressions and computations

SYM_Expr COM_SymbolicFromExpression(COM_Env* env,SP_Node* node){
  TEMP_REGION(temp,nullptr);

  auto Recurse = [temp,env](auto Recurse,SP_Node* top) -> SYM_Expr{
    SYM_Expr res = SYM_Nil;
    
    int opCount = SP_Type_OpCount(top->type);
    
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
      COM_Ent ent = COM_GetEnt(env,top->token);

      int val = ent.currentValue;
      bool found = 1;
      switch(ent.type){
      case COM_EntType_PARAM:{
        // Nothing
      } break;
      default: found = 0;
      }

      if(!found){
        res = SYM_Var(top->token.identifier);
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

    case SP_Type_LITERAL:{
      res = SYM_Lit(top->token.number);
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

  SP_Node* expr = node;
  if(expr && expr->type == SP_Type_EXPR){
    expr = expr->childs;
  }

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
      var = COM_GetEnt(env,ptr->token);
    } else {
      Assert(ptr->childs->type == SP_Type_VAR);
      
      var = COM_GetEnt(env,ptr->childs->token);
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

COM_RangeValues COM_CalculateRange(COM_Env* env,SP_Node* rangeOrExpr){
  SP_Node* range = rangeOrExpr;
  if(range->type == SP_Type_EXPR){
    range = range->childs;
  }
  Assert(range->type == SP_Type_RANGE);

  COM_ConstantResult lowVal = COM_ComputeConstantValue(env,range->first);
  COM_ConstantResult highVal = COM_ComputeConstantValue(env,range->second);

  if(lowVal.anyError || highVal.anyError){
    COM_ReportError(env,"Range must be a constant expression",range);
  }

  COM_RangeValues res = {};
  res.low = lowVal.value;
  res.high = highVal.value;
  res.error = lowVal.anyError || highVal.anyError;
  return res;
}

COM_EntPort InstantiateExpression(COM_Env* env,SP_Node* top,Arena* out){
  bool isExpr = SP_Type_IsExpr(top->type);

  // MARK
  COM_Unit* unit = &COM_Unit_Nil;
  int port = 0;

  bool handled = 1;
  if(isExpr){
    int opCount = SP_Type_OpCount(top->type);
    String typeName = SP_Type_Name(top->type);

    COM_EntPort lhs = COM_EntPort_Nil;
    COM_EntPort rhs = COM_EntPort_Nil;

    if(opCount >= 1){
      lhs = InstantiateExpression(env,top->first,out);
    }
    if(opCount >= 2){
      rhs = InstantiateExpression(env,top->second,out);
    }

    FUDeclaration* decl = DECL_GetType(typeName,{});
    if(IsNil(decl)){
      COM_ReportError(env,SF("Did not find declaration '%.*s'",UN(typeName)));
    }
    
    unit = PushStruct<COM_Unit>(out);
    unit->decl = decl;
    unit->name = PushString(out,"%.*s_%d",UN(typeName),env->tempIndex++);
  } else {
    switch(top->type){
    case SP_Type_FUNCTION_CALL:{
      NOT_IMPLEMENTED("TODO");
    } break;
    case SP_Type_LITERAL:{
      NOT_IMPLEMENTED("TODO");
    } break;
    case SP_Type_VAR:{
      COM_Ent ent = COM_GetEnt(env,top->token);
      unit = ent.unit;
    } break;
    case SP_Type_PORT_ACCESS:{
      SP_Node* var = top->childs;
      Assert(var->type == SP_Type_VAR);

      SP_Node* expr = top->childs->next;
      
      COM_RangeValues range = COM_CalculateRange(env,expr);

      if(!range.error && range.low != range.high){
        COM_ReportError(env,"Cannot have range expressions inside expressions",expr);
      }
      
      COM_Ent ent = COM_GetEnt(env,var->token);
      unit = ent.unit;
      port = range.low;
    } break;

    default: handled = 0;
    }
  }

  Assert(handled);

  COM_EntPort res = {};
  res.ent.unit = unit;
  res.port = port;

  return res;
};

// ======================================
// Compilation

COM_Unit* COM_InstantiateModule(SP_Node* moduleDef,Array<ParamNameAndValue> topLevelParams,Arena* out){
  Assert(moduleDef->type = SP_Type_MODULE_DECL);

  FREE_ARENA(envArena);
  FREE_ARENA(envErrorArena);
  COM_Env envInst = {};
  COM_Env* env = &envInst;
  env->arena = envArena;
  env->errorArena = envErrorArena;

  for(SP_Node* ptr = moduleDef->childs; ptr; ptr = ptr->next){
    bool handled = 1;
    switch(ptr->type){
    case SP_Type_INPUT_DECL:{
      Token name = ptr->token;

      COM_PushEnt(env,name,COM_EntType_MODULE_INPUT);
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

      String typeName = ptr->token.identifier;

      ArenaList<ParamNameAndValue>* paramList = PushList<ParamNameAndValue>(temp);
      Array<ParamNameAndValue> params = {};
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

            ParamNameAndValue* val = paramList->PushElem();
            val->name = paramName.identifier;
            val->value = valuation.value;
          } break;
          case SP_Type_VAR_DECL:{
            Token name = ptr->token;

            if(params.size == 0 && !Empty(paramList)){
              params = PushArray(temp,paramList);
            }

            if(!decl){
              decl = DECL_GetType(typeName,params);
            }

            COM_Unit* unit = PushStruct<COM_Unit>(out);
            unit->name = PushString(out,name.identifier);
            unit->decl = decl;
            unit->debug = modDebug;
            unit->isShared = modShare;
            unit->isStatic = modStatic;
            unit->sharedIndex = shareIndex;

            DLL_Append(env->head,env->tail,next,prev,unit);
            
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
        COM_Connect(env,lhs->ent,lhs->port,rhs->ent,rhs->port,lhs->delay,out);

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
      
      SP_Node* expr = rhsExpr;
      if(expr && expr->type == SP_Type_EXPR){
        expr = expr->childs;
      }

      COM_EntPort exprInst = InstantiateExpression(env,expr,out);
      COM_Ent* lhs = COM_PushEnt(env,lhsVar->token,COM_EntType_MODULE_UNIT);
      lhs->unit = exprInst.ent.unit;
    } break;

    case SP_Type_FUNC_STATE:
    case SP_Type_FUNC_MEM:
    case SP_Type_FUNC_CONFIG:{
      TEMP_REGION(temp,out);

      Token name = ptr->token;
      
      bool debug = 0;
      bool sim = 0;

      COM_PushScope(env);

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

      // NOTE: List of work nodes that contain only EQUALITY whose loops can be accessed by following parent node
      Work* equalHead = 0;
      Work* equalTail = 0;

      while(head){
        Work* work = LL_PopFront(head,next);
        SP_Node* node = work->node;

        if(node->type == SP_Type_EQUALITY){
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

      - LEFT HERE - Need to finish functions, do not forget to see if we can implement any expression passing to functions calls [ex: do something like: unit.Function(x+1)] while we are in the middle of fixing the old implementation.

      COM_PopScope(env);
    } break;

    default: handled = 0;
    };
    Assert(handled);
  }

  if(env->anyError){
    
  }
  
  return env->head;
}

// ====================================== 
// Env

COM_Ent* COM_PushEnt(COM_Env* env,Token name,COM_EntType type){
  for(COM_EntNode* ptr = env->entHead; ptr; ptr = ptr->next){
    if(ptr->v.name.identifier == name.identifier){
      return &ptr->v;
    }
  }

  COM_EntNode* res = 0;
  if(env->entFreeList){
    res = LL_PopFront(env->entFreeList,next);
  } else {
    res = PushStruct<COM_EntNode>(env->arena);
  }
  
  res->v.name = name;
  res->v.type = type;
  res->v.scope = env->scope;

  // TODO: Error report on trying to insert out.

  return &res->v;
}

COM_Ent COM_GetEnt(COM_Env* env,Token name,bool canFail){
  for(COM_EntNode* ptr = env->entHead; ptr; ptr = ptr->next){
    if(ptr->v.name.identifier == name.identifier){
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
  env->scope += 1;
}

void COM_PopScope(COM_Env* env){
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
      LL_Push(env->entFreeList,next,ptr);
    } else {
      prev = ptr;
    }
  }
}

// ======================================
// Env Connections

void COM_Connect(COM_Env* env,COM_Ent out,int outPort,COM_Ent in,int inPort,int delay,Arena* arenaOut){
  FUDeclaration* outDecl = out.unit->decl;
  FUDeclaration* inDecl = in.unit->decl;

  if(!(IsNil(outDecl) || IsNil(inDecl))){
    int outPortCount = outDecl->NumberOutputs();
    int inPortCount = inDecl->NumberInputs();
          
    // TODO: We can also show the offending expression since we could get the node representation
    if(outPort >= outPortCount){
      COM_ReportError(env,SF("Unit does not contain port index: %d",outPort),out.name);
    }
    // TODO: We can also show the offending expression since we could get the node representation
    if(inPort >= inPortCount){
      COM_ReportError(env,SF("Unit does not contain port index: %d",inPort),in.name);
    }

    COM_Connection* con = PushStruct<COM_Connection>(arenaOut);
    con->out = out.unit;
    con->outPort = outPort;
    con->in = in.unit;
    con->inPort = inPort;
    con->delay = delay;

    LL_Append(env->conHead,env->conTail,next,con);
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
  printf("%.*s\n",UN(msg));
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

