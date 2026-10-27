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

// nocheckin
#if 0
bool IsNil(COM_UnpackedExpr in){
  bool res = (in.type == COM_UnpackedType_NIL);
  return res;
}
#endif

bool COM_Ent_IsVar(COM_EntType in){
  bool res = (in == COM_EntType_LOOP_VAR ||
              in == COM_EntType_ARG_DYN || 
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
  bool res = (in == COM_EntType_LOOP_VAR ||
              in == COM_EntType_PARAM ||
              in == COM_EntType_ARG_NONE ||
              in == COM_EntType_ARG_FIXED ||
              in == COM_EntType_ARG_DYN ||
              in == COM_EntType_ARG_BUFFER);

  return res;
}

bool COM_Ent_IsWire(COM_EntType in){
  bool res = (in == COM_EntType_VAR_WITH_STATE ||
              in == COM_EntType_VAR_WITH_CONFIG);
  return res;
}

bool COM_Ent_IsUnitLike(COM_EntType in){
  bool res = (in == COM_EntType_MODULE_UNIT ||
              in == COM_EntType_MERGE_TYPE);
  return res;
}

COM_Unit* COM_Copy(COM_Unit* unit,Arena* out){
  COM_Unit* copied = PushStruct<COM_Unit>(out);
  *copied = *unit;
  copied->next = copied->prev = 0;
  copied->mergeNext = copied->mergePrev = 0;
  copied->name = PushString(out,unit->name);
  return copied;
}

COM_CopyResult COM_Copy(COM_Graph in,bool computeMapping,Arena* out){
  COM_UnitList units = {};
  COM_EdgeList edges = {};

  for(COM_Unit* unit = in.units.head; unit; unit = unit->next){
    COM_Unit* copied = PushStruct<COM_Unit>(out);
    *copied = *unit;
    copied->name = PushString(out,unit->name);

    copied->next = copied->prev = nullptr;

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

    case SP_Node_ACCESS:  Assert(false);
#endif

    case SP_Type_FUNC_CALL: {
      String funcName = top->token.val;

      SP_Node* firstArg = top->childs;
      SP_Node* secondArg = top->childs->next;

      if(funcName == "Duty"){
        res = SYM_Duty(Recurse(Recurse,firstArg),Recurse(Recurse,secondArg));
      } else {
        // TODO: Change into proper error reporting.
        Assert(false);
      }
      
#if 0
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
#endif
    } break;

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
      COM_ConnectInfo* info = PushStruct<COM_ConnectInfo>(out);

      info->ent = var;
      size += 1;

      LL_Append(head,tail,next,info);
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

  COM_ConstantResult lowVal = {};
  COM_ConstantResult highVal = {};
  if(range->type == SP_Type_RANGE){
    lowVal = COM_ComputeConstantValue(env,range->first);
    highVal = COM_ComputeConstantValue(env,range->second);

    if(lowVal.divByZero || highVal.divByZero){
      COM_ReportError(env,"Div by zero detected",range);
    }

    if(lowVal.nonConstant || highVal.nonConstant){
      COM_ReportError(env,"Range must be a constant expression",range);
    }
  } else if(SP_Type_IsExpr(range->type)){
    lowVal = highVal = COM_ComputeConstantValue(env,range);
  } else {
    COM_ReportError(env,"Unknown construct inside range, expected expression or range",range);
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

  if(node->type == SP_Type_FUNC_CALL){
    SP_Node* varNode = node->first;
    SP_Node* argHead = node->second;

    COM_Ent var = COM_ResolveEntity(env,varNode,false);
    if(!COM_Ent_IsUnitLike(var.type)){
      return {};
    }
    
    String funcName = node->token.val;
    COM_Function* func = 0;
    for(COM_FunctionNode* f = var.decl->funcs; f; f = f->next){
      if(f->v.name == funcName){
        func = &f->v;
        break;
      }
    }

    var.type = COM_EntType_FUNCTION_CALL;
    var.node = argHead;
    var.func = func;
    
    return var;
  }

  bool isVarStuff = SP_Type_IsVarAccess(varAccessNode->type);
  if(!isVarStuff){
    return {};
  }
  
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
      FUDeclaration* decl = varSide.decl;
    
      String name = access.val;

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
      }
      
      if(!type){
        for(Wire w : decl->configs){
          if(w.name == name){
            type = COM_EntType_VAR_WITH_CONFIG;
            wireName = w.name;
            break;
          }
        }
      }

      if(!type){
        for(Wire w : decl->states){
          if(w.name == name){
            type = COM_EntType_VAR_WITH_STATE;
            wireName = w.name;
            break;
          }
        }
      }

      if(!type){
        COM_ReportError(env,"Entity does not contain wire",access);
      }

      res = varSide;
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

    if(!val.divByZero){
      bool constant = !val.nonConstant;

      if(constant){
        if(COM_Ent_IsArray(varSide.type)){
          NOT_IMPLEMENTED("TODO: Array stuff");
        } else {
          res = varSide;
          res.leftoverRange = accessExpr;
        }
      }

      if(!constant){
        if(COM_Ent_IsArray(varSide.type)){
          COM_ReportError(env,"Cannot have a non constant expression inside an array access",accessExpr);
        } else {
          res = varSide;
          res.leftoverRange = accessExpr;
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
  bool isOperation = SP_Type_IsOperation(top->type);

  // MARK
  COM_Ent ent = COM_Ent_Nil;
  int port = 0;
  int delay = 0;

  bool handled = 1;
  if(isOperation){
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

    COM_Ent* entPtr = COM_PushEnt(env,unit,{},COM_EntType_GENERATED_UNIT);
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

COM_FunctionNode* COM_CompileFunction(COM_Env* env,SP_Node* topNode,Arena* out){
  TEMP_REGION(temp,out);

  SP_Node* ptr = topNode;
  Token funcName = ptr->token;

  bool isState  = (ptr->type == SP_Type_FUNC_STATE);
  bool isConfig = (ptr->type == SP_Type_FUNC_CONFIG);
  bool isMem =    (ptr->type == SP_Type_FUNC_MEM);

  COM_FunctionType funcType = {};
  if(isState) funcType = COM_FunctionType_STATE;
  if(isConfig) funcType = COM_FunctionType_CONFIG;
  if(isMem) funcType = COM_FunctionType_MEM;
      
  bool debug = 0;
  bool sim = 0;

  // Start new env scope ========================================================
  COM_PushScope(env);
  defer{COM_PopScope(env);};

  // Process arguments and get statements =======================================
  auto paramList = PushList<COM_FuncParam>(temp);
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

          COM_FuncParam* arg = paramList->PushElem();
          arg->name = PushString(out,varName.val);
          arg->type = COM_ArgType_SIMPLE;
          arg->isDyn = (type == COM_EntType_ARG_DYN);

          if(type == COM_EntType_ARG_BUFFER){
            arg->type = COM_ArgType_ADDR;
          }
    } break;
    case SP_Type_STMT_LIST: stmtGroup = input; break;
    default: handled = 0;
    };
    Assert(handled);

    if(stmtGroup){
      break;
    }
  }
  Array<COM_FuncParam> params = PushArray<COM_FuncParam>(out,paramList);
      
  // NOTE We always have a stmt group even if function is empty =================
  Assert(stmtGroup->next == nullptr);

  // Flatten for loops into individual groups ===================================
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

  // Unpack statements ==========================================================
  COM_Stmt* stmtHead = 0;
  COM_Stmt* stmtTail = 0;
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

      forLoops = PushArray<AddressGenForDef2>(out,forCount);

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
        
        // Store if params are used in loop expressions ===============================
        SP_NodeList flattenedStart = SP_Flatten(start,temp);
        SP_NodeList flattenedEnd = SP_Flatten(end,temp);
        flattenedStart.tail->next = flattenedEnd.head;

        for(SP_NodeNode* n = flattenedStart.head; n; n = n->next){
          SP_Node* node = n->node;
          if(node->type == SP_Type_VAR){
            String name = node->token.val;
            for(COM_FuncParam& p : params){
              if(p.name == name){
                p.usedOnLoopExpressions = true;
              }
            }
          }
        }
      }
    }

    ReverseInPlace(forLoops);

    // Push loop variables in entity ==============================================
    for(AddressGenForDef2 loop : forLoops){
      COM_PushEnt(env,loop.loopVariable,COM_EntType_LOOP_VAR);
    } 
        
    SP_Node* equalityOrFunction = work->node;

    SP_Node* lhsNode = SP_UnpackExpr(equalityOrFunction->childs);
    SP_Node* rhsNode = SP_UnpackExpr(equalityOrFunction->childs->next);

    COM_Stmt* stmt = nullptr;

    // Handle state in here =======================================================
    if(isState){
      COM_Ent rhsEnt = COM_ResolveEntity(env,rhsNode,true);

      if(lhsNode->type != SP_Type_VAR){
        COM_ReportError(env,"LHS of state function must be a simple name",lhsNode);
      }
      if(rhsEnt.type != COM_EntType_VAR_WITH_STATE){
        COM_ReportError(env,"RHS of state function must be a state wire",rhsNode);
      }

      String name = lhsNode->token.val;

      // TODO: Need to unpack the rhs as well.
      stmt = PushStruct<COM_Stmt>(out);
      stmt->type = COM_StmtType_ASSIGN;
      stmt->lhs = PushString(out,name);
      stmt->rhs = PushString(out,"%.*s.%.*s",UN(rhsEnt.name),UN(rhsEnt.wireName));
    } else {
      // Unpack entities ============================================================
      // We decide which statement type we currently are from the lhs.
      COM_Ent lhsEnt = COM_ResolveEntity(env,lhsNode,true);

      bool ignore = false;

      // TODO: nocheckin: Cleanup all the statement creation code, a lot of repetition

      // Functions ==================================================================
      if(!stmt && lhsEnt.type == COM_EntType_FUNCTION_CALL){
        COM_Function* func = lhsEnt.func;

        ignore = 1;

        // Map arg to parameter =======================================================
        TrieMap<String,SYM_Expr>* paramToArg = PushTrieMap<String,SYM_Expr>(temp);
        int paramIndex = 0;
        SP_Node* arg = lhsEnt.node;

        for(; paramIndex < func->params.size && arg; arg = arg->next,paramIndex++){
          COM_FuncParam param = func->params[paramIndex];

          SYM_Expr argAsExpr = COM_SymbolicFromExpression(env,arg);
          paramToArg->Insert(param.name,argAsExpr);
        }
        
        if((paramIndex >= func->params.size && arg != nullptr) ||
           (paramIndex  < func->params.size && arg == nullptr)){
          // TODO: Better error reporting
          COM_ReportError(env,"Number of arguments does not match function parameter count",{});
          ENTER_DEBUG();
        }
            
        // Need to map the function to our side =======================================
        for(COM_Stmt* node = func->stmts; node; node = node->next){
          switch(node->type){
          case COM_StmtType_NIL: Assert(false); break;
          case COM_StmtType_ASSIGN: {
            COM_Stmt* stmt = PushStruct<COM_Stmt>(out);
            stmt->type = COM_StmtType_ASSIGN;
            stmt->lhs = PushString(out,"%.*s.%.*s",UN(lhsEnt.name),UN(node->lhs));
            stmt->rhsExpr = SYM_Replace(node->rhsExpr,paramToArg);
            stmt->rhs = SYM_Repr(stmt->rhsExpr,out);
                  
            LL_Append(stmtHead,stmtTail,next,stmt);
          } break;
          case COM_StmtType_ADDR_GEN:{
            // Compute true loops =========================================================
            int trueLoopSize = forLoops.size + node->accessLoops.size;
            Array<AddressGenForDef2> trueLoops = PushArray<AddressGenForDef2>(out,trueLoopSize);
            int index = 0;
            for(int i = 0; i < forLoops.size; i++){
              trueLoops[index++] = forLoops[i];
            }
            for(int i = 0; i < node->accessLoops.size; i++){
              trueLoops[index].startSym = SYM_Replace(node->accessLoops[i].startSym,paramToArg);
              trueLoops[index].endSym = SYM_Replace(node->accessLoops[i].endSym,paramToArg);
              trueLoops[index].loopVariable = node->accessLoops[i].loopVariable; // TODO: Need to check what happens in here

              index += 1;
            }

            SYM_Expr trueExpr = SYM_Replace(node->accessExpr,paramToArg);
            AddressAccess* access = CompileAddressGen2(trueLoops,trueExpr);

            COM_Stmt* stmt = PushStruct<COM_Stmt>(out);
            stmt->type = COM_StmtType_ADDR_GEN;
            stmt->lhs = lhsEnt.name;
            stmt->rhs = node->rhs;
            stmt->maxLoopSize = node->maxLoopSize;
            stmt->access.access = access;
            stmt->access.inst.type = node->access.inst.type;
            stmt->access.inst.loopsSupported = node->access.inst.type;

            stmt->accessExpr = node->accessExpr;
            stmt->accessLoops = trueLoops;

            LL_Append(stmtHead,stmtTail,next,stmt);
          } break;
          case COM_StmtType_MEM_COPY:{
            NOT_IMPLEMENTED();
          } break;
          }
        }
      }

      // TODO: nocheckin: We need to cleanup the if expressions since 
      //       its kinda sketchy how we separate from config/state/mem 
      //       We need to be more careful with order and whatnot.

      if(!stmt && lhsEnt.type == COM_EntType_VAR_WITH_CONFIG){
        SYM_Expr expr = COM_SymbolicFromExpression(env,rhsNode);

        stmt = PushStruct<COM_Stmt>(out);
        stmt->type = COM_StmtType_ASSIGN;
        stmt->lhs = PushString(out,"%.*s.%.*s",UN(lhsEnt.name),UN(lhsEnt.wireName));
        stmt->rhsExpr = expr;
        stmt->rhs = SYM_Repr(stmt->rhsExpr,out);
      }

      if(!stmt && isConfig && lhsEnt.type == COM_EntType_MODULE_UNIT){
        COM_Ent rhsEnt = COM_ResolveEntity(env,rhsNode,true);
        bool isRhsExpr = SP_Type_IsExpr(rhsNode->type);

        if(!stmt && rhsEnt.leftoverRange){
          SYM_Expr expr = COM_SymbolicFromExpression(env,rhsEnt.leftoverRange);
          AddressAccess* access = CompileAddressGen2(forLoops,expr);

          // TODO: Need to set current data based on the unit declaration.
          stmt = PushStruct<COM_Stmt>(out);
          stmt->type = COM_StmtType_ADDR_GEN;
          stmt->lhs = lhsEnt.name;
          stmt->rhs = rhsEnt.name;

          stmt->maxLoopSize = HW_GetAddrSize(lhsEnt.decl->externalMemory[0],0);
          stmt->access.access = access;
          stmt->access.dir = Direction_READ;
          stmt->access.inst.type = AddressGenType_READ;
          stmt->access.inst.loopsSupported = 2;

          // TODO: nocheckin: Resolve this
          stmt->accessExpr = expr;
          stmt->accessLoops = forLoops;
        }

        if(!stmt && lhsEnt.decl->supportedAddressGen.type == AddressGenType_GEN && isRhsExpr){
          SYM_Expr expr = COM_SymbolicFromExpression(env,rhsNode);
          AddressAccess* access = CompileAddressGen2(forLoops,expr);

          // TODO: Need to set current data based on the unit declaration.
          stmt = PushStruct<COM_Stmt>(out);
          stmt->type = COM_StmtType_ADDR_GEN;
          stmt->maxLoopSize = HW_GetAddrSize(lhsEnt.decl->externalMemory[0],0);
          stmt->lhs = lhsEnt.name;
          stmt->rhs = {};
          stmt->rhsExpr = expr;
          stmt->access.access = access;
          stmt->access.inst.type = AddressGenType_GEN;
          stmt->access.inst.loopsSupported = 2;

          // TODO: nocheckin: Resolve this
          stmt->accessExpr = expr;
          stmt->accessLoops = forLoops;
        }

        if(!stmt){
          COM_ReportError(env,"Unexpected expression",rhsNode);
        }
      }
      
      if(!stmt && lhsEnt.leftoverRange){
        COM_Ent rhsEnt = COM_ResolveEntity(env,rhsNode,true);

        if(isMem){
          if(!rhsEnt.leftoverRange){
            COM_ReportError(env,"Expected an unit on the right side",rhsNode);
          }

          if(lhsEnt.type != COM_EntType_MODULE_UNIT &&
             rhsEnt.type != COM_EntType_MODULE_UNIT){
            COM_ReportError(env,"Either lhs or rhs needs to be a unit",topNode);
          }
          if(lhsEnt.type == COM_EntType_MODULE_UNIT &&
             rhsEnt.type == COM_EntType_MODULE_UNIT){
            COM_ReportError(env,"Versat does not support memory transfers between units directly",topNode);
          }
          if(forLoops.size > 1){
            COM_ReportError(env,"We do not support more than one loop for memory transfers",topNode);
          }

          SYM_Expr expr = SYM_1;
          if(forLoops.size == 1){
            SYM_Expr start = forLoops[0].startSym;
            SYM_Expr end = forLoops[0].endSym;

            expr = end - start;
          }

          String unitName = {};
          String addrName = {};
          Direction dir = {};
          if(lhsEnt.type == COM_EntType_MODULE_UNIT){
            dir = Direction_READ;
            unitName = PushString(out,"%.*s_addr",UN(lhsEnt.name));
            addrName = rhsEnt.name;
          }
          if(rhsEnt.type == COM_EntType_MODULE_UNIT){
            dir = Direction_WRITE;
            unitName = PushString(out,"%.*s_addr",UN(rhsEnt.name));
            addrName = lhsEnt.name;
          }

          stmt = PushStruct<COM_Stmt>(out);
          stmt->type = COM_StmtType_MEM_COPY;
          stmt->lhs = unitName;
          stmt->rhs = addrName;
          stmt->rhsExpr = expr;
          stmt->dir = dir;
        } else if(isConfig){
          if(rhsEnt.type != COM_EntType_MODULE_UNIT){
            COM_ReportError(env,"Expected an unit on the right side",rhsNode);
          }

          SYM_Expr expr = COM_SymbolicFromExpression(env,lhsEnt.leftoverRange);
          AddressAccess* access = CompileAddressGen2(forLoops,expr);

          // TODO: Need to set current data based on the unit declaration.
          stmt = PushStruct<COM_Stmt>(out);
          stmt->type = COM_StmtType_ADDR_GEN;
          stmt->maxLoopSize = HW_GetAddrSize(rhsEnt.decl->externalMemory[0],0);
          stmt->lhs = rhsEnt.name;
          stmt->rhs = lhsEnt.name;
          stmt->access.access = access;
          stmt->access.dir = Direction_READ;
          stmt->access.inst.type = AddressGenType_READ;
          stmt->access.inst.loopsSupported = 2;

          // TODO: nocheckin: Resolve this
          stmt->accessExpr = expr;
          stmt->accessLoops = forLoops;
        }
      }

      if(!stmt && COM_Ent_IsWire(lhsEnt.type)){
        stmt = PushStruct<COM_Stmt>(out);
        stmt->type = COM_StmtType_ASSIGN;
        stmt->lhs = PushString(out,"%.*s.%.*s",UN(lhsEnt.name),UN(lhsEnt.wireName));
 
        SYM_Expr asExpr = COM_SymbolicFromExpression(env,rhsNode);
        stmt->rhs = SYM_Repr(asExpr,out);
      }

      if(!stmt && lhsEnt.type == COM_EntType_VAR_WITH_VIRTUAL_MEM){
        SYM_Expr expr = COM_SymbolicFromExpression(env,rhsNode);
        AddressAccess* access = CompileAddressGen2(forLoops,expr);
        
        stmt = PushStruct<COM_Stmt>(out);
        stmt->type = COM_StmtType_ADDR_GEN;
        stmt->lhs = lhsEnt.name;
        stmt->maxLoopSize = HW_GetAddrSize(lhsEnt.decl->externalMemory[0],0);
        stmt->access.access = access;
        stmt->access.dir = lhsEnt.dir;
        stmt->access.inst.type = AddressGenType_MEM;
        stmt->access.inst.loopsSupported = 1;
        stmt->access.port = lhsEnt.port;
        
        // TODO: nocheckin: Resolve this
        stmt->accessExpr = expr;
        stmt->accessLoops = forLoops;
      }

      Assert((stmt || ignore) && "Missing some processing");
    }
        
    if(stmt){
      LL_Append(stmtHead,stmtTail,next,stmt);
    }
  }

  // Pack into function =========================================================
  COM_FunctionNode* func = PushStruct<COM_FunctionNode>(out);
  func->v.name = PushString(out,funcName.val);
  func->v.stmts = stmtHead;
  func->v.type = funcType;
  func->v.params = params;
  func->v.structToReturn = isState ? PushString(out,"%.*s_Struct",UN(func->v.name)) : "";

  return func;
}

// nocheckin
#if 0
COM_UnpackedExpr COM_UnpackExpr(COM_Env* env,SP_Node* node){
  COM_UnpackedType type = {};
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
        type = COM_UnpackedType_ARRAY_ACCESS;
        expr = ent.node;
        found = 1;
      }

      if(!found && isWire){
        type = COM_UnpackedType_WIRE;
        name = ent.wireName;
        found = 1;
      }

      if(!found && isExpr){
        type = COM_UnpackedType_EXPR;
        expr = node;
        found = 1;
      }

      if(!found && ent.type == COM_EntType_NIL && node->type == SP_Type_VAR){
        type = COM_UnpackedType_NAME;
        name = node->token.val;
        found = 1;
      }
    }

    if(!found && node->type == SP_Type_FUNCTION_CALL){
      NOT_IMPLEMENTED("TODO");
      type = COM_UnpackedType_FUNC_CALL;
      found = 1;
    }

    if(isExpr){
      expr = node;
      type = COM_UnpackedType_EXPR;
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

COM_UnpackedExpr COM_CollapseToName(COM_UnpackedExpr expr,Arena* out){
  COM_UnpackedExpr res = {};

  if(expr.type == COM_UnpackedType_NAME){
    res = expr;
    Assert(!Empty(res.name));
  }
  if(expr.type == COM_UnpackedType_VAR){
    res = expr;
    res.name = expr.ent.name;
    Assert(!Empty(res.name));
  }
  if(expr.type == COM_UnpackedType_EXPR){
    if(expr.expr->type == SP_Type_VAR){
      res.type = COM_UnpackedType_NAME;
      res.name = PushString(out,expr.expr->token.val);
      Assert(!Empty(res.name));
    }
  }

  return res;
}
#endif

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

      COM_Ent* ent = COM_PushEnt(env,in,name,COM_EntType_MODULE_UNIT);
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

      {
        DECL_Param* param = paramList->PushElem();
        param->name = "DELAY_W";
        param->val = SYM_DelayW;
      }
      if(0){
        DECL_Param* param = paramList->PushElem();
        param->name = "ADDR_W";
        param->val = SYM_AddrW;
      }
      {
        DECL_Param* param = paramList->PushElem();
        param->name = "DATA_W";
        param->val = SYM_DataW;
      }
      {
        DECL_Param* param = paramList->PushElem();
        param->name = "LEN_W";
        param->val = SYM_LenW;
      }
      {
        DECL_Param* param = paramList->PushElem();
        param->name = "AXI_DATA_W";
        param->val = SYM_AxiDataW;
      }
      {
        DECL_Param* param = paramList->PushElem();
        param->name = "AXI_ADDR_W";
        param->val = SYM_AxiAddrW;
      }

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
            
            COM_Ent* var = COM_PushEnt(env,unit,name,COM_EntType_MODULE_UNIT);
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
        SP_NodeList flatten = SP_Flatten(rhsVarGroup,temp);
        
        for(SP_NodeNode* ptr = flatten.head; ptr; ptr = ptr->next){
          SP_Node* node = ptr->node;

          if(node->type == SP_Type_VAR && node->token.val == "out"){
            String name = "out";

            COM_Unit* unit = COM_PushUnit(&env->graph,name,BasicDeclaration::output,env->outArena);
            COM_Ent* var = COM_PushEnt(env,node->token,COM_EntType_MODULE_UNIT);
            var->unit = unit;
            var->name = unit->name;
            var->decl = unit->decl;
            
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
      lhs->decl = exprInst.ent.unit->decl;
    } break;


    case SP_Type_FUNC_STATE:
    case SP_Type_FUNC_MEM:
    case SP_Type_FUNC_CONFIG:{
      COM_FunctionNode* func = COM_CompileFunction(env,ptr,out);
      LL_Append(funcHead,funcTail,next,func);

#if 0
      TEMP_REGION(temp,out);

      Token funcName = ptr->token;

      bool isState  = (ptr->type == SP_Type_FUNC_STATE);
      bool isConfig = (ptr->type == SP_Type_FUNC_CONFIG);
      bool isMem =    (ptr->type == SP_Type_FUNC_MEM);
      
      bool debug = 0;
      bool sim = 0;

      // Start new env scope ========================================================
      COM_PushScope(env);
      defer{COM_PopScope(env);};

      auto argList = PushList<COM_Arg>(temp);
      
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

          COM_Arg* arg = argList->PushElem();
          arg->name = PushString(out,varName.val);
          arg->type = COM_ArgType_SIMPLE;

          if(type == COM_EntType_ARG_BUFFER){
            arg->type = COM_ArgType_ADDR;
          }
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

        ReverseInPlace(forLoops);
        
        SP_Node* equalityOrFunction = work->node;

        SP_Node* lhsNode = SP_UnpackExpr(equalityOrFunction->childs);
        SP_Node* rhsNode = SP_UnpackExpr(equalityOrFunction->childs->next);

        COM_Stmt* stmt = nullptr;

        // Handle state in here =======================================================
        if(isState){
          COM_Ent rhsEnt = COM_ResolveEntity(env,rhsNode,true);

          if(lhsNode->type != SP_Type_VAR){
            COM_ReportError(env,"LHS of state function must be a simple name",lhsNode);
          }
          if(rhsEnt.type != COM_EntType_VAR_WITH_STATE){
            COM_ReportError(env,"RHS of state function must be a state wire",rhsNode);
          }

          String name = lhsNode->token.val;

          // TODO: Need to unpack the rhs as well.
          stmt = PushStruct<COM_Stmt>(out);
          stmt->type = COM_StmtType_ASSIGN;
          stmt->lhs = PushString(out,name);
          stmt->rhs = PushString(out,"%.*s.%.*s",UN(rhsEnt.name),UN(rhsEnt.wireName));
        } else {
          // Unpack entities ============================================================
          
          // We decide which statement type we currently are from the lhs.
          COM_Ent lhsEnt = COM_ResolveEntity(env,lhsNode,true);

          // Missing function handling.
#if 0
          if(!stmt && lhsEnt.type == COM_EntType_){
            NOT_IMPLEMENTED();
          }
#endif

          bool ignore = false;

          if(!stmt && lhsEnt.type == COM_EntType_FUNCTION_CALL){
            COM_Function* func = lhsEnt.func;

            ignore = 1;
            
            // Need to map the function to our side =======================================
            for(COM_Stmt* node = func->stmts; node; node = node->next){
              switch(node->type){
                case COM_StmtType_NIL: Assert(false); break;
                case COM_StmtType_ASSIGN: {
                  COM_Stmt* stmt = PushStruct<COM_Stmt>(out);
                  stmt->type = COM_StmtType_ASSIGN;
                  stmt->lhs = PushString(out,"%.*s.%.*s",UN(lhsEnt.unit->name),UN(node->lhs));
                  stmt->rhs = node->rhs;
                  
                  LL_Append(stmtHead,stmtTail,next,stmt);
                } break;
                case COM_StmtType_ADDR_GEN:{
                  NOT_IMPLEMENTED();
                } break;
                case COM_StmtType_MEM_COPY:{
                  NOT_IMPLEMENTED();
                } break;
              }
            }
          }

          if(!stmt && lhsEnt.type == COM_EntType_VAR_WITH_CONFIG){
            SYM_Expr expr = COM_SymbolicFromExpression(env,rhsNode);

            stmt = PushStruct<COM_Stmt>(out);
            stmt->type = COM_StmtType_ASSIGN;
            stmt->lhs = PushString(out,"%.*s.%.*s",UN(lhsEnt.unit->name),UN(lhsEnt.wireName));
            stmt->rhs = SYM_Repr(expr,out);
          }

          if(!stmt && lhsEnt.type == COM_EntType_MODULE_UNIT){
            COM_Ent rhsEnt = COM_ResolveEntity(env,rhsNode,true);

            if(rhsEnt.type != COM_EntType_VAR_WITH_LEFTOVER_RANGE){
              COM_ReportError(env,"Unexpected expression",rhsNode);
            } else {
              SYM_Expr expr = COM_SymbolicFromExpression(env,rhsEnt.node);
              AddressAccess* access = CompileAddressGen2(forLoops,expr);

              // TODO: Need to set current data based on the unit declaration.
              stmt = PushStruct<COM_Stmt>(out);
              stmt->type = COM_StmtType_ADDR_GEN;
              stmt->lhs = lhsEnt.name;
              stmt->rhs = rhsEnt.name;
              stmt->access.access = access;
              stmt->access.dir = Direction_READ;
              stmt->access.inst.type = AddressGenType_READ;
              stmt->access.inst.loopsSupported = 2;
            }
          }

          if(!stmt && lhsEnt.type == COM_EntType_VAR_WITH_LEFTOVER_RANGE){
            //SYM_Expr expr = COM_SymbolicFromExpression(env,rhsNode);
            COM_Ent rhsEnt = COM_ResolveEntity(env,rhsNode,true);

            if(rhsEnt.type != COM_EntType_MODULE_UNIT){
              COM_ReportError(env,"Expected an unit on the right side",rhsNode);
            }

            SYM_Expr expr = COM_SymbolicFromExpression(env,lhsEnt.node);
            AddressAccess* access = CompileAddressGen2(forLoops,expr);

            // TODO: Need to set current data based on the unit declaration.
            stmt = PushStruct<COM_Stmt>(out);
            stmt->type = COM_StmtType_ADDR_GEN;
            stmt->lhs = rhsEnt.name;
            stmt->rhs = lhsEnt.name;
            stmt->access.access = access;
            stmt->access.dir = Direction_READ;
            stmt->access.inst.type = AddressGenType_READ;
            stmt->access.inst.loopsSupported = 2;
          }

          if(!stmt && COM_Ent_IsWire(lhsEnt.type)){
            stmt = PushStruct<COM_Stmt>(out);
            stmt->type = COM_StmtType_ASSIGN;
            stmt->lhs = PushString(out,"%.*s.%.*s",UN(lhsEnt.name),UN(lhsEnt.wireName));
 
            SYM_Expr asExpr = COM_SymbolicFromExpression(env,rhsNode);
            stmt->rhs = SYM_Repr(asExpr,out);
          }

          Assert((stmt || ignore) && "Missing some processing");
        }
        
        if(stmt){
          LL_Append(stmtHead,stmtTail,next,stmt);
        }
      }

      // Pack into function =========================================================
      COM_FunctionNode* func = PushStruct<COM_FunctionNode>(out);
      func->v.name = PushString(out,funcName.val);
      func->v.stmts = stmtHead;
      func->v.type = COM_FunctionType_CONFIG;
      func->v.args = PushArray(out,argList);

      LL_Append(funcHead,funcTail,next,func);
#endif
    } break;

    default: handled = 0;
    };
    Assert(handled);
  }

  if(env->anyError){
    printf("Error on instantiate, terminating\n");
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

COM_Ent* COM_PushEnt(COM_Env* env,COM_Unit* unit,Token nameToken,COM_EntType type){
  COM_Ent* ent = COM_PushEnt(env,nameToken,type);
  ent->unit = unit;
  ent->decl = unit->decl;
  ent->name = unit->name;
  return ent;
}

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
    *res = {};
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
      LL_PushNoTail(env->entFreeList,next,ptr);
    } else {
      prev = ptr;
    }
  }
}

// ======================================
// Env Connections

void COM_Connect(COM_Env* env,COM_Ent out,int outPort,COM_Ent in,int inPort,int delay){
  FUDeclaration* outDecl = out.decl;
  FUDeclaration* inDecl = in.decl;

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

    LL_Append(env->graph.edges.head,env->graph.edges.tail,next,con);
  }
}

// ======================================
// Env error reporting

void COM_ReportError(COM_Env* env,String msg){
  printf("%.*s\n",UN(msg));
  env->anyError = 1;
  ENTER_DEBUG();
}

void COM_ReportError(COM_Env* env,String msg,SP_Node* top){
  // TODO: Need to figure out all the text that the top node "encodes" and do a proper error report
  printf("%.*s\n",UN(msg));
  env->anyError = 1;
  ENTER_DEBUG();
}

void COM_ReportError(COM_Env* env,String msg,Token token){
  printf("%.*s: %.*s\n",UN(msg),UN(token.val));
  env->anyError = 1;
  ENTER_DEBUG();
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

String COM_DebugPushDotGraph(String name,COM_Unit* units,COM_Edge* edges,Arena* out,COM_ReprFunction Repr){
  TEMP_REGION(temp,out);

  auto l = PushList<DOT_Info>(temp);
  for(COM_Unit* ptr = units; ptr; ptr = ptr->next){
    DOT_Info* info = l->PushElem();
    *info = Repr(ptr,nullptr,out);
    info->name = PushString(out,"%p",ptr);
  }
  for(COM_Edge* con = edges; con; con = con->next){
    DOT_Info* info = l->PushElem();
    *info = Repr(nullptr,con,out);
    info->firstNode  = PushString(out,"%p",con->out);
    info->secondNode = PushString(out,"%p",con->in);
  }

  DOT_Graph content = {};
  content.graphLabel = name;
  content.content = PushArray(temp,l);

  String result = GenerateDotGraph(content,out);
  return result;
}

void COM_DebugOutputDotGraph(String filepath,String name,COM_Unit* units,COM_Edge* edges,COM_ReprFunction repr){
  TEMP_REGION(temp,nullptr);
  String content = COM_DebugPushDotGraph(name,units,edges,temp,repr);

  String filePath = DEBUG_GetDebugRegionFilepath(filepath,temp);
  OutputContentToFile(filePath,content);
}

// ======================================
// Graph helpers

COM_Unit* COM_GetInputUnit(COM_Graph graph,int index){
  for(COM_Unit* ptr = graph.units.head; ptr; ptr = ptr->next){
    if(ptr->decl == BasicDeclaration::input && ptr->special == index){
      return ptr;
    }
  }
  return &COM_Unit_Nil;
}

COM_Unit* COM_GetOutputUnit(COM_Graph graph){
  for(COM_Unit* ptr = graph.units.head; ptr; ptr = ptr->next){
    if(ptr->decl == BasicDeclaration::output){
      return ptr;
    }
  }
  return &COM_Unit_Nil;
}

COM_Unit* COM_PushUnit(COM_Graph* graph,String name,FUDeclaration* decl,Arena* out){
  COM_Unit* unit = 0;

  if(graph->freeUnits){
    unit = graph->freeUnits;
    LL_PopFrontNoTail(graph->freeUnits,next);
    *unit = {};
  } else {
    unit = PushStruct<COM_Unit>(out);
  }

  unit->name = PushString(out,name);
  unit->decl = decl;
  unit->numberInputs = decl->inputs.size;

  DLL_Append(graph->units.head,graph->units.tail,next,prev,unit);
  return unit;
}

COM_Edge* COM_PushEdge(COM_Graph* graph,Arena* out){
  COM_Edge* edge = 0;

  if(graph->freeEdges){
    edge = graph->freeEdges;
    LL_PopFrontNoTail(graph->freeEdges,next);
    *edge = {};
  } else {
    edge = PushStruct<COM_Edge>(out);
  }

  LL_Append(graph->edges.head,graph->edges.tail,next,edge);
  return edge;
}

COM_Edge* COM_InsertMiddle(COM_Graph* graph,COM_Edge* edge,COM_Unit* toInsert,int inPort,int outPort,Arena* out){
  COM_Unit* savedIn = edge->in;
  int savedInPort = edge->inPort;

  edge->in = toInsert;
  edge->inPort = inPort;

  COM_Edge* newEdge = COM_PushEdge(graph,out);
  newEdge->out = toInsert;
  newEdge->outPort = outPort;
  newEdge->in = savedIn;
  newEdge->inPort = savedInPort;

  return newEdge;
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
    int count = 0;
    for(COM_UnitNode* node = nodeHead; node; count++,node = node->next){
      COM_Unit* ptr = node->v;
      DAG_Data* data = (DAG_Data*) ptr->general;
      
      data->order = count;
    }

    auto Repr = [](COM_Unit* unit,COM_Edge* edge,Arena* out) -> DOT_Info{
      DOT_Info res = {};
      if(unit){
        DAG_Data* data = (DAG_Data*) unit->general;
        res.middleContent = PushString(out,"%.*s (%d)",UN(unit->name),data->order);
      }

      return res;
    };

    COM_DebugOutputDotGraph("DAG.dot","DAG",units,edges,Repr);
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

  /* NOTE: How does this work?

    For every node in order:

    We start with the global latency at each input port. This is just the number of cycles it takes for
    valid data to arrive at a port (or exit from a port) without regards for anything else.

    We calculate delays based on the ports themselves instead of caring about the unit (since we can calculate wether an output port depends on a given input port). 

    To align inputs we add extra latency to the ports if needed. Because multiple ports might depend on the same input port, we might need to also add extra latency to output ports and align inputs.

    All these calculations are stored as extra latency associated to each port.

    After this point, we propagate this information by setting the global latency of the input ports of the units connected to ours and continue until we finish all the nodes.

    At the end, we end up with extra delays both in the input ports and the output ports. If we wanted we could just add a buffer equal to this sum and everything would line up correctly, but because we have units that can delay themselves, we do a step that tries to push these extra delays towards each edge of the graph (inputs / outputs) and then we pack this information for the output.

   */

  COM_Unit* units = graph.units.head;
  COM_Edge* edges = graph.edges.head;

  struct DELAY_Data{
    Array<int> globalInputLatency;
    Array<int> globalOutputLatency;
    Array<int> inputExtraDelay;
    Array<int> outputExtraDelay;
    bool delaySet;
  };

  struct DELAY_EdgeData{
    bool isAny;
  };

  int count = 0;
  int inputCount = 0;
  int outputCount = 0;
  for(COM_UnitNode* ptr = order.head; ptr; ptr = ptr->next){
    COM_Unit* unit = ptr->v;
    DELAY_Data* data = PushStruct<DELAY_Data>(temp);
    Assert(!unit->general);
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

  FUDeclaration* varBuffer = DECL_GetVariableBufferType();
  for(COM_Edge* edge = edges; edge; edge = edge->next){
    DELAY_EdgeData* data = PushStruct<DELAY_EdgeData>(temp);
    Assert(!edge->general);
    edge->general = data;
    
    if(edge->out->decl == varBuffer){
      data->isAny = true;
    }
  }

  // Iteration ==================================================================
  for(COM_UnitNode* ptr = order.head; ptr; ptr = ptr->next){
    COM_Unit* unit = ptr->v;
    FUDeclaration* decl = unit->decl;
    DELAY_Data* data = (DELAY_Data*) unit->general;

    // Calculate output global latency from input latency =========================
    for(int outPort = 0; outPort < data->globalOutputLatency.size; outPort++){
      DECL_PortInfo outInfo = decl->outputs[outPort];

      // TODO: Take another look at isAny and properly propagate it if possible.

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

      // Add extra latency on inputs to align =======================================
      for(int index : dependPorts){
        if(data->globalInputLatency[index] != max){
          data->inputExtraDelay[index] = max - data->inputExtraDelay[index];
        }
      }

      // Output latency is the maximum of input latency of dependent nodes plus the units own output delay
      data->globalOutputLatency[outPort] = max + decl->outputs[outPort].delay;

#if 0
      // Align inputs so that they arrive at the unit at the same time ==============
      for(int index : dependPorts){
        data->globalInputLatency[index] = max;
      }
#endif
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
  
  // Delays is the amount of cycles that we must delay a given port in order for things to line up.
  // 

  // TODO: Handle any delay edges in order to support 
  auto DelayRepr = [](COM_Unit* unit,COM_Edge* edge,Arena* out) -> DOT_Info{
    DOT_Info res = {};
    if(unit){
      DELAY_Data* data = (DELAY_Data*) unit->general;

      int nodeDelay = INT_MAX;
      for(int delay : data->globalInputLatency){
        nodeDelay = MIN(nodeDelay,delay);
      }
      if(nodeDelay == INT_MAX){
        nodeDelay = 0;
      }
      res.middleContent = PushString(out,"%.*s (%d)",UN(unit->name),nodeDelay);
    }
    if(edge){
      DELAY_Data* outData = (DELAY_Data*) edge->out->general;
      DELAY_Data* inData = (DELAY_Data*) edge->in->general;

      int extraOut = outData->outputExtraDelay[edge->outPort];
      int extraIn = inData->inputExtraDelay[edge->inPort];

      res.headContent = PushString(out,"(%d - %d)",outData->globalOutputLatency[edge->outPort],extraOut);
      res.middleContent = PushString(out,"%d -> %d",edge->outPort,edge->inPort);
      res.tailContent = PushString(out,"(%d - %d)",inData->globalInputLatency[edge->inPort],extraIn);
    }

    return res;
  };

  String label = "Global latency, Node: Contains the minimum latency of every input edge. Edges near nodes: (Global latency - extra delay), Edges middle: port connection";
  COM_DebugOutputDotGraph("GlobalLatency.dot",label,units,edges,DelayRepr);
  
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

  // TODO: We are currently processing things based on latencies. We can just convert latencies into delays
  //       and interact with the graph that way. A delay can be pushed across units (we can remove the delay of 
  //       an edge by changing the delays of the units and the edges that the delay affects).
  //       In theory this should make things simpler, the final algorithm purpose is to push delays towards
  //       input and output units, trying to minimize the amount of extra buffers needed to insert.
  //       We could even try to check if we could transform the problem into a optimization problem that 
  //       we can then solve by minimization. Do not know if this is actually needed to obtain the minimum values
  //       but could try.

  COM_PortDelay* portHead = 0;
  COM_PortDelay* portTail = 0;

  COM_EdgeDelay* head = 0;
  COM_EdgeDelay* tail = 0;
  for(COM_Edge* edge = edges; edge; edge = edge->next){
    DELAY_EdgeData* edgeData = (DELAY_EdgeData*) edge->general;
    DELAY_Data* outData = (DELAY_Data*) edge->out->general;
    DELAY_Data* inData = (DELAY_Data*) edge->in->general;

    int outputDelay = outData->outputExtraDelay[edge->outPort];
    int inputDelay = inData->inputExtraDelay[edge->inPort];

    int edgeTotalDelay = inputDelay + outputDelay;
    Assert(edgeTotalDelay >= 0);
   
    bool isComputation = DECL_IsComputeWithDelay(edge->in->decl);
    bool canPushToOut  = DECL_IsSource(edge->out->decl);
    bool canPushToIn   = DECL_IsSink(edge->in->decl);

    // Already pushed delay for input and outputs =================================
    if(edge->out->decl == BasicDeclaration::input ||
       edge->in->decl  == BasicDeclaration::output){
      edgeTotalDelay = 0;

      canPushToOut = false;
      canPushToIn = false;
    }

    if(isComputation || canPushToIn){
      canPushToIn = false;

      COM_PortDelay* d = PushStruct<COM_PortDelay>(outArena);
      d->unit = edge->in;
      d->port = edge->inPort;
      d->dir = Direction_INPUT;
      d->extraDelay = inData->globalInputLatency[edge->inPort];
      outData->delaySet = true;

      LL_Append(portHead,portTail,next,d);

      edgeTotalDelay = 0;
    }

    if(canPushToOut){
      canPushToIn = false;

      COM_PortDelay* d = PushStruct<COM_PortDelay>(outArena);
      d->unit = edge->out;
      d->port = edge->outPort;
      d->dir = Direction_OUTPUT;
      d->extraDelay = outputDelay;
      outData->delaySet = true;

      LL_Append(portHead,portTail,next,d);

      edgeTotalDelay = 0;
    }

    // TODO: nocheckin: Need to store the amount of delays for variable buffers somewhere.
    if(edgeTotalDelay != 0 && !edgeData->isAny){
      COM_EdgeDelay* node = PushStruct<COM_EdgeDelay>(outArena);
      node->delay = edgeTotalDelay;
      node->edge = edge;
      
      LL_Append(head,tail,next,node);
    }
  }
   
  COM_ClearGeneralPtr(units);
  COM_ClearGeneralPtr(edges);

  COM_DelayResult res = {};
  res.edgeDelays = head;
  res.portDelays = portHead;
  res.inputDelays = inputDelays;
  res.outputLatencies = outputLatencies;

  return res;
}

COM_Graph COM_Flatten(COM_Graph in,Arena* out){
  TEMP_REGION(temp,out);

  COM_CopyResult copied = COM_Copy(in,false,out);
  COM_Graph graph = copied.result;

  void* MARK_TO_DELETE = (void*) 0x1;

  struct Work{
    Work* next;
    COM_Unit* toProcess;
  };

  Work* workHead = 0;
  Work* workTail = 0;

  for(COM_Unit* ptr = graph.units.head; ptr; ptr = ptr->next){
    if(DECL_IsComposite(ptr->decl)){
      Work* w = PushStruct<Work>(temp);
      w->toProcess = ptr;
      LL_Append(workHead,workTail,next,w);
    }
  }

  FREE_ARENA(mapArena);
  while(workHead){
    BLOCK_REGION(mapArena);

    Work* w = workHead;
    LL_PopFront(workHead,workTail,next);

    COM_Unit* unit = w->toProcess;

    // Maps from default graph to unit in new flattened graph.
    TrieMap<COM_Unit*,COM_Unit*>* unitMap = PushTrieMap<COM_Unit*,COM_Unit*>(mapArena);
    
    COM_Graph subGraph = unit->decl->baseGraph;
    COM_Unit* outputUnit = COM_GetOutputUnit(subGraph);
      
    for(COM_Unit* u = subGraph.units.head; u; u = u->next){
      COM_Unit* newUnit = COM_PushUnit(&graph,u->name,u->decl,out);
      unitMap->Insert(u,newUnit);

      if(DECL_IsComposite(newUnit->decl)){
        Work* w = PushStruct<Work>(temp);
        LL_Append(workHead,workTail,next,w);
      }
    }

    // Handle edges that have unit as input =======================================
    for(COM_Edge* outerEdge = graph.edges.head; outerEdge; outerEdge = outerEdge->next){
      if(outerEdge->in != unit){
        continue;
      }

      int inputIndex = outerEdge->inPort;

      for(COM_Edge* innerEdge = subGraph.edges.head; innerEdge; innerEdge = innerEdge->next){
        if(DECL_IsInput(innerEdge->out->decl) && innerEdge->out->special == inputIndex){
          outerEdge->general = MARK_TO_DELETE;

          COM_Edge* newEdge = COM_PushEdge(&graph,out);
          newEdge->out = outerEdge->out;
          newEdge->outPort = outerEdge->outPort;
          newEdge->in = unitMap->GetOrFail(innerEdge->in);
          newEdge->inPort = innerEdge->inPort;
          newEdge->delay = outerEdge->delay + innerEdge->delay;
        }
      }
    }

    // Handle edges that have unit as output ======================================
    for(COM_Edge* innerEdge = subGraph.edges.head; innerEdge; innerEdge = innerEdge->next){
      if(!DECL_IsOutput(innerEdge->in->decl)){
        continue;
      } 

      for(COM_Edge* outerEdge = graph.edges.head; outerEdge; outerEdge = outerEdge->next){
        if(outerEdge->out == unit){
          outerEdge->general = MARK_TO_DELETE;

          COM_Edge* newEdge = COM_PushEdge(&graph,out);
          newEdge->out = unitMap->GetOrFail(innerEdge->out);
          newEdge->outPort = innerEdge->outPort;
          newEdge->in = outerEdge->in;
          newEdge->inPort = outerEdge->inPort;
          newEdge->delay = outerEdge->delay + innerEdge->delay;
        }
      }
    }

    // Handle inner edges only ====================================================
    for(COM_Edge* innerEdge = subGraph.edges.head; innerEdge; innerEdge = innerEdge->next){
      if(DECL_IsInput(innerEdge->out->decl) || DECL_IsOutput(innerEdge->in->decl)){
        continue;
      }

      COM_Edge* newEdge = COM_PushEdge(&graph,out);
      newEdge->out = unitMap->GetOrFail(innerEdge->out);
      newEdge->outPort = innerEdge->outPort;
      newEdge->in = unitMap->GetOrFail(innerEdge->in);
      newEdge->inPort = innerEdge->inPort;
      newEdge->delay = innerEdge->delay;
    }

    // Remove edges marked to remove ==============================================
    {
      COM_Edge* next = 0;
      COM_Edge* prev = 0;
      for(COM_Edge* ptr = graph.edges.head; ptr; ptr = next){
        next = ptr;

        if(ptr->general == MARK_TO_DELETE){
          ptr->general = 0;
          LL_Remove(graph.edges.head,graph.edges.tail,next,prev,ptr);
          LL_PushNoTail(graph.freeEdges,next,ptr);
        } else {
          prev = ptr;
        }
      }
    }

    // Remove the unit ============================================================
    {
      COM_Unit* next = 0;
      COM_Unit* prev = 0;
      for(COM_Unit* ptr = graph.units.head; ptr; ptr = next){
        next = ptr;

        if(ptr == unit){
          LL_Remove(graph.units.head,graph.units.tail,next,prev,ptr);
          LL_PushNoTail(graph.freeUnits,next,ptr);
        } else {
          prev = ptr;
        }
      }
    }
  }

  return graph;

  // Unit 0 port A has edge to Unit X port B.
  // Unit X is composite and has edge from Input B to unit Y port D.
  // Flattened we get, unit 0 port A has edge to unit Y port D
  // Which means that we need to get the input edge.
  // Because we can have multiple edges from input to node, need to iterate inside edges as well.
  // Output is the same except the edges we iterate once are inside and the edges that can repeat are outside.

#if 0
  Pair<Accelerator*,AcceleratorMapping*> pair = CopyAcceleratorWithMapping(accel,AcceleratorPurpose_FLATTEN,true,temp);
  Accelerator* newAccel = pair.first;
  
  Pool<FUInstance*> compositeInstances = {};
  Pool<FUInstance*> toRemove = {};

  TrieMap<StaticId,int>* staticToIndex = PushTrieMap<StaticId,int>(temp);
  SubMap* subMappingDone = PushTrieMap<SubMappingInfo,PortInstance>(globalPermanent);
  
  for(int i = 0; i < times; i++){
    for(FUInstance* instPtr : newAccel->allocated){
      FUInstance* inst = instPtr;

      bool containsStatic = false;
      containsStatic = inst->isStatic || inst->declaration->staticUnits != nullptr;
      // TODO: For now we do not flatten units that are static or contain statics. It messes merge configurations and still do not know how to procceed. Need to beef merge up a bit before handling more complex cases. The problem was that after flattening statics would become shared (unions) and as such would appear in places where they where not supposed to appear.

      // TODO: Maybe static and shared units configurations could be stored in a separated structure.
      if(inst->declaration->baseCircuit && !containsStatic){
        FUInstance** ptr = compositeInstances.Alloc();

        *ptr = instPtr;
      }
    }

    if(compositeInstances.Size() == 0){
      break;
    }

    TrieMap<int,int>* sharedToFirstChildIndex = PushTrieMap<int,int>(temp);

    int freeSharedIndex = GetFreeShareIndex(newAccel); //(maxSharedIndex != -1 ? maxSharedIndex + 1 : 0);
    int count = 0;
    for(FUInstance** instPtr : compositeInstances){
      FUInstance* inst = (*instPtr);

      Assert(inst->declaration->baseCircuit);

      count += 1;
      Accelerator* circuit = inst->declaration->baseCircuit;
      FUInstance* outputInstance = GetOutputInstance(&circuit->allocated);

      int savedSharedIndex = freeSharedIndex;
      int instSharedIndex = freeSharedIndex;
      if(inst->sharedEnable){
        // Flattening a shared unit
        int* val = sharedToFirstChildIndex->Get(inst->sharedIndex);

        if(val){
          sharedToFirstChildIndex->Insert(inst->sharedIndex,freeSharedIndex);
        } else {
          freeSharedIndex = *val;
        }
      }

      TrieMap<int,int>* sharedToShared = PushTrieMap<int,int>(temp);
      //std::unordered_map<int,int> sharedToShared;
      // Create new instance and map then
      AcceleratorMapping* map = MappingSimple(circuit,newAccel,temp); // TODO: Leaking

      for(FUInstance* ptr : circuit->allocated){
        FUInstance* circuitInst = ptr;
        if(circuitInst->declaration->type == FUDeclarationType_SPECIAL){
          continue;
        }

        String newName = PushString(globalPermanent,"%.*s_%.*s",UN(inst->name),UN(circuitInst->name));

        // We are copying the instance but some differences are gonna happen in relation to static and config sharing. 
        FUInstance* newInst = CopyInstance(newAccel,circuitInst,true,newName);
        MappingInsertEqualNode(map,circuitInst,newInst);

        if(circuitInst->isStatic){
          bool found = false;
          int shareIndex = 0;
          for(auto iter : staticToIndex){
            if(iter.first.parent == inst->declaration && CompareString(iter.first.name,circuitInst->name)){
              found = true;
              shareIndex = iter.second;
              break;
            }
          }

          if(!found){
            shareIndex = GetFreeShareIndex(newAccel);

            StaticId id = {};
            id.name = circuitInst->name;
            id.parent = inst->declaration;
            staticToIndex->Insert(id,shareIndex);
          }

          ShareInstanceConfig(newInst,shareIndex);
        } else if(circuitInst->sharedEnable && inst->sharedEnable){
          int* val = sharedToShared->Get(circuitInst->sharedIndex);

          if(val){
            ShareInstanceConfig(newInst,*val);
          } else {
            int newIndex = GetFreeShareIndex(newAccel);

            sharedToShared->Insert(circuitInst->sharedIndex,newIndex);

            ShareInstanceConfig(newInst,newIndex);
          }
        } else if(inst->sharedEnable){ // Currently flattening instance is shared
          ShareInstanceConfig(newInst,instSharedIndex); // Always use the same inst index
        } else if(circuitInst->sharedEnable){
          int* val = sharedToShared->Get(circuitInst->sharedIndex);

          if(val){
            ShareInstanceConfig(newInst,*val);
          } else {
            int newIndex = freeSharedIndex;
            
            sharedToShared->Insert(circuitInst->sharedIndex,newIndex);

            ShareInstanceConfig(newInst,newIndex);
            freeSharedIndex = GetFreeShareIndex(newAccel);
          }
        }
      }

      if(inst->sharedEnable && savedSharedIndex > freeSharedIndex){
        freeSharedIndex = savedSharedIndex;
      }

      // TODO: All these iterations could be converted to a single loop where inside we check if its input,output or inernal edge
      // Add accel edges to output instances
      for(EdgeIterator iter = IterateEdges(newAccel); iter.IsValid(); iter.Next()){
        Edge edge = iter.Value();

        if(edge.units[0].inst == inst){
          for(EdgeIterator iter2 = IterateEdges(circuit); iter2.IsValid(); iter2.Next()){
            Edge circuitEdge = iter2.Value();

            if(circuitEdge.units[1].inst == outputInstance && circuitEdge.units[1].port == edge.units[0].port){
              FUInstance* other = MappingMapOutput(map,circuitEdge.units[0]).inst;

              if(other == nullptr){
                continue;
              }

              FUInstance* out = other;
              FUInstance* in = edge.units[1].inst;
              int outPort = circuitEdge.units[0].port;
              int inPort = edge.units[1].port;
              int delay = edge.delay + circuitEdge.delay;

              SubMappingInfo t = {};

              t.subDeclaration = inst->declaration;
              t.higherName = inst->name;
              t.isInput = false;
              t.subPort = circuitEdge.units[1].port;

              subMappingDone->Insert(t,{in,inPort});
              
              ConnectUnits(out,outPort,in,inPort,delay);
            }
          }
        }
      }

      // Add accel edges to input instances
      for(EdgeIterator iter = IterateEdges(newAccel); iter.IsValid(); iter.Next()){
        Edge edge = iter.Value();

        //Edge* edge = &edgeInst;
        if(edge.units[1].inst == inst){
          FUInstance* circuitInst = GetInputInstance(&circuit->allocated,edge.units[1].port);

          for(EdgeIterator iter2 = IterateEdges(circuit); iter2.IsValid(); iter2.Next()){
            Edge circuitEdge = iter2.Value();

            if(circuitEdge.units[0].inst == circuitInst){
              FUInstance* other = MappingMapInput(map,circuitEdge.units[1]).inst;

              if(other == nullptr){
                continue;
              }

              FUInstance* out = edge.units[0].inst;
              FUInstance* in = other;
              int outPort = edge.units[0].port;
              int inPort = circuitEdge.units[1].port;
              int delay = edge.delay + circuitEdge.delay;

              SubMappingInfo t = {};

              t.subDeclaration = inst->declaration;
              t.higherName = inst->name;
              t.isInput = true;
              t.subPort = edge.units[1].port;
              
              subMappingDone->Insert(t,{out,outPort});
              
              ConnectUnits(out,outPort,in,inPort,delay);
            }
          }
        }
      }

      // Add circuit specific edges
      for(EdgeIterator iter = IterateEdges(circuit); iter.IsValid(); iter.Next()){
        Edge edge = iter.Value();

        FUInstance* otherInput = MappingMapOutput(map,edge.units[0]).inst;
        FUInstance* otherOutput = MappingMapInput(map,edge.units[1]).inst;

        if(otherInput == nullptr || otherOutput == nullptr){
          continue;
        }

        FUInstance* out = otherInput;
        FUInstance* in = otherOutput;
        int outPort = edge.units[0].port;
        int inPort = edge.units[1].port;
        int delay = edge.delay;

        ConnectUnits(out,outPort,in,inPort,delay);
      }

      // Add input to output specific edges
      for(EdgeIterator iter = IterateEdges(newAccel); iter.IsValid(); iter.Next()){
        Edge edge1 = iter.Value();

        if(edge1.units[1].inst == inst){
          PortInstance input = edge1.units[0];
          FUInstance* circuitInput = GetInputInstance(&circuit->allocated,edge1.units[1].port);

          for(EdgeIterator iter2 = IterateEdges(newAccel); iter2.IsValid(); iter2.Next()){
            Edge edge2 = iter2.Value();

            if(edge2.units[0].inst == inst){
              PortInstance output = edge2.units[1];
              int outputPort = edge2.units[0].port;

              for(EdgeIterator iter3 = IterateEdges(newAccel); iter3.IsValid(); iter3.Next()){
                Edge circuitEdge = iter3.Value();

                if(circuitEdge.units[0].inst == circuitInput
                   && circuitEdge.units[1].inst == outputInstance
                   && circuitEdge.units[1].port == outputPort){
                  int delay = edge1.delay + circuitEdge.delay + edge2.delay;

                  ConnectUnits(input,output,delay);
                }
              }
            }
          }
        }
      }

      RemoveFUInstance(newAccel,*instPtr);
    }

    toRemove.Clear();
    compositeInstances.Clear();
  }

  toRemove.Clear(true);
  compositeInstances.Clear(true);

  newAccel->name = accel->name;
  
  return {newAccel,subMappingDone};
#endif
}
