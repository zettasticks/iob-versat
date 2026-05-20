#include "userConfigs.hpp"

#include "accelerator.hpp"
#include "globals.hpp"
#include "memory.hpp"
#include "parser.hpp"
#include "symbolic.hpp"
#include "declaration.hpp"
#include "utils.hpp"
#include "utilsCore.hpp"

#include "CEmitter.hpp"
#include "versatSpecificationParser.hpp"

readOnly ConfigFunction ConfigFunction_Nil = {};

// ============================================================================
// Instantiation and manipulation

static String GlobalConfigFunctionName(String functionName,FUDeclaration* decl,Arena* out){
  String name = PushString(out,"%.*s_%.*s",UN(decl->name),UN(functionName));
  return name;
}

struct DecompConfigStatement{
  // Single type
  bool isFunctionInvoc;
  Array<MathExpression*> args;
  ConfigFunction* func;

  // LHS type
  struct {
    bool isVirtualWire;
    bool isSingleWire;
    bool isEntityOnly;
    bool isArrayAccess;

    Entity entity; // Always contains the "bigger" entity (FUs and such).
    Entity subEntity; // Contains the smaller entity (wires and such).

    String name;
  } lhs;
  
  // RHS type
  struct {
    bool isHierAccess;
    bool isArray;
    bool isExpr;

    Entity entity;
    Entity subEntity; 

    // TODO: I think that this has to go. Entity + SubEntity is enough to provide all the info needed to user.
    SYM_Expr expr;
  } rhs;
};

DecompConfigStatement DecomposeConfigStatement(Env* env,ConfigStatement* stmt,Arena* out){
  DecompConfigStatement res = {};

  // TODO: While we currently do not support it, we could technically add:
  //       Multiple access expressions: a.b.c.d.e actually resolving to something (currently we do not support this).

  // TODO: Since the majority of the logic is specific to a certain size and format of the access, we could add more
  //       robust error checking and reporting.

  Array<Entity> lhs = env->GetEntity(stmt->lhs,out);

  Entity lhsFirst = Entity_Nil;
  Entity lhsSecondLast = Entity_Nil;
  Entity lhsLast = Entity_Nil;

  if(lhs.size >= 1){
    lhsFirst = lhs[0];
    lhsLast = lhs[lhs.size - 1];
  }
  if(lhs.size >= 2){
    lhsSecondLast = lhs[lhs.size - 2];
  }
  
  Assert(!IsLoop(stmt->type));
  if(stmt->type == ConfigStatementType_FUNCTION_CALL){
    res.isFunctionInvoc = true;
    res.func = lhsLast.func;
    res.args = stmt->lhs->next->arguments;
    res.lhs.name = lhsFirst.name.identifier;
  }

  if(stmt->type == ConfigStatementType_EQUALITY){
    // Left hand side
    res.lhs.name = lhsFirst.name.identifier;

    FULL_SWITCH(lhsLast.type){
    case EntityType_NIL:{
      // Nothing
      Assert(false);
    } break;

    case EntityType_VARIABLE_SPECIAL:
    case EntityType_VARIABLE_INPUT:
    case EntityType_FUNCTION:
    case EntityType_GEN_VALUE:
    case EntityType_PARAM:
    case EntityType_SYM:
    case EntityType_FU_ARRAY:{
      // TODO: Should be user error, right?
      //Assert(false);
    } break;

    case EntityType_FU:{
      res.lhs.isEntityOnly = true;
      res.lhs.name = lhsLast.name.identifier;
      res.lhs.entity = lhsLast;
    } break;
    case EntityType_ACCESS_EXPR:{
      res.lhs.isArrayAccess = true;
      res.lhs.entity = lhsSecondLast;
      res.lhs.subEntity = lhsLast;
    } break;
    case EntityType_MEM_PORT:{
      res.lhs.isVirtualWire = true;
      res.lhs.entity = lhsSecondLast;
      res.lhs.subEntity = lhsLast;
    } break;
    case EntityType_CONFIG_WIRE:
    case EntityType_STATE_WIRE:{
      res.lhs.isSingleWire = true;
      res.lhs.entity = lhsSecondLast;
      res.lhs.subEntity = lhsLast;
    } break;
  }

    // Right hand side
    MathExpression* rhs = stmt->rhs;
    Array<Entity> rhsDecomp = env->GetEntity(rhs,out);

    Entity rhsFirst = Entity_Nil;
    Entity rhsLast = Entity_Nil;

    if(rhsDecomp.size >= 1){
      rhsFirst = rhsDecomp[0];
    }
    if(rhsDecomp.size >= 2){
      rhsLast = rhsDecomp[rhsDecomp.size - 1];
    }

    res.rhs.entity = rhsFirst;
    res.rhs.subEntity = rhsLast;

    bool found = false;
    
    // TODO: The access expr stuff is really weird. 
    if(!found && rhsFirst.type == EntityType_ACCESS_EXPR){
      // TODO: Do not even know if we can reach this.
      found = true;
      res.rhs.isArray = true;
      res.rhs.entity = rhsFirst;
      res.rhs.expr = rhsFirst.sym;
    } 
    if(!found && rhsFirst.type == EntityType_FU && rhsLast.type == EntityType_ACCESS_EXPR){
      found = true;
      res.rhs.isArray = true;
    }
    if(!found && rhsFirst.type == EntityType_VARIABLE_INPUT && rhsLast.type == EntityType_ACCESS_EXPR){
      found = true;
      res.rhs.isArray = true;
      res.rhs.entity = rhsFirst;
      res.rhs.subEntity = rhsLast;
    }

    if(!found && rhsFirst.type == EntityType_FU
       && (rhsLast.type == EntityType_CONFIG_WIRE || 
           rhsLast.type == EntityType_STATE_WIRE)){
      // Statement of the form x = ent.wire
      // This is mostly state statements
      found = true;
      res.rhs.entity = rhsFirst;
      res.rhs.subEntity = rhsLast;
      res.rhs.isHierAccess = true;
    }

    if(!found && rhsFirst.type == EntityType_SYM){
      found = true;
      res.rhs.isExpr = true;
      res.rhs.entity = rhsFirst;
      res.rhs.expr = rhsFirst.sym;
    }

    if(rhsLast.type == EntityType_ACCESS_EXPR){
      res.rhs.expr = rhsLast.sym;
    }

    //Assert(found);
  }
  
  return res;
}

ConfigFunction* InstantiateConfigFunction(Env* env,ConfigFunctionDef* def,FUDeclaration* declaration,String content,Arena* out){
  TEMP_REGION(temp,out);

  auto list = PushList<ConfigStuff>(temp);
  ConfigFunctionType type = {};

  // TODO: Where is the error checking being perform to check if a given composite module contains the actual function?
  String structToReturnName = "void";
  Array<ConfigVarDeclaration> variables = def->variables;
  Array<Token> variableNames = Extract(variables,temp,&ConfigVarDeclaration::name);

  String stateStructContent = {};

  env->PushScope();

  for(ConfigVarDeclaration varDecl : variables){
    EntityVarFlags flags = {};
    if(varDecl.type.identifier == "Address"){
      flags = EntityVarFlags_ADDRESS;
    }

    env->AddVariable(varDecl.name,varDecl.arraySize,flags);
  }
  
  // TODO: This flow is not good. With a bit more work we probably can join state and config into the same flow or at least avoid duplicating work. For now we are mostly prototyping so gonna keep pushing what we have.
  // NOTE: We probably can join mem and config together. 

  // Break apart every for loop + statement into individual statements.
  // NOTE: Kinda slow but should not be a problem anytime soon.
  TrieMap<ConfigStatement*,ConfigStatement*>* nodeToParent = PushTrieMap<ConfigStatement*,ConfigStatement*>(temp);
  auto simpleStmtList = PushList<ConfigStatement*>(temp);

  auto Recurse = [nodeToParent,simpleStmtList](auto Recurse,ConfigStatement* top) -> void{
    if(IsLoop(top->type)){
      for(ConfigStatement* child : top->childs){
        nodeToParent->Insert(child,top);
        Recurse(Recurse,child);
      }
    }
    if(IsLeaf(top->type)){
      *simpleStmtList->PushElem() = top;
    }
  };

  for(ConfigStatement* top : def->statements){
    Recurse(Recurse,top);
  }

  auto stmtList = PushList<Array<ConfigStatement*>>(temp);
  for(ConfigStatement* stmt : simpleStmtList){
    auto list = PushList<ConfigStatement*>(temp);
    
    ConfigStatement* ptr = stmt;
    while(ptr){
      *list->PushElem() = ptr;

      ConfigStatement** possibleParent = nodeToParent->Get(ptr);
      ConfigStatement* parent = possibleParent ? *possibleParent : nullptr;

      ptr = parent;
    }

    Array<ConfigStatement*> asArray = PushArray(temp,list);
    ReverseInPlace(asArray);
    *stmtList->PushElem() = asArray;
  }

  // From this point on use this. Every N sized array is composed of N-1 FOR_LOOP types and 1 STATEMENT type.
  // TODO: Remember, after pushing every statement into an individual loop, we need to do error checking and check if the variable still exists. We cannot do variable checking globally since some statements might not be inside one of the loops.
  Array<Array<ConfigStatement*>> individualStatements = PushArray(temp,stmtList);

  // Pass all gen loops to outer loops.
  for(Array<ConfigStatement*>& individual : individualStatements){
    for(int i = 0; i < individual.size; i++){
      for(int j = 0; j < individual.size - 1; j++){
        if(   individual[j]->type == ConfigStatementType_FOR_LOOP
           && individual[j+1]->type == ConfigStatementType_GEN_LOOP){
          
          SWAP(individual[j],individual[j+1]);
        }
      }
    }
  }

  // TODO: Kinda stupid calculating things this way but the rest of the code needs to collapse into a simpler form for the more robust approach first.
  auto variablesUsedOnLoopExpressions = PushTrieSet<String>(temp);

  for(Array<ConfigStatement*> arr : individualStatements){
    for(ConfigStatement* conf : arr){
      if(!IsLoop(conf->type)){
        continue;
      }

      AddressGenForDef def = conf->def;

      {
        auto tokens = AccumTokens(def.startSym,temp);

        for(Token tok : tokens){
          variablesUsedOnLoopExpressions->Insert(tok.identifier); 
        }
      }

      {
        auto tokens = AccumTokens(def.endSym,temp);

        for(Token tok : tokens){
          variablesUsedOnLoopExpressions->Insert(tok.identifier); 
        }
      }
    }
  }

  Array<ConfigVariable> varInfo = PushArray<ConfigVariable>(out,def->variables.size);
  Array<String> varNames = PushArray<String>(out,def->variables.size); 
  for(int i = 0; i < variables.size; i++){
    ConfigVarDeclaration decl = variables[i];
    Token typeTok = decl.type;

    ConfigVarType type = ConfigVarType_SIMPLE;

    if(typeTok.identifier == "Address"){
      type = ConfigVarType_ADDRESS;
    } else if(typeTok.identifier == "Fixed"){
      type = ConfigVarType_FIXED;
    } else if(typeTok.identifier == "Dyn"){
      type = ConfigVarType_DYN;
    } else if(!Empty(typeTok.identifier)){
      // TODO: Proper error report, can only be one of three
      env->ReportError(typeTok,"Not a valid variable type: Must be one of: 'Address', 'Fixed' or 'Dyn'");
    }
    
    varInfo[i].type = type;
    varInfo[i].name = PushString(out,decl.name.identifier);
    
    if(decl.arraySize){
      varInfo[i].arraySize = env->CalculateConstantExpression(decl.arraySize);
    }

    varNames[i] = varInfo[i].name;
  }

  bool supportsSizeCalc = true;
  for(ConfigVariable var : varInfo){
    if(variablesUsedOnLoopExpressions->Exists(var.name)){
      if(var.type != ConfigVarType_FIXED && var.type != ConfigVarType_DYN){
        supportsSizeCalc = false;

        // TODO: Better error calculation.
        printf("[WARNING] UserConfig function \"%.*s\" does not support runtime size calculations because var \"%.*s\" which is part of loop logic is not defined as Fixed or Dyn\n",UN(def->name.identifier),UN(var.name));
      }
    }
  }

  if(def->type == UserConfigType_CONFIG){
    type = ConfigFunctionType_CONFIG;

    for(Array<ConfigStatement*> stmts : individualStatements){
      // Collect all gen loops into an "iterator" structures.
      // Iterate over that structure setting the gen variables to the expected value.

      // Separate loops for easier processing
      auto genList = PushList<AddressGenForDef>(temp);
      for(int i = 0; i < stmts.size - 1; i++){
        if(stmts[i]->type == ConfigStatementType_GEN_LOOP){
          *genList->PushElem() = stmts[i]->def;
        }
      }
      Array<AddressGenForDef> genLoops = PushArray(temp,genList);

      auto forList = PushList<AddressGenForDef>(temp);
      for(int i = 0; i < stmts.size - 1; i++){
        if(stmts[i]->type == ConfigStatementType_FOR_LOOP){
          *forList->PushElem() = stmts[i]->def;
        }
      }
      Array<AddressGenForDef> forLoops = PushArray(temp,forList);

      ConfigStatement* simple = stmts[stmts.size - 1];

      // Setup gen loop state
      struct GenLoopState{
        Token name;
        int val;
        int start;
        int end;
      };
      Array<GenLoopState> genState = PushArray<GenLoopState>(temp,genLoops.size);

      // We are now inside the loops scope.
      env->PushScope();
      for(int i = 0; i <  genLoops.size; i++){
        AddressGenForDef gen = genLoops[i];

        genState[i].name = gen.loopVariable;
        genState[i].start = env->CalculateConstantExpression(gen.startSym);
        genState[i].end = env->CalculateConstantExpression(gen.endSym);
        genState[i].val = genState[i].start;

        env->SetGenVariable(gen.loopVariable,genState[i].start);
      }

      for(AddressGenForDef loop : forLoops){
        env->AddVariable(loop.loopVariable);
      }

      while(1){
        // Register current loop variables on environment
        for(int i = 0; i <  genLoops.size; i++){
          GenLoopState gen = genState[i];

          env->SetGenVariable(gen.name,gen.val);
        }

        DecompConfigStatement decomp = DecomposeConfigStatement(env,simple,temp);

        String lhsName = decomp.lhs.name;

        Entity entity = decomp.lhs.entity;
        Entity wirePortEnt = decomp.lhs.subEntity;

        // Function invocation is basically argument instantiation and replacing the 
        // statements with the new version.
        if(decomp.isFunctionInvoc){
          Array<MathExpression*> args = decomp.args;
          ConfigFunction* function = decomp.func;

          // TODO: Not an assert, should be a proper error
          Assert(function->type == ConfigFunctionType_CONFIG);

          if(!function){
            // TODO: Error
            printf("Error 2, function does not exist, make sure the name is correct\n");
            exit(-1);
            return nullptr;
          }

          if(args.size != function->variables.size){
            printf("Error 2.1, number of arguments does not match\n");
            exit(-1);
            return nullptr;
          }
      
          // Invocation var to function argument
          TrieMap<String,SYM_Expr>* argToVar = PushTrieMap<String,SYM_Expr>(temp);
          for(int i = 0; i <  args.size; i++){
            // Arg is in the function space
            ConfigVariable arg = function->variables[i];

            SYM_Expr var = env->SymbolicFromMathExpression(args[i]);

            // TODO: Kinda stupid.
            Array<String> vars = SYM_GetAllVariables(var,temp);
            if(arg.usedOnLoopExpressions){
              for(String s : vars){
                variablesUsedOnLoopExpressions->Insert(s);
              }
            }

            argToVar->Insert(arg.name,var);
          }
      
          for(ConfigStuff stuff : function->stuff){
            // TODO: We are building the struct access expression in here but I got a feeling that we probably want to preserve data as much as possible in order to tackle merge later on.
            FULL_SWITCH(stuff.type){
            case ConfigStuffType_ASSIGNMENT:{
              String lhs = PushString(out,"%.*s.%.*s",UN(lhsName),UN(stuff.assign.lhs));
        
              SYM_Expr rhs = stuff.assign.rhs;
              SYM_Expr newExpr = SYM_Replace(rhs,argToVar);
        
              ConfigStuff* newAssign = list->PushElem();
              newAssign->type = ConfigStuffType_ASSIGNMENT;
              newAssign->assign.lhs = lhs;
              newAssign->assign.rhs = newExpr;
            } break;
            case ConfigStuffType_ADDRESS_GEN:{
              AccessAndType access = stuff.access;

              ConfigStuff* newAccess = list->PushElem();
              newAccess->type = ConfigStuffType_ADDRESS_GEN;

              // TODO: By doing stuff this way we do not allow expressions inside functions.
              //       We cannot have ent.func(expr + expr) for example since we assume that the expression
              //       inside is just simple substitution.
              newAccess->access = access;
              newAccess->access.access = ReplaceVariables(access.access,argToVar,varNames,out);
              newAccess->lhs = lhsName + stuff.lhs;
            } break;
            case ConfigStuffType_MEMORY_TRANSFER:{
              // We should never have memory transfers at config functions, right?
              NOT_IMPLEMENTED();
            } break;
          }
          }
        } else if(decomp.lhs.isSingleWire){
          // We are setting a value to a constant wire.
          String wireName = wirePortEnt.name.identifier;

          ConfigStuff* assign = list->PushElem();
          assign->type = ConfigStuffType_ASSIGNMENT;
          assign->assign.lhs = PushString(out,"%.*s.%.*s",UN(lhsName),UN(wireName));
          assign->assign.rhs = decomp.rhs.expr;
        } else if(decomp.lhs.isVirtualWire || decomp.rhs.isExpr || decomp.rhs.isArray){
          // Is Address gen expression. Including array accesses for VUnits
          AddressAccess* access = CompileAddressGen(env,variableNames,forLoops,decomp.rhs.expr,content);
          AddressGenInst supported = entity.decl->supportedAddressGen;

          // TODO: Need to check if the decomp expression matches what is actually supported by the entity.
          //       Ex: An expression of the form a = array[i] is not supported if 'a' is a Generator.
          ConfigStuff* newAssign = list->PushElem();

          newAssign->type = ConfigStuffType_ADDRESS_GEN;
          newAssign->access.access = access;
          newAssign->access.inst = supported;
          newAssign->access.dir = wirePortEnt.dir;
          newAssign->access.port = wirePortEnt.port;

          newAssign->pointerVarName = PushString(out,decomp.rhs.entity.name.identifier);
          newAssign->lhs = lhsName;
        } else {
          // Decomp failed. Error already reported.
          //Assert(false);
        }

        // Increment gen state in reverse order.
        bool didAllLoops = true;
        for(int i = genLoops.size - 1; i >= 0; i--){
          GenLoopState& gen = genState[i];

          gen.val += 1;
          if(gen.val >= gen.end){
            gen.val = gen.start;
            continue;
          }

          didAllLoops = false;
          break;
        }

        if(didAllLoops){
          break;
        }
      }
      env->PopScope();
    }
  }

  if(def->type == UserConfigType_STATE){
    type = ConfigFunctionType_STATE;

    CEmitter* c = StartCCode(temp);
    
    String structName = PushString(out,"%.*s_%.*s_Struct",UN(declaration->name),UN(def->name.identifier));
    structToReturnName = structName;

    c->Struct(structName);
    for(ConfigStatement* stmt : def->statements){
      // TODO: Remove this since we want to rely on the decomp stuff
      String name = GetBase(stmt->lhs)->name.identifier;
          
      String wireName = {};
      
      ConfigIdentifier* before = GetBeforeBase(stmt->lhs);
      if(before){
        wireName = before->name.identifier;
      }      

      // TODO: Proper error reporting. 
      Assert(Empty(wireName));

      c->Member("int",name);
    }
    c->EndStruct();

    stateStructContent = PushASTRepr(c,out,false);

    for(ConfigStatement* stmt : def->statements){
      env->PushScope();

      Entity ent = Entity_Nil;
      ent.type = EntityType_VARIABLE_SPECIAL;
      ent.name = stmt->lhs->name;
      env->PushEntity(ent.name,ent);

      DecompConfigStatement decomp = DecomposeConfigStatement(env,stmt,temp);

      String lhsName = decomp.lhs.name;
      Entity rhsMain = decomp.rhs.entity;
      Entity wireOrFunction = decomp.rhs.subEntity;

      bool found = false;
      if(wireOrFunction.type == EntityType_FUNCTION){
        found = true;

        String varName = rhsMain.name.identifier;
        
        for(ConfigStuff stmt : wireOrFunction.func->stuff){
          ConfigStuff* assign = list->PushElem();
          assign->type = ConfigStuffType_ASSIGNMENT;
          assign->assign.lhs = lhsName;
          assign->assign.rhsId = PushString(out,"%.*s.%.*s",UN(varName),UN(stmt.assign.rhsId));
        }      
      }

      if(wireOrFunction.type == EntityType_STATE_WIRE){
        found = true;

        String wireName = wireOrFunction.name.identifier;
        String varName = rhsMain.name.identifier;

        ConfigStuff* assign = list->PushElem();
        assign->type = ConfigStuffType_ASSIGNMENT;
        assign->assign.lhs = lhsName;
        assign->assign.rhsId = PushString(out,"%.*s.%.*s",UN(varName),UN(wireName));
      }

      if(!found){
        // TODO: Improved error messages
        printf("Error, did not find an expected type\n");
        ENTER_DEBUG();
      }

      env->PopScope();
    }
  }
  
  if(def->type == UserConfigType_MEM){
    type = ConfigFunctionType_MEM;

    for(Array<ConfigStatement*> stmts : individualStatements){
      env->PushScope();

      auto forList = PushList<AddressGenForDef>(temp);
      for(int i = 0; i < stmts.size - 1; i++){
        if(stmts[i]->type == ConfigStatementType_FOR_LOOP){
          *forList->PushElem() = stmts[i]->def;
        }
      }
      Array<AddressGenForDef> forLoops = PushArray(temp,forList);

      for(AddressGenForDef def : forLoops){
        env->AddVariable(def.loopVariable);
      }

      // TODO: We only support a single transfer loop.
      ConfigStatement* stmt = stmts[0];
      ConfigStatement* simple = stmts[stmts.size - 1];
      bool singleStatement = (stmts.size == 1);

      DecompConfigStatement decomp = DecomposeConfigStatement(env,simple,temp);

      if(decomp.isFunctionInvoc){
        ConfigFunction* func = decomp.func;

        // TODO: Not an assert, should be a proper error
        Assert(func->type == ConfigFunctionType_MEM);

        for(ConfigStuff stuff : func->stuff){
          ConfigStuff* assign = list->PushElem();
          assign->type = ConfigStuffType_MEMORY_TRANSFER;

          //nocheckin: This probably only currently works because variable have the same names
          // TODO: Need to create more complex tests to force the issue
          assign->transfer = stuff.transfer;
          assign->transfer.name = simple->lhs->name.identifier + assign->transfer.name;
        }
      } 
      
      Entity dst = Entity_Nil;
      Entity src = Entity_Nil;
      TransferDirection dir = TransferDirection_NONE;

      // TODO: Weird logic. Also need to see how this would act when we add arrays and stuff like that into the mix.
      if(decomp.lhs.entity.type == EntityType_FU){
        dir = TransferDirection_READ;
        dst = decomp.lhs.entity;
        src = decomp.rhs.entity;
      }
      if(decomp.rhs.entity.type == EntityType_FU){
        dir = TransferDirection_WRITE;
        dst = decomp.rhs.entity;
        src = decomp.lhs.entity;
      }

      if(dir != TransferDirection_NONE){
        SYM_Expr size = SYM_One;
        if(!singleStatement){
          SYM_Expr start = env->SymbolicFromMathExpression(stmt->def.startSym);
          SYM_Expr end = env->SymbolicFromMathExpression(stmt->def.endSym);

          size = end - start;
        }

        ConfigStuff* assign = list->PushElem();
        assign->type = ConfigStuffType_MEMORY_TRANSFER;
        assign->transfer.dir = dir;
        assign->transfer.size = size;
        assign->transfer.name = assign->transfer.name + dst.name.identifier;

        assign->transfer.variable = PushString(out,src.name.identifier);
      }

      env->PopScope();
    }
  }

  for(int i = 0; i < variables.size; i++){
    varInfo[i].usedOnLoopExpressions = variablesUsedOnLoopExpressions->Exists(varInfo[i].name);
  }

  ConfigFunction* func = PushStruct<ConfigFunction>(out);
  func->type = type;
  func->decl = declaration;
  func->stuff = PushArray(out,list);
  func->variables = varInfo;
  func->individualName = PushString(out,def->name.identifier);
  func->fullName = GlobalConfigFunctionName(func->individualName,declaration,out);
  func->structToReturnName = structToReturnName;
  func->stateStructContent = stateStructContent;
  func->debug = def->debug;
  func->supportsSizeCalc = supportsSizeCalc;

  env->PopScope();
  
  return func;  
}
