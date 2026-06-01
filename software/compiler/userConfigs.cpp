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
#include "templateEngine.hpp"

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

// TODO: While I think this was worthwhile, it is also not a good way of doing things. We started by 
//       trying to make the code generic instead of just doing the work that we actually need and then
//       compressing it back into a single portion.

// NOTE: For now we will keep this approach but eventually I want to remove this and do the proper logic in 
//       config state and mem portions; 
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

        bool isLhsWireAccess = false;
        bool isLhsWireVirtual = false;
        bool isLhsFunctionCall = false;
        bool lhsError = false;

        Entity lhsBase = Entity_Nil;
        Entity lhsSub = Entity_Nil;
        Array<MathExpression*> funcArgs = {};

        // Decompose lhs side =========================================================
        {
          bool nameAlreadySeen = false;

          ConfigIdentifier* ptr = simple->lhs;
          
          for(; ptr; ptr = ptr->next){
            FULL_SWITCH(ptr->type){
            case ConfigIdentifierType_BASE:{
              // Parser should never allow this
              Assert(!nameAlreadySeen);

              lhsBase = env->GetEntity(ptr->name);
              nameAlreadySeen = true;
            } break;
            case ConfigIdentifierType_ARRAY:{
              if(!Nil(lhsSub)){
                env->ReportError(lhsBase.name,"Cannot have array access expressions at this point");
                lhsError = true;
              }

              if(isLhsFunctionCall){
                env->ReportError({},"Cannot have array access after a function expression");
                lhsError = true;
              }

              MathExpression* expr = ptr->arrayExpr;

              bool found = false;
              if(!found && lhsBase.type == EntityType_FU_ARRAY){
                found = true;
        
                int index = env->CalculateConstantExpression(expr);
        
                if(index < 0 || index >= lhsBase.dims[0]){
                  env->ReportError({},"Outside array bounds");
                  lhsError = true;
                }

                String arrayName = PushString(out,"%.*s_%d",UN(lhsBase.name.identifier),index);
        
                Array<int> newDims = Offset(lhsBase.dims,1);

                if(newDims.size > 0){
                  lhsBase.type = EntityType_FU_ARRAY;
          
                  // TODO-2
                  lhsBase.name = {};
                  lhsBase.name.type = TokenType_IDENTIFIER;
                  lhsBase.name.identifier = arrayName;
                  lhsBase.name.originalData = arrayName;

                  lhsBase.dims = newDims;
                } else {
                  FUInstance** possibleInst = env->table->Get(arrayName);

                  if(!possibleInst){
                    //ReportError({},"Inst does not exist");
                  } else {
                    lhsBase = MakeEntity(*possibleInst);
                  }
                }
              }

              if(!found){
                env->ReportError(lhsBase.name,"Did not find entity referenced by this");
                lhsError = true;
              }
            } break;
            case ConfigIdentifierType_ACCESS:{
              isLhsWireAccess = true;

              Token accessName = ptr->name;
              String access = accessName.identifier;

              if(lhsBase.type != EntityType_FU){
                env->ReportError(accessName,"Trying to access entity that does not support member access");
              }

              if(lhsBase.type == EntityType_FU){
                lhsSub = env->GetEntityFromAccess(lhsBase,accessName);

                if(Nil(lhsSub)){
                  env->ReportError(accessName,"Not found");
                  lhsError = true;
                }
              }
              
              isLhsWireVirtual = (lhsSub.type == EntityType_MEM_PORT);
            } break;
            case ConfigIdentifierType_FUNC_CALL:{
              isLhsFunctionCall = true;

              Token funcName = ptr->functionName;
              FUDeclaration* decl = lhsBase.decl;

              bool found = false;
              for(MergePartition part : decl->info.infos){
                for(ConfigFunction* func : part.userFunctions){
                  if(func->individualName == funcName.identifier){
                    lhsSub.type = EntityType_FUNCTION;
                    lhsSub.func = func;
            
                    if(found){
                      // TODO-1
                      env->ReportError(funcName,"Multiple functions with same name detected\n");
                      lhsError = true;
                    }
                    found = true;
                  }
                }
              }
              
              if(found){
                funcArgs = ptr->arguments;
              }
            } break;
          }
          }
        }

        bool isRhsArrayAccess = false; // Entire thing is A[...];
        bool isRhsExpressionOnly = false; // Is only a mathematical expression.
        bool isRhsFunctionCall = false;
        bool isRangeSize = false;
        
        Entity rhsEntity = Entity_Nil;
        SYM_Expr rhsExpr = SYM_Nil;
        bool rhsError = false;
        MathExpression* rhsFunctionCall = {};

        // Decompose rhs side =========================================================
        if(!isLhsFunctionCall){
          MathExpression* ptr = simple->rhs;
          
          FULL_SWITCH(ptr->type){
          case MathType_FUNCTION_CALL:{
            if(ptr->name.identifier == "Range"){
              isRhsFunctionCall = true;
              rhsFunctionCall = ptr;
            }
            if(ptr->name.identifier == "RangeSize"){
              isRhsFunctionCall = true;
              rhsFunctionCall = ptr;
              isRangeSize = true;
            }

            if(!isRhsFunctionCall){
              rhsError = true;
            }
          } break;
          case MathType_NAME:
          case MathType_LITERAL:
          case MathType_OPERATION: {
            rhsExpr = env->SymbolicFromMathExpression(ptr);
            isRhsExpressionOnly = true;
          } break;

          // We do not support accesses on rhs of config functions ====================
          case MathType_SINGLE_ACCESS: {
            rhsError = true;
            Assert(false);
          } break;

          // Array accesses only supports constant expressions (for now) ================
          case MathType_ARRAY_ACCESS: {
            isRhsArrayAccess = true;

            DEBUG_BREAK();

            rhsEntity = env->GetEntity(ptr->name);

            for(int i = 0; i < ptr->expressions.size; i++){
              MathExpression* expr = ptr->expressions[i];

              FULL_SWITCH(expr->type){
              case MathType_FUNCTION_CALL: {
                if(expr->name.identifier == "Range"){
                  isRhsFunctionCall = true;
                  rhsFunctionCall = expr;
                }
                if(expr->name.identifier == "RangeSize"){
                  isRhsFunctionCall = true;
                  rhsFunctionCall = expr;
                  isRangeSize = true;
                }

                if(!isRhsFunctionCall){
                  rhsError = true;
                }
              } break;

              case MathType_NAME:
              case MathType_LITERAL:
              case MathType_OPERATION:{
                SYM_Expr asSym = env->SymbolicFromMathExpression(expr);
                SYM_EvaluateResult res = SYM_ConstantEvaluate(asSym);
                
                if(res.divByZero){
                  printf("Division by zero while trying to calculate array index\n");
                  rhsError = true;
                }

                int val = res.result;
                bool isExpressionConstant = !res.nonConstantValue;
                bool found = false;
                bool isExpression = false;
                Entity newEntity = Entity_Nil;
                
                if(!found && !rhsError){
                  bool isVarInput = rhsEntity.type == EntityType_VARIABLE_INPUT;
                  bool isAddress = (rhsEntity.flags & EntityVarFlags_ADDRESS);
                  bool isArray = rhsEntity.arrayDims > 0;

                  bool isVarInputAddressable = (isVarInput && isAddress && isArray);

                  if(!found && isExpressionConstant && isVarInputAddressable){
                    found = true;
                    String trueName = PushString(temp,"%.*s[%d]",UN(rhsEntity.name.identifier),val);
                    newEntity.type = EntityType_VARIABLE_INPUT;
                    newEntity.name.identifier = trueName;
                    newEntity.flags = rhsEntity.flags;
                    newEntity.arrayDims = rhsEntity.arrayDims - 1;
                  }

                  if(!found && (!isExpressionConstant || !isVarInputAddressable)){
                    if(!SYM_IsNil(rhsExpr)){
                      rhsError = true;
                    }
                    isExpression = true;
                    rhsExpr = asSym;
                  }
                }

                if(!found && !isExpression){
                  rhsError = true;
                }

                if(found){
                  rhsEntity = newEntity;
                }
              } break;
              case MathType_SINGLE_ACCESS:
              case MathType_ARRAY_ACCESS: {
                rhsError = true;
                //Assert(false);
              } break;

            }
            }
          } break;
        }
        }

        String lhsName = lhsBase.name.identifier;

        if(rhsError){
          printf("\n\n\nRHS decomp failed\n\n\n");
        }
          
        // Function invocation is basically argument instantiation and replacing the 
        // statements with the new version.
        if(isRhsFunctionCall){
          Array<MathExpression*> args = rhsFunctionCall->expressions;

/*
  The best way of doing this is to make a simple language that describes the things that need to happen on runtime.
  I basically need to describe the things that I want to happen at runtime.
*/

          MathExpression* start = args[0];
          MathExpression* end = args[1];
          MathExpression* unitCount = args[2];
          MathExpression* index = args[3];

          // Update the variables used.
          {
            auto tokens = AccumTokens(start,temp);
            for(Token tok : tokens){
              variablesUsedOnLoopExpressions->Insert(tok.identifier); 
            }
          }
          {
            auto tokens = AccumTokens(end,temp);
            for(Token tok : tokens){
              variablesUsedOnLoopExpressions->Insert(tok.identifier); 
            }
          }
          {
            auto tokens = AccumTokens(unitCount,temp);
            for(Token tok : tokens){
              variablesUsedOnLoopExpressions->Insert(tok.identifier); 
            }
          }
          {
            auto tokens = AccumTokens(index,temp);
            for(Token tok : tokens){
              variablesUsedOnLoopExpressions->Insert(tok.identifier); 
            }
          }

          SYM_Expr startSym = env->SymbolicFromMathExpression(start);
          SYM_Expr endSym = env->SymbolicFromMathExpression(end);
          SYM_Expr unitCountSym = env->SymbolicFromMathExpression(unitCount);
          SYM_Expr indexSym = env->SymbolicFromMathExpression(index);

          /*
            Ok, the current objective is to move as much as this logic to runtime first.
            Afterwards we can try to figure out how to proceeed.
           */

          AddressGenForDef def = {};

          static int d = 0;

          String values[3] = {
            PushString(out,"T%d",d++),
            PushString(out,"T%d",d++),
            PushString(out,"T%d",d++)
          };

          def.startSym = PushStruct<MathExpression>(temp);
          def.startSym->type = MathType_NAME;
          def.startSym->name.identifier = values[0]; // "loopStart";

          def.endSym = PushStruct<MathExpression>(temp);
          def.endSym->type = MathType_NAME;
          def.endSym->name.identifier = values[1]; // "loopEnd";

          if(isRangeSize){
            def.startSym->type = MathType_LITERAL;
            def.startSym->val = 0;
          }
          if(isRangeSize){
            def.endSym->name.identifier = values[2];
          }

          def.loopVariable.identifier = "_";

          Array<AddressGenForDef> loopArray = PushArray<AddressGenForDef>(temp,1);
          loopArray[0] = def;

          SYM_Expr varStuff = SYM_Var("_");

          Array<Token> variables = PushArray<Token>(temp,1);
          variables[0] = def.loopVariable;

          AddressAccess* access = CompileAddressGen(env,variables,loopArray,varStuff,{});
          AddressGenInst supported = lhsBase.decl->supportedAddressGen;

          // TODO: Need to check if the decomp expression matches what is actually supported by the entity.
          //       Ex: An expression of the form a = array[i] is not supported if 'a' is a Generator.
          ConfigStuff* newAssign = list->PushElem();

          newAssign->extra = true;
          newAssign->trueStart = startSym;
          newAssign->trueEnd = endSym;
          newAssign->unitCount = unitCountSym;
          newAssign->index = indexSym;

          String startRepr = SYM_Repr(startSym,temp);
          String endRepr = SYM_Repr(endSym,temp);
          String unitCountRepr = SYM_Repr(unitCountSym,temp);
          String indexRepr = SYM_Repr(indexSym,temp);

          String templ = PushString(out,R"FOO(
  int @{0};
  int @{1};
  int @{2};

{
  int trueStart = %.*s;
  int trueEnd = %.*s;
  int A = trueEnd - trueStart;
  int B = %.*s;
  int N = %.*s;
  int mod = A %% B;
  int size = N + 1 <= mod ? (A/B) + 1 : (A/B);
  int firstVal = mod >= (N+1) ?  N * size : N * size + mod;
  if(amount <= index){
    firstVal = 0;
  }
  if(size < 0){
    size = 0;
  }
  if(firstVal < 0){
    firstVal = 0;
  }
  @{0} = firstVal;
  @{1} = firstVal + size;
  @{2} = size;
}
                            )FOO",UN(startRepr),UN(endRepr),UN(unitCountRepr),UN(indexRepr));
          String fullTemplate = TE_Substitute(templ,values,out);
          newAssign->extraLoopStartAndEndTemplate = fullTemplate;

          newAssign->type = ConfigStuffType_ADDRESS_GEN;
          newAssign->access.access = access;
          newAssign->access.inst = supported;
          newAssign->access.dir = lhsSub.dir;
          newAssign->access.port = lhsSub.port;

          newAssign->pointerVarName = PushString(out,rhsEntity.name.identifier);
          newAssign->lhs = lhsName;

        } else if(isLhsFunctionCall){
          Array<MathExpression*> args = funcArgs;
          ConfigFunction* function = lhsSub.func;

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
        } else if(isLhsWireAccess && !isLhsWireVirtual){
          // We are setting a value to a constant wire.
          String wireName = lhsSub.name.identifier;

          ConfigStuff* assign = list->PushElem();
          assign->type = ConfigStuffType_ASSIGNMENT;
          assign->assign.lhs = PushString(out,"%.*s.%.*s",UN(lhsName),UN(wireName));
          assign->assign.rhs = rhsExpr;
        } else if(isLhsWireVirtual || isRhsExpressionOnly || isRhsArrayAccess){
          // Is Address gen expression. Including array accesses for VUnits
          AddressAccess* access = CompileAddressGen(env,variableNames,forLoops,rhsExpr,content);
          AddressGenInst supported = lhsBase.decl->supportedAddressGen;

          // TODO: Need to check if the decomp expression matches what is actually supported by the entity.
          //       Ex: An expression of the form a = array[i] is not supported if 'a' is a Generator.
          ConfigStuff* newAssign = list->PushElem();

          newAssign->type = ConfigStuffType_ADDRESS_GEN;
          newAssign->access.access = access;
          newAssign->access.inst = supported;
          newAssign->access.dir = lhsSub.dir;
          newAssign->access.port = lhsSub.port;

          newAssign->pointerVarName = PushString(out,rhsEntity.name.identifier);
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

    auto lhsSideMembers = PushList<Token>(temp);
    for(ConfigStatement* stmt : def->statements){

      // Decompose lhs side =========================================================
      ConfigIdentifier* id = stmt->lhs;
      Token lhsName = id->name;

      if(id->type != ConfigIdentifierType_BASE){
        env->ReportError(lhsName,"State expressions can only contain simple names on lhs");
      }

      for(Token alreadyExist : lhsSideMembers){
        if(lhsName.identifier == alreadyExist.identifier){
          env->ReportError(lhsName,"Cannot repeat name of state element");
        }
      }
      *lhsSideMembers->PushElem() = lhsName;

      // Decompose rhs side =========================================================
      Entity rhsEntity = Entity_Nil;
      Entity rhsSubEntity = Entity_Nil;
      bool rhsError = false;

      MathExpression* ptr = stmt->rhs;
      FULL_SWITCH(ptr->type){
      case MathType_NAME:
      case MathType_LITERAL:
      case MathType_OPERATION:{
        SYM_Expr sym = env->SymbolicFromMathExpression(ptr);

        rhsEntity.type = EntityType_SYM;
        rhsEntity.sym = sym;
      } break;

      case MathType_ARRAY_ACCESS: {
        rhsError = true;
      } break;

      case MathType_FUNCTION_CALL:
      case MathType_SINGLE_ACCESS:{
        MathExpression* shouldBeFU = ptr->expressions[0];
        
        FUAccess access = env->ResolveFU(shouldBeFU,temp);
        
        if(access.leftovers.size > 0){
          rhsError = true;
        }
        
        if(access.entity.type == EntityType_FU){
          rhsEntity = access.entity;
          rhsSubEntity = env->GetEntityFromAccess(rhsEntity,ptr->name);
        }
      } break;
      }

      bool found = false;
      if(rhsSubEntity.type == EntityType_FUNCTION){
        found = true;

        String varName = rhsEntity.name.identifier;
        
        for(ConfigStuff stmt : rhsSubEntity.func->stuff){
          ConfigStuff* assign = list->PushElem();
          assign->type = ConfigStuffType_ASSIGNMENT;
          assign->assign.lhs = lhsName.identifier;
          assign->assign.rhsId = PushString(out,"%.*s.%.*s",UN(varName),UN(stmt.assign.rhsId));
        }      
      }

      if(rhsSubEntity.type == EntityType_STATE_WIRE){
        found = true;

        String wireName = rhsSubEntity.name.identifier;
        String varName = rhsEntity.name.identifier;

        ConfigStuff* assign = list->PushElem();
        assign->type = ConfigStuffType_ASSIGNMENT;
        assign->assign.lhs = lhsName.identifier;
        assign->assign.rhsId = PushString(out,"%.*s.%.*s",UN(varName),UN(wireName));
      }

      if(rhsEntity.type == EntityType_SYM){
        found = true;

        ConfigStuff* assign = list->PushElem();
        assign->type = ConfigStuffType_ASSIGNMENT;
        assign->assign.lhs = lhsName.identifier;
        assign->assign.rhsId = SYM_Repr(rhsEntity.sym,out);
        assign->assign.noAccess = true;
      }

      if(!found){
        // TODO: Improved error messages
        printf("Error, did not find an expected type\n");
        ENTER_DEBUG();
      }
    }

    // Generate C struct ==========================================================
    CEmitter* c = StartCCode(temp);
    
    String structName = PushString(out,"%.*s_%.*s_Struct",UN(declaration->name),UN(def->name.identifier));
    structToReturnName = structName;

    c->Struct(structName);
    for(Token names : lhsSideMembers){
      c->Member("int",names.identifier);
    }
    c->EndStruct();

    stateStructContent = PushASTRepr(c,out,false);
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
