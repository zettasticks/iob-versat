#include "userConfigs.hpp"

#include "accelerator.hpp"
#include "globals.hpp"
#include "memory.hpp"
#include "newParser.hpp"
#include "symbolic.hpp"
#include "declaration.hpp"
#include "utils.hpp"
#include "utilsCore.hpp"

#include "CEmitter.hpp"
#include "versatSpecificationParser.hpp"

// TODO: Copied from spec parser, we probably want to move this somewhere and reconciliate everything.
// We probably want a common parsing file (different from parser.hpp) that contains all this logic instead of repeating or separating over multiple files.
static void ReportError3(String content,Token faultyToken,String error){
  TEMP_REGION(temp,nullptr);

  String loc = GetRichLocationError(content,faultyToken,temp);

  printf("[Error]\n");
  printf("%.*s:\n",UN(error));
  printf("%.*s\n",UN(loc));
  printf("\n");
}

static void ReportError(Tokenizer* tok,Token faultyToken,String error){
  String content = tok->GetContent();
  ReportError3(content,faultyToken,error);
}

static void ReportErrorIf(bool cond,String error){
  // TODO Proper error storage and report later handling.
  if(cond){
    printf("%.*s\n",UN(error));
  }
}

static bool _ExpectError(Tokenizer* tok,String expected){
  TEMP_REGION(temp,nullptr);

  Token got = tok->NextToken();
  if(!CompareString(got,expected)){
    
    auto builder = StartString(temp);
    builder->PushString("Parser Error.\n Expected to find:  '");
    builder->PushString(PushEscapedString(temp,expected,' '));
    builder->PushString("'\n");
    builder->PushString("  Got:");
    builder->PushString(PushEscapedString(temp,got,' '));
    builder->PushString("\n");
    String text = EndString(temp,builder);
    ReportError(tok,got,StaticFormat("%*s",UN(text))); \
    return true;
  }
  return false;
}

// Macro because we want to return as well
#define EXPECT(TOKENIZER,STR) \
  do{ \
    if(_ExpectError(TOKENIZER,STR)){ \
      return {}; \
    } \
  } while(0)

#define CHECK_IDENTIFIER(ID) \
  if(!IsIdentifier(ID)){ \
    ReportError(tok,ID,StaticFormat("type name '%.*s' is not a valid name",UN(ID))); \
    return {}; \
  }

bool IsNextTokenConfigFunctionStart(Parser* parser){
  bool res = false;

  res |= parser->IfPeekToken(NewTokenType_KEYWORD_CONFIG);
  res |= parser->IfPeekToken(NewTokenType_KEYWORD_STATE);
  res |= parser->IfPeekToken(NewTokenType_KEYWORD_MEM);
  
  return res;
}

// ======================================
// Globals

static TrieMap<String,ConfigFunction>* nameToFunction;
static Arena userConfigsArenaInst;
static Arena* userConfigsArena;

void InitializeUserConfigs(){
  userConfigsArenaInst = InitArena(Megabyte(1));
  userConfigsArena = &userConfigsArenaInst;

  nameToFunction = PushTrieMap<String,ConfigFunction>(userConfigsArena);
}

// ============================================================================
// Instantiation and manipulation

static FunctionInvocation* ParseFunctionInvocation(Array<Token> tokens,Arena* out){
  TEMP_REGION(temp,out);
  
  String functionName = {};

  #define CHECK_OR_ERROR(VAL) \
    if(CompareString(tokens[index],VAL)){ \
      index++; \
    } else { \
    } \
  
  int index = 0;

  if(IsIdentifier(tokens[index])){
    functionName = tokens[index++];
  } else {
    // TODO: Error
  }

  CHECK_OR_ERROR("(");
  
  auto list = PushList<Token>(temp);
  while(index < tokens.size){
    if(CompareString(tokens[index],",")){
      index++;
    }

    if(CompareString(tokens[index],")")){
      index++;
      break;
    }

    if(IsIdentifier(tokens[index])){
      *list->PushElem() = tokens[index];
      index++;
    }
  }

  if(index != tokens.size){
    // TODO: Error
  }

  FunctionInvocation* res = PushStruct<FunctionInvocation>(out);
  res->functionName = functionName;
  res->arguments = PushArray(out,list);
  return res;
}

static String GlobalConfigFunctionName(String functionName,FUDeclaration* decl,Arena* out){
  String name = PushString(out,"%.*s_%.*s",UN(decl->name),UN(functionName));
  return name;
}

struct ParseResult{
  bool isArray;
  bool isFunctionInvoc;
  bool containsAccess;
  bool isExpr;
  
  SYM_Expr expr;
  Array<SpecExpression*> args;
  Token entityName;
  Token wireName; 
  Token functionName;
};

struct DecompConfigStatement{
  bool isArray;
  bool isFunctionInvoc;
  bool containsAccess;
  bool isExpr;
  
  SYM_Expr expr;
  Array<SpecExpression*> args;
  Token entityName;
  Token wireName; 

  ConfigFunction* func;
};

DecompConfigStatement DecomposeConfigStatement(Env* env,ConfigStatement* stmt,Arena* out){
  DecompConfigStatement res = {};
  
  Assert(stmt->type != ConfigStatementType_FOR_LOOP);
  if(stmt->type == ConfigStatementType_FUNCTION_CALL){
    res.isFunctionInvoc = true;
    
    Entity* ent = env->GetEntity(stmt->lhs,out);
    
    res.func = ent->func;
    res.args = stmt->lhs->parent->arguments;
  }
  if(stmt->type == ConfigStatementType_EQUALITY){
    SpecExpression* top = stmt->rhs;

    if(top->type == SpecType_ARRAY_ACCESS){
      res.isArray = true;
      res.expr = SymbolicFromSpecExpression(top->expressions[0]);
      res.entityName = top->name;
    } else if(top->type == SpecType_SINGLE_ACCESS){
      res.containsAccess = true;
      res.entityName = top->name;
      res.wireName = top->expressions[0]->name;
    } else {    
      res.isExpr = true;
      res.expr = SymbolicFromSpecExpression(top);
    }
  }
  
  return res;
}

// TODO: We could do better. We could have a decompose ConfigStatement
ParseResult ParseRHS(Env* env,SpecExpression* top,Arena* out){
/*
  RHS can be:

  wire           'name.wire'
  addrExpr       'addr[expr]'
  expression     'expr'
*/

  // TODO: We still need to access env to check if this makes sense or not.
  //       nocheckin

  ParseResult res = {};
  if(top->type == SpecType_ARRAY_ACCESS){
    res.isArray = true;
    res.expr = SymbolicFromSpecExpression(top->expressions[0]);
    res.entityName = top->name;
  } else if(top->type == SpecType_SINGLE_ACCESS){
    res.containsAccess = true;
    res.entityName = top->name;
    res.wireName = top->expressions[0]->name;
  } else {    
    res.isExpr = true;
    res.expr = SymbolicFromSpecExpression(top);
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

  env->PushScope();

  for(Token name : variableNames){
    env->AddVariable(name);
  }
  
  // TODO: This flow is not good. With a bit more work we probably can join state and config into the same flow or at least avoid duplicating work. For now we are mostly prototyping so gonna keep pushing what we have.

  // Break apart every for loop + statement into individual statements.
  // NOTE: Kinda slow but should not be a problem anytime soon.
  TrieMap<ConfigStatement*,ConfigStatement*>* nodeToParent = PushTrieMap<ConfigStatement*,ConfigStatement*>(temp);
  auto simpleStmtList = PushList<ConfigStatement*>(temp);

  auto Recurse = [nodeToParent,simpleStmtList](auto Recurse,ConfigStatement* top) -> void{
    if(top->type == ConfigStatementType_FOR_LOOP){
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

  // TODO: Kinda stupid calculating things this way but the rest of the code needs to collapse into a simpler form for the more robust approach first.
  auto variablesUsedOnLoopExpressions = PushTrieSet<String>(temp);

  for(Array<ConfigStatement*> arr : individualStatements){
    for(ConfigStatement* conf : arr){
      if(conf->type != ConfigStatementType_FOR_LOOP){
        continue;
      }

      AddressGenForDef def = conf->def;

      {
        auto tokens = AccumTokens(def.startSym,temp);

        for(Token tok : tokens){
          if(IsIdentifier(tok)){
            variablesUsedOnLoopExpressions->Insert(tok); 
          }
        }
      }

      {
        auto tokens = AccumTokens(def.endSym,temp);

        for(Token tok : tokens){
          if(IsIdentifier(tok)){
            variablesUsedOnLoopExpressions->Insert(tok); 
          }
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
    if(CompareString(typeTok,"Address")){
      type = ConfigVarType_ADDRESS;
    } else if(CompareString(typeTok,"Fixed")){
      type = ConfigVarType_FIXED;
    } else if(CompareString(typeTok,"Dyn")){
      type = ConfigVarType_DYN;
    } else {
      // TODO: Proper error report, can only be one of three
      env->ReportError(typeTok,"Not a valid variable type");
    }
    
    varInfo[i].type = type;
    varInfo[i].name = PushString(out,decl.name);

    varNames[i] = varInfo[i].name;
  }

  bool supportsSizeCalc = true;
  for(ConfigVariable var : varInfo){
    if(variablesUsedOnLoopExpressions->Exists(var.name)){
      if(var.type != ConfigVarType_FIXED && var.type != ConfigVarType_DYN){
        supportsSizeCalc = false;

        // TODO: Better error calculation.
        printf("[WARNING] UserConfig function \"%.*s\" does not support runtime size calculations because var \"%.*s\" which is part of loop logic is not defined as Fixed or Dyn\n",UN(def->name),UN(var.name));
      }
    }
  }

  if(def->type == UserConfigType_CONFIG){
    type = ConfigFunctionType_CONFIG;

    for(Array<ConfigStatement*> stmts : individualStatements){
      ConfigStatement* simple = stmts[stmts.size - 1];
      // TODO: Call entity function to make sure that the entity exists and it is a config wire

      Token name = GetBase(simple->lhs)->name;

      auto forLoops = PushList<AddressGenForDef>(temp);

      for(int i = 0; i < stmts.size - 1; i++){
        *forLoops->PushElem() = stmts[i]->def;
      }

      Array<AddressGenForDef> loops = PushArray(temp,forLoops);

      DecompConfigStatement decomp = DecomposeConfigStatement(env,simple,temp);

      // MARK

      Entity* ent = env->GetEntity(simple->lhs,temp);

      Entity* portEnt = nullptr;
      if(ent->type == EntityType_MEM_PORT){
        portEnt = ent;
        ent = ent->parent;
      }

      Entity* wirePort = nullptr;
      if(ent->type == EntityType_CONFIG_WIRE){
        wirePort = ent;
        ent = ent->parent;
      }

      if(decomp.isFunctionInvoc){
        Array<SpecExpression*> args = decomp.args;

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

          SYM_Expr var = SymbolicFromSpecExpression(args[i]);

          // TODO: Kinda stupid.
          Array<String> vars = SYM_GetAllVariables(var,temp);
          if(arg.usedOnLoopExpressions){
            for(String s : vars){
              variablesUsedOnLoopExpressions->Insert(s);
            }
          }

          argToVar->Insert(arg.name,var);
        }
        
        String name = GetBase(simple->lhs)->name;
      
        for(ConfigStuff stuff : function->stuff){
          // TODO: We are building the struct access expression in here but I got a feeling that we probably want to preserve data as much as possible in order to tackle merge later on.
          FULL_SWITCH(stuff.type){
          case ConfigStuffType_ASSIGNMENT:{
            String lhs = PushString(out,"%.*s.%.*s",UN(name),UN(stuff.assign.lhs));
        
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

            newAccess->access = access;
            newAccess->access.access = ReplaceVariables(access.access,argToVar,varNames,out);

            String lhs = PushString(out,"%.*s.%.*s",UN(name),UN(stuff.lhs));

            newAccess->lhs = lhs;
            newAccess->hierLhs = Add(name,stuff.hierLhs,out);
          } break;
          case ConfigStuffType_MEMORY_TRANSFER:{
            // We should never have memory transfers at config functions, right?
            NOT_IMPLEMENTED();
          } break;
        }
        }
      } else {
        ParseResult parsedRhs = ParseRHS(env,simple->rhs,temp);

        AddressAccess* access = nullptr;

        if(parsedRhs.isExpr || parsedRhs.isArray){
          access = CompileAddressGen(variableNames,loops,parsedRhs.expr,content);
        }
        AddressGenInst supported = ent->instance->declaration->supportedAddressGen;

        // TODO: This logic is stupid. Rework into something better when we finalize the addressGen change.
        if(wirePort){
          String name = GetBase(simple->lhs)->name;
          
          ConfigIdentifier* before = GetBeforeBase(simple->lhs);
          String wireName = before->name;

          ConfigStuff* assign = list->PushElem();
          assign->type = ConfigStuffType_ASSIGNMENT;
          assign->assign.lhs = PushString(out,"%.*s.%.*s",UN(name),UN(wireName));
          assign->assign.rhs = parsedRhs.expr;
        } else if(supported.type == AddressGenType_GEN){
          ConfigStuff* newAssign = list->PushElem();

          newAssign->type = ConfigStuffType_ADDRESS_GEN;
          newAssign->access.access = access;
          newAssign->access.inst = supported;

          newAssign->lhs = PushString(out,name);
          newAssign->hierLhs = Add(newAssign->hierLhs,name,out);
        } else if(parsedRhs.isExpr || parsedRhs.isArray){
          // NOTE: Memories and Generator do not follow the addr[expr]. They just have <instance> = <expr>.
          ConfigStuff* newAssign = list->PushElem();

          newAssign->type = ConfigStuffType_ADDRESS_GEN;
          newAssign->access.access = access;
          newAssign->access.inst = supported;

          if(portEnt){
            newAssign->access.dir = portEnt->dir;
            newAssign->access.port = portEnt->port;
          }

          newAssign->accessVariableName = PushString(out,parsedRhs.entityName);
          newAssign->lhs = PushString(out,name);
          newAssign->hierLhs = Add(newAssign->hierLhs,name,out);
        } else {
          ConfigStuff* newAssign = list->PushElem();

          newAssign->type = ConfigStuffType_ADDRESS_GEN;
          newAssign->access.access = access;
          newAssign->access.inst = supported;
          newAssign->accessVariableName = PushString(out,parsedRhs.entityName);
          newAssign->lhs = PushString(out,name);
          newAssign->hierLhs = Add(newAssign->hierLhs,name,out);
        }
      }
    }
  }

  auto newStructs = PushList<String>(temp);

  if(def->type == UserConfigType_STATE){
    type = ConfigFunctionType_STATE;

    CEmitter* c = StartCCode(temp);
    
    String structName = PushString(out,"%.*s_%.*s_Struct",UN(declaration->name),UN(def->name));
    structToReturnName = structName;

    c->Struct(structName);
    for(ConfigStatement* stmt : def->statements){
      String name = GetBase(stmt->lhs)->name;
          
      String wireName = {};
      
      ConfigIdentifier* before = GetBeforeBase(stmt->lhs);
      if(before){
        wireName = before->name;
      }      

      // TODO: Proper error reporting. 
      Assert(Empty(wireName));

      c->Member("int",name);
    }
    c->EndStruct();

    *newStructs->PushElem() = PushASTRepr(c,out,false);

    for(ConfigStatement* stmt : def->statements){
      String name = GetBase(stmt->lhs)->name;

      String wireName = {};
      
      ConfigIdentifier* before = GetBeforeBase(stmt->lhs);
      if(before){
        wireName = before->name;
      }

      ConfigIdentifier id = {};

      if(stmt->rhs->type == SpecType_SINGLE_ACCESS){
        id.name = stmt->rhs->name;
      }

      Entity* ent = env->GetEntity(stmt->rhs,temp);

      ParseResult parsedRhs = ParseRHS(env,stmt->rhs,temp);

      if(ent->type == EntityType_CONFIG_FUNCTION){
        String varName = parsedRhs.entityName;
        
        for(ConfigStuff stmt : ent->func->stuff){
          ConfigStuff* assign = list->PushElem();
          assign->type = ConfigStuffType_ASSIGNMENT;
          assign->assign.lhs = name;
          assign->assign.rhsId = PushString(out,"%.*s.%.*s",UN(varName),UN(stmt.assign.rhsId));
        }      
      } else if(parsedRhs.containsAccess){
        ConfigStuff* assign = list->PushElem();
          
        String wireName = parsedRhs.wireName;
        String varName = parsedRhs.entityName;

        assign->type = ConfigStuffType_ASSIGNMENT;
        assign->assign.lhs = name;
        assign->assign.rhsId = PushString(out,"%.*s.%.*s",UN(varName),UN(wireName));
      }
    }
  }
  
  if(def->type == UserConfigType_MEM){
    type = ConfigFunctionType_MEM;

    for(Array<ConfigStatement*> stmts : individualStatements){
      ConfigStatement* stmt = stmts[0];
      ConfigStatement* simple = stmts[stmts.size - 1];

      bool singleStatement = (stmts.size == 1);
      
      // We currently only support a single statement or a single loop.
      // We technically can do multi loops just fine, just need to augment the logic to support it.
      Assert(singleStatement || (stmt->type == ConfigStatementType_FOR_LOOP 
                              && IsLeaf(simple->type)));

      DecompConfigStatement decomp = DecomposeConfigStatement(env,simple,temp);
      
      if(decomp.isFunctionInvoc){
        ConfigFunction* func = decomp.func;

        // TODO: Not an assert, should be a proper error
        Assert(func->type == ConfigFunctionType_MEM);

        for(ConfigStuff stuff : func->stuff){
          ConfigStuff* assign = list->PushElem();
          assign->type = ConfigStuffType_MEMORY_TRANSFER;

          // nocheckin: This probably only currently works because variable have the same names
          // TODO: Need to create more complex tests to force the issue
          assign->transfer = stuff.transfer;
          assign->transfer.hierEntity = Add(simple->lhs->name,assign->transfer.hierEntity,out);
        }
      } else {
        ParseResult parsedRhs = ParseRHS(env,simple->rhs,temp);

        Assert(parsedRhs.isArray);

        Entity* left = env->GetEntity(simple->lhs,temp);
        Entity* right = env->GetEntity(parsedRhs.entityName);

        Entity* unit = nullptr;
        Entity* addrVar = nullptr;

        TransferDirection dir = TransferDirection_NONE;
        if(left->type == EntityType_FU){
          Assert(right->type == EntityType_VARIABLE_INPUT);
          dir = TransferDirection_READ;

          unit = left;
          addrVar = right;
        } else {
          Assert(left->type == EntityType_VARIABLE_INPUT);
          dir = TransferDirection_WRITE;

          unit = right;
          addrVar = left;
        }

        SYM_Expr size = SYM_One;
        if(!singleStatement){
          SYM_Expr start = SymbolicFromSpecExpression(stmt->def.startSym);
          SYM_Expr end = SymbolicFromSpecExpression(stmt->def.endSym);

          size = end - start;
        }

        ConfigStuff* assign = list->PushElem();
        assign->type = ConfigStuffType_MEMORY_TRANSFER;
        assign->transfer.dir = dir;
        assign->transfer.size = size;
        assign->transfer.hierEntity = Add(assign->transfer.hierEntity,unit->instance->name,out);

        assign->transfer.variable = PushString(out,addrVar->varName);
      }
    }
  }

  for(int i = 0; i < variables.size; i++){
    varInfo[i].usedOnLoopExpressions = variablesUsedOnLoopExpressions->Exists(varInfo[i].name);
  }

  ConfigFunction func = {};
  func.type = type;
  func.decl = declaration;
  func.stuff = PushArray(out,list);
  func.variables = varInfo;
  func.individualName = PushString(out,def->name);
  func.fullName = GlobalConfigFunctionName(func.individualName,declaration,out);
  func.newStructs = PushArray(out,newStructs);
  func.structToReturnName = structToReturnName;
  func.debug = def->debug;
  func.supportsSizeCalc = supportsSizeCalc;

  ConfigFunction* res = nameToFunction->Insert(func.fullName,func);

  env->PopScope();
  
  return res;  
}
