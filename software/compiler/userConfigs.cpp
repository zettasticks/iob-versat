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

static ConfigIdentifier* ParseConfigIdentifier(Tokenizer* tok,Arena* out){
  Token id = tok->PeekToken();
  CHECK_IDENTIFIER(id);
  tok->AdvancePeek();
  
  ConfigIdentifier* base = PushStruct<ConfigIdentifier>(out);
  base->type = ConfigAccessType_BASE;
  base->name = id;

  ConfigIdentifier* ptr = base;

  while(!tok->Done()){
    ConfigIdentifier* parsed = nullptr;

    if(!parsed && tok->IfNextToken(".")){
      Token access = tok->NextToken();
      CHECK_IDENTIFIER(access);

      parsed = PushStruct<ConfigIdentifier>(out);
      parsed->type = ConfigAccessType_ACCESS;
      parsed->name = access;
    }

    if(!parsed && tok->IfNextToken("[")){
      SymbolicExpression* expr = ParseSymbolicExpression(tok,out);
      PROPAGATE(expr);

      EXPECT(tok,"]");

      parsed = PushStruct<ConfigIdentifier>(out);
      parsed->type = ConfigAccessType_ARRAY;
      parsed->expr = expr;
    }

    if(parsed){
      ptr->parent = parsed;
      ptr = parsed;
    }

    break;
  }

  return base;
}

static Opt<ConfigStatement*> ParseConfigStatement(Tokenizer* tok,Arena* out){
  TEMP_REGION(temp,out);
  
  Token peek = tok->PeekToken();

  ConfigStatement* stmt = PushStruct<ConfigStatement>(out);

  if(CompareString(peek,"for")){
    tok->AdvancePeek();
      
    Token loopVariable = tok->NextToken();
    CHECK_IDENTIFIER(loopVariable);
      
    Array<Token> startSym = TokenizeSymbolicExpression(tok,out);
    if(Empty(startSym)){
      return {};
    }
      
    EXPECT(tok,"..");

    Array<Token> endSym = TokenizeSymbolicExpression(tok,out);
    if(Empty(endSym)){
      return {};
    }

    EXPECT(tok,"{");
      
    //stmt->def = (AddressGenForDef){.loopVariable = loopVariable,.startSym = startSym,.endSym = endSym};
    
    auto list = PushArenaList<ConfigStatement*>(temp);
    while(!tok->Done()){
      Opt<ConfigStatement*> child = ParseConfigStatement(tok,out);
      PROPAGATE(child);

      *list->PushElem() = child.value();

      if(tok->IfPeekToken("}")){
        break;
      }
    }

    stmt->childs = PushArrayFromList(out,list);
    stmt->type = ConfigStatementType_FOR_LOOP;

    EXPECT(tok,"}");
  } else {
    // Assignment
    ConfigIdentifier* lhs = ParseConfigIdentifier(tok,out);
    PROPAGATE(lhs);
    EXPECT(tok,"=");

    stmt->lhs = lhs;

#if 0
    if(IsIdentifier(tok->PeekToken(0)) && tok->PeekToken(1) == "."){
      // Parse identifier only. Cannot be symbolic expression.
      ConfigIdentifier* rhs = ParseConfigIdentifier(tok,out);
      PROPAGATE(rhs);
        
      stmt->rhsId = rhs;
      stmt->rhsType = ConfigRHSType_IDENTIFIER;
    } else if(IsIdentifier(tok->PeekToken(0)) && tok->PeekToken(1) == "("){
      // Parse function call only. Cannot be symbolic expression
      String functionName = tok->NextToken();
      tok->AdvancePeek();

      auto list = PushArenaList<Token>(temp);
      while(!tok->Done()){
        tok->IfNextToken(",");

        if(tok->IfNextToken(")")){
          break;
        }

        Token var = tok->NextToken();
        if(IsIdentifier(var)){
          *list->PushElem() = var;
        }
      }
        
      FunctionInvocation* func = PushStruct<FunctionInvocation>(out);
      func->functionName = functionName;
      func->arguments = PushArrayFromList(out,list);

      stmt->func = func;
      stmt->rhsType = ConfigRHSType_FUNCTION_CALL;
    } else if(IsIdentifier(tok->PeekToken(0)) && tok->PeekToken(1) == "["){
      // For 
      ConfigIdentifier* rhs = ParseConfigIdentifier(tok,out);
      PROPAGATE(rhs);
        
      stmt->rhsId = rhs;
      stmt->rhsType = ConfigRHSType_IDENTIFIER;
    } else {
      SymbolicExpression* expr = ParseSymbolicExpression(tok,out);
      
      stmt->expr = expr;
      // TODO: Because we are trying to fit everything into the symbolic expression parsing stuff, we are making the code worse than it needs to be.
      //       We need to parse an expression. That expression can contain anything. If can be addr[expr], expr.expr, expr only and so on. Only on the instantiate function do we actually look and see if what we have is an addr access, a symbolic expression, a simple wire access and so on.
      //       Basically, we are trying to do more on this stage than we actually want to.
      stmt->rhsType = ConfigRHSType_SYMBOLIC_EXPR;
    }
#endif

    EXPECT(tok,";");
    
    stmt->type = ConfigStatementType_STATEMENT;
  }

  return stmt;
}

// TODO: Copy from versatSpec. This function at the very least needs to be moved into a more common place.
//       Furthermore we can make this more generic if we take an enum flag that indicates the type of numbers accepted, if needed.
static Opt<int> ParseNumber(Tokenizer* tok){
  // TODO: We only handle integers, for now.
  Token number = tok->NextToken();

  bool negate = false;
  if(CompareString(number,"-")){
    negate = true;
    number = tok->NextToken();
  }

  for(int i = 0; i < number.size; i++){
    if(!(number[i] >= '0' && number[i] <= '9')){
      ReportError(tok,number,StaticFormat("%.*s is not a valid number",UN(number)));
      return {};
    }
  }

  int res = ParseInt(number);
  if(negate){
    res = -res;
  }
  
  return res;
}

static Opt<ConfigVarDeclaration> ParseConfigVarDeclaration(Tokenizer* tok){
  ConfigVarDeclaration res = {};

  res.name = tok->NextToken();
  CHECK_IDENTIFIER(res.name);

  Token peek = tok->PeekToken();
  if(CompareString(peek,"[")){
    tok->AdvancePeek();

    Opt<int> number = ParseNumber(tok);
    PROPAGATE(number);
    int arraySize = number.value();

    EXPECT(tok,"]");

    res.arraySize = arraySize;
    res.isArray = true;
  }

  Token type = {};
  if(tok->IfNextToken(":")){
    type = tok->NextToken();
  }

  res.type = type;
  
  return res;
}

static Opt<Array<ConfigVarDeclaration>> ParseConfigFunctionArguments(Tokenizer* tok,Arena* out){
  auto array = StartArray<ConfigVarDeclaration>(out);

  EXPECT(tok,"(");

  if(tok->IfNextToken(")")){
    return EndArray(array);
  }
  
  while(!tok->Done()){
    Opt<ConfigVarDeclaration> var = ParseConfigVarDeclaration(tok);
    PROPAGATE(var);

    *array.PushElem() = var.value();
    
    if(tok->IfNextToken(",")){
      continue;
    } else {
      break;
    }
  }

  EXPECT(tok,")");

  return EndArray(array);
}

ConfigFunctionDef* ParseConfigFunction(Tokenizer* tok,Arena* out){
  TEMP_REGION(temp,out);

  UserConfigType type = UserConfigType_NONE;
  if(tok->IfNextToken("config")){
    type = UserConfigType_CONFIG;
  } else if(tok->IfNextToken("mem")){
    type = UserConfigType_MEM;
  } else if(tok->IfNextToken("state")){
    type = UserConfigType_STATE;
  }

  if(type == UserConfigType_NONE){
    return nullptr;
  }

  bool debug = tok->IfNextToken("debug");
    
  Token configName = tok->NextToken();
  CHECK_IDENTIFIER(configName);

  Opt<Array<ConfigVarDeclaration>> functionVars = {};
  if(type == UserConfigType_MEM || type == UserConfigType_CONFIG){
    functionVars = ParseConfigFunctionArguments(tok,out);
    PROPAGATE(functionVars);
  }

  EXPECT(tok,"{");

  auto stmts = PushArenaList<ConfigStatement*>(temp);
  while(!tok->Done()){
    Token peek = tok->PeekToken();

    if(CompareString(peek,";")){
      tok->AdvancePeek();
      continue;
    }
    if(CompareString(peek,"}")){
      break;
    }
        
    Opt<ConfigStatement*> config = ParseConfigStatement(tok,out);
    PROPAGATE(config);

    *stmts->PushElem() = config.value();
  }

  EXPECT(tok,"}");

  ConfigFunctionDef* res = PushStruct<ConfigFunctionDef>(out);
  res->type = type;
  res->name = configName;
  if(functionVars.has_value()){
    res->variables = functionVars.value();
  }
  res->statements = PushArrayFromList(out,stmts);
  res->debug = debug;

  return res;
}

bool IsNextTokenConfigFunctionStart(Parser* parser){
  bool res = false;

  res |= parser->IfPeekToken(NewTokenType_KEYWORD_CONFIG);
  res |= parser->IfPeekToken(NewTokenType_KEYWORD_STATE);
  res |= parser->IfPeekToken(NewTokenType_KEYWORD_MEM);
  
  return res;
}

bool IsNextTokenConfigFunctionStart(Tokenizer* tok){
  bool res = false;

  res |= tok->IfPeekToken("config");
  res |= tok->IfPeekToken("state");
  res |= tok->IfPeekToken("mem");
  
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
  
  auto list = PushArenaList<Token>(temp);
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
  res->arguments = PushArrayFromList(out,list);
  return res;
}

static String GlobalConfigFunctionName(String functionName,FUDeclaration* decl,Arena* out){
  String name = PushString(out,"%.*s_%.*s",UN(decl->name),UN(functionName));
  return name;
}

struct ParseResult{
  bool isArray;
  bool isFunctionInvoc;
  bool isWire;
  bool isExpr;
  
  SymbolicExpression* expr;
  Array<SpecExpression*> args;
  Token entityName;
  Token wireName; 
  Token functionName;
};

ParseResult ParseRHS(Env* env,SpecExpression* top,Arena* out){
/*
  RHS can be:

  wire           'name.wire'
  addrExpr       'addr[expr]'
  functionInvoc  'name(args)'
  expression     'expr'
*/

  // TODO: We still need to access env to check if this makes sense or not.
  //       nocheckin

  ParseResult res = {};
  if(top->type == SpecExpression::ARRAY_ACCESS){
    res.isArray = true;
    res.expr = SymbolicFromSpecExpression(top->expressions[0],out);
    res.entityName = top->name;
  } else if(top->type == SpecExpression::SINGLE_ACCESS){
    res.isWire = true;
    res.entityName = top->name;
    res.wireName = top->expressions[0]->name;
  } else if(top->type == SpecExpression::FUNCTION_CALL){
    res.isFunctionInvoc = true;
    res.args = top->expressions;
    res.functionName = top->name;
  } else {    
    res.isExpr = true;
    res.expr = SymbolicFromSpecExpression(top,out);
  }

  return res;
}

ConfigFunction* InstantiateConfigFunction(Env* env,ConfigFunctionDef* def,FUDeclaration* declaration,String content,Arena* out){
  TEMP_REGION(temp,out);

  auto list = PushArenaList<ConfigStuff>(temp);
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
  auto simpleStmtList = PushArenaList<ConfigStatement*>(temp);

  auto Recurse = [nodeToParent,simpleStmtList](auto Recurse,ConfigStatement* top) -> void{
    if(top->type == ConfigStatementType_FOR_LOOP){
      for(ConfigStatement* child : top->childs){
        nodeToParent->Insert(child,top);
        Recurse(Recurse,child);
      }
    }
    if(top->type == ConfigStatementType_STATEMENT){
      *simpleStmtList->PushElem() = top;
    }
  };

  for(ConfigStatement* top : def->statements){
    Recurse(Recurse,top);
  }

  auto stmtList = PushArenaList<Array<ConfigStatement*>>(temp);
  for(ConfigStatement* stmt : simpleStmtList){
    auto list = PushArenaList<ConfigStatement*>(temp);
    
    ConfigStatement* ptr = stmt;
    while(ptr){
      *list->PushElem() = ptr;

      ConfigStatement** possibleParent = nodeToParent->Get(ptr);
      ConfigStatement* parent = possibleParent ? *possibleParent : nullptr;

      ptr = parent;
    }

    Array<ConfigStatement*> asArray = PushArrayFromList(temp,list);
    ReverseInPlace(asArray);
    *stmtList->PushElem() = asArray;
  }

  // From this point on use this. Every N sized array is composed of N-1 FOR_LOOP types and 1 STATEMENT type.
  // TODO: Remember, after pushing every statement into an individual loop, we need to do error checking and check if the variable still exists. We cannot do variable checking globally since some statements might not be inside one of the loops.
  Array<Array<ConfigStatement*>> individualStatements = PushArrayFromList(temp,stmtList);

  // TODO: Kinda stupid calculating things this way but the rest of the code needs to collapse into a simpler form for the more robust approach first.
  auto variablesUsedOnLoopExpressions = PushTrieSet<String>(temp);

  for(Array<ConfigStatement*> arr : individualStatements){
    for(ConfigStatement* conf : arr){
      if(conf->type != ConfigStatementType_FOR_LOOP){
        continue;
      }

      AddressGenForDef2 def = conf->def2;

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
    varInfo[i].usedOnLoopExpressions = variablesUsedOnLoopExpressions->Exists(decl.name);
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
      ConfigStatement* stmt = stmts[0];
      ConfigStatement* simple = stmts[stmts.size - 1];
      // TODO: Call entity function to make sure that the entity exists and it is a config wire

      Token name = {};
      Assert(simple->type == ConfigStatementType_STATEMENT);
      name = GetBase(simple->lhs)->name;
      
      FULL_SWITCH(stmt->type){
      // NOTE: This will always produce an address gen
      case ConfigStatementType_FOR_LOOP:{
        auto forLoops = PushArenaList<AddressGenForDef2>(temp);

        for(int i = 0; i < stmts.size - 1; i++){
          *forLoops->PushElem() = stmts[i]->def2;
        }

        Array<AddressGenForDef2> loops = PushArrayFromList(temp,forLoops);

        Entity* ent = env->GetEntity(simple->lhs,temp);
        
        String name = GetBase(simple->lhs)->name;

        Entity* portEnt = nullptr;
        if(ent->type == EntityType_MEM_PORT){
          portEnt = ent;
          ent = ent->parent;
        }
        
        bool isArray = simple->trueRhs->type == SpecExpression::ARRAY_ACCESS;

        SymbolicExpression* expr = nullptr;
        
        if(isArray){
          expr = SymbolicFromSpecExpression(simple->trueRhs->expressions[0],temp);
        }

        ParseResult parsedRhs = ParseRHS(env,simple->trueRhs,temp);

        AddressAccess* access = nullptr;

        if(parsedRhs.isExpr || parsedRhs.isArray){
          access = CompileAddressGen(variableNames,loops,parsedRhs.expr,content);
        }
        AddressGenInst supported = ent->instance->declaration->supportedAddressGen;

        DEBUG_BREAK();

        // TODO: This logic is stupid. Rework into something better when we finalize the addressGen change.
        if(supported.type == AddressGenType_GEN){
          ConfigStuff* newAssign = list->PushElem();

          newAssign->type = ConfigStuffType_ADDRESS_GEN;
          newAssign->access.access = access;
          newAssign->access.inst = supported;

          newAssign->lhs = PushString(out,name);
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
        } else {
          ConfigStuff* newAssign = list->PushElem();

          newAssign->type = ConfigStuffType_ADDRESS_GEN;
          newAssign->access.access = access;
          newAssign->access.inst = supported;
          newAssign->accessVariableName = PushString(out,parsedRhs.entityName);
          newAssign->lhs = PushString(out,name);
        }
      } break;
      case ConfigStatementType_STATEMENT:{
        ParseResult parsedRhs = ParseRHS(env,stmt->trueRhs,temp);

        if(parsedRhs.isFunctionInvoc){
          Token functionName = parsedRhs.functionName;
          
          Array<SpecExpression*> args = parsedRhs.args;

          //FunctionInvocation* functionInvoc = stmt->func;

          ConfigFunction* function = nameToFunction->Get(functionName);
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
          Hashmap<String,SymbolicExpression*>* argToVar = PushHashmap<String,SymbolicExpression*>(temp,args.size);
          for(int i = 0; i <  args.size; i++){
            ConfigVariable arg = function->variables[i];

            //SymbolicExpression* var = PushVariable(temp,functionInvoc->arguments[i]);
            
            SymbolicExpression* var = SymbolicFromSpecExpression(args[i],temp);
            argToVar->Insert(arg.name,var);
          }

          String name = GetBase(simple->lhs)->name;
      
          for(ConfigStuff assign : function->stuff){
            // TODO: We are building the struct access expression in here but I got a feeling that we probably want to preserve data as much as possible in order to tackle merge later on.
            String lhs = PushString(out,"%.*s.%.*s",UN(name),UN(assign.assign.lhs));
        
            SymbolicExpression* rhs = assign.assign.rhs;
            SymbolicExpression* newExpr = ReplaceVariables(rhs,argToVar,out);
        
            ConfigStuff* newAssign = list->PushElem();
            newAssign->type = ConfigStuffType_ASSIGNMENT;
            newAssign->assign.lhs = lhs;
            newAssign->assign.rhs = newExpr;
          }
        }
        if(parsedRhs.isExpr){
          String name = GetBase(simple->lhs)->name;
          
          ConfigIdentifier* before = GetBeforeBase(simple->lhs);
          String wireName = before->name;

          ConfigStuff* assign = list->PushElem();
          assign->type = ConfigStuffType_ASSIGNMENT;
          assign->assign.lhs = PushString(out,"%.*s.%.*s",UN(name),UN(wireName));
          assign->assign.rhs = SymbolicDeepCopy(parsedRhs.expr,out);
        }
      }
    }
    }
  }    

  auto newStructs = PushArenaList<String>(temp);

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

      if(stmt->trueRhs->type == SpecExpression::SINGLE_ACCESS){
        id.name = stmt->trueRhs->name;
      }

      Entity* ent = env->GetEntity(stmt->trueRhs,temp);

      ParseResult parsedRhs = ParseRHS(env,stmt->trueRhs,temp);

      if(parsedRhs.isWire){
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

      bool singleStatement = (individualStatements.size == 1);

      Assert(singleStatement || (stmt->type == ConfigStatementType_FOR_LOOP 
                              && simple->type == ConfigStatementType_STATEMENT));

      ParseResult parsedRhs = ParseRHS(env,simple->trueRhs,temp);

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

      String sizeExpr = "1";
      if(!singleStatement){
        SymbolicExpression* start = SymbolicFromSpecExpression(stmt->def2.startSym,temp);
        SymbolicExpression* end = SymbolicFromSpecExpression(stmt->def2.endSym,temp);

        SymbolicExpression* diff = SymbolicSub(end,start,temp);
        sizeExpr = PushRepr(out,diff);
      }

#if 1
      ConfigStuff* assign = list->PushElem();
      assign->type = ConfigStuffType_MEMORY_TRANSFER;
      assign->transfer.dir = dir;
      assign->transfer.entityName = PushString(out,unit->instance->name);
      assign->transfer.sizeExpr = sizeExpr;
      assign->transfer.variable = PushString(out,addrVar->varName);
#endif

      // Left and right side must both be arrays.
#if 0
      FULL_SWITCH(stmt->type){
      case ConfigStatementType_STATEMENT:{
        FULL_SWITCH(stmt->rhsType){
        case ConfigRHSType_FUNCTION_CALL:{
          // TODO: Proper error reporting
          Assert(false);
        } break;
        case ConfigRHSType_SYMBOLIC_EXPR:{
          // TODO: Proper error reporting
          Assert(false);
        } break;
        case ConfigRHSType_IDENTIFIER:{
          // Only tackling simple forms, for now.
          Assert(stmt->lhs.expr);
          Assert(stmt->rhsId.expr);
        
          // Check that left entity supports memory access
          Array<String> accessExpr = PushArray<String>(temp,1);
          accessExpr[0] = stmt->lhs.name;

          Opt<Entity> entityOpt = GetEntityFromHierAccessWithEnvironment(&declaration->info,env,accessExpr);

          if(!entityOpt){
            // TODO: Proper error reporting.
            printf("Error, entity does not exist\n");
            exit(-1);
          }
          Entity entity = entityOpt.value();
          Assert(entity.type == EntityType_NODE);
          Assert(entity.info->memMapBits.has_value());
          
          ConfigStuff* assign = list->PushElem();
          assign->type = ConfigStuffType_MEMORY_TRANSFER;
          assign->transfer.dir = TransferDirection_READ;
          assign->transfer.identity = "TOP_m_addr";
          assign->transfer.sizeExpr = "1";
          assign->transfer.variable = PushString(out,simple->rhsId.name);
        } break;
      }
      } break;
      case ConfigStatementType_FOR_LOOP:{
        auto forLoops = PushArenaList<AddressGenForDef>(temp);

        for(int i = 0; i < stmts.size - 1; i++){
          *forLoops->PushElem() = stmts[i]->def;
        }
        
        Array<AddressGenForDef> loops = PushArrayFromList(temp,forLoops);

        ReportErrorIf(loops.size != 1,"Cannot handle more than 1 loop inside a mem configuration");

        Assert(simple->type == ConfigStatementType_STATEMENT);
        Assert(simple->rhsType == ConfigRHSType_IDENTIFIER);
        
        SymbolicExpression* end = ParseSymbolicExpression(loops[0].endSym,out);
        
        String name = simple->lhs.name;

        Array<String> accessExpr = PushArray<String>(temp,1);
        accessExpr[0] = simple->lhs.name;
        Opt<Entity> entityOpt = GetEntityFromHierAccess(&declaration->info,accessExpr);

        // TODO: This is currently hardcoded for some examples while we are trying to figure out how to proceed.
        if(entityOpt.has_value() && entityOpt.value().type == EntityType_NODE){
          // Need to build an address gen in here or something that we can then use to extract the info that we need.
          ConfigStuff* assign = list->PushElem();
          assign->type = ConfigStuffType_MEMORY_TRANSFER;
          assign->transfer.dir = TransferDirection_READ;
          assign->transfer.identity = "TOP_m_addr";
          assign->transfer.sizeExpr = PushRepr(out,end);
          assign->transfer.variable = "addr";//PushString(out,simple->rhsId.name);
        } else {
          ConfigStuff* assign = list->PushElem();
          assign->type = ConfigStuffType_MEMORY_TRANSFER;
          assign->transfer.dir = TransferDirection_WRITE;
          assign->transfer.identity = "TOP_m_addr";
          assign->transfer.sizeExpr = PushRepr(out,end);
          assign->transfer.variable = "addr";//PushString(out,simple->rhsId.name);
        }
      } break;
    }      
#endif
    }
  }

  ConfigFunction func = {};
  func.type = type;
  func.decl = declaration;
  func.stuff = PushArrayFromList(out,list);
  func.variables = varInfo;
  func.individualName = PushString(out,def->name);
  func.fullName = GlobalConfigFunctionName(func.individualName,declaration,out);
  func.newStructs = PushArrayFromList(out,newStructs);
  func.structToReturnName = structToReturnName;
  func.debug = def->debug;
  func.supportsSizeCalc = supportsSizeCalc;

  ConfigFunction* res = nameToFunction->Insert(func.fullName,func);

  env->PopScope();
  
  return res;  
}
