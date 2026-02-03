#include "versatSpecificationParser.hpp"

#include "accelerator.hpp"
#include "declaration.hpp"
#include "embeddedData.hpp"
#include "globals.hpp"
#include "memory.hpp"
#include "parser.hpp"
#include "symbolic.hpp"
#include "templateEngine.hpp"
#include "utils.hpp"
#include "utilsCore.hpp"
#include "userConfigs.hpp"

// TODO: Rework expression parsing to support error reporting similar to module diff.
//       A simple form of synchronization after detecting an error would vastly improve error reporting
//       Change iterative and merge to follow module def.
//       Need to detect multiple inputs to the same port and report error.
//       Error reporting is very generic. Implement more specific forms.
//       This parser is still not production ready. At the very least all the Asserts should be removed and replaced
//         by actual error reporting. Not a single Assert is programmer error detector, all the current ones are
//         user error that most be reported.


// TODO: This functions could show more lines of code before and after so we can actually see where the problem is.
void ReportError(String content,Token faultyToken,String error){
  TEMP_REGION(temp,nullptr);

  String loc = GetRichLocationError(content,faultyToken,temp);

  printf("[Error]\n");
  printf("%.*s:\n",UN(error));
  printf("%.*s\n",UN(loc));
  printf("\n");
}

void ReportError2(String content,Token faultyToken,Token goodToken,String faultyError,String good){
  TEMP_REGION(temp,nullptr);

  String loc = GetRichLocationError(content,faultyToken,temp);
  String loc2 = GetRichLocationError(content,goodToken,temp);
  
  printf("[Error]\n");
  printf("%.*s:\n",UN(faultyError));
  printf("%.*s\n",UN(loc));
  printf("%.*s:\n",UN(good));
  printf("%.*s\n",UN(loc2));
  printf("\n");
}

void ReportError(Tokenizer* tok,Token faultyToken,String error){
  String content = tok->GetContent();
  ReportError(content,faultyToken,error);
}

bool _ExpectError(Tokenizer* tok,String expected){
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

void _UnexpectError(Token token){
  TEMP_REGION(temp,nullptr);
  String content = PushString(temp,"At pos: %d:%d, did not expect to get: \"%.*s\"",token.loc.start.line,token.loc.start.column,UN(token));  
  printf("%.*s\n",UN(content));
}

// Macro because we want to return as well
#define EXPECT(TOKENIZER,STR) \
  do{ \
    if(_ExpectError(TOKENIZER,STR)){ \
      return {}; \
    } \
  } while(0)


#define UNEXPECTED(TOK) \
  do{ \
    _UnexpectError(TOK); \
    return {}; \
  } while(0)

#define CHECK_IDENTIFIER(ID) \
  if(!IsIdentifier(ID)){ \
    ReportError(tok,ID,StaticFormat("type name '%.*s' is not a valid name",UN(ID))); \
    return {}; \
  }

Opt<int> ParseNumber(Tokenizer* tok){
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

Opt<Range<int>> ParseRange(Tokenizer* tok){
  Range<int> res = {};

  Opt<int> n1 = ParseNumber(tok);
  PROPAGATE(n1);

  if(!tok->IfNextToken("..")){
    res.start = n1.value();
    res.end = n1.value();
    return res;
  }

  Opt<int> n2 = ParseNumber(tok);
  PROPAGATE(n2);

  res.start = n1.value();
  res.end = n2.value();
  
  return res;
}

Opt<Var> ParseVar(Tokenizer* tok){
  Var var = {};

  Token name = tok->NextToken();

  Token peek = tok->PeekToken();
  if(CompareString(peek,"[")){
    tok->AdvancePeek();

    Opt<Range<int>> range = ParseRange(tok);
    PROPAGATE(range);

    var.isArrayAccess = true;
    var.index = range.value();

    EXPECT(tok,"]");
  }
  
  int delayStart = 0;
  int delayEnd = 0;
  peek = tok->PeekToken();
  if(CompareString(peek,"{")){
    tok->AdvancePeek();

    Opt<Range<int>> rangeOpt = ParseRange(tok);
    PROPAGATE(rangeOpt);
    delayStart = rangeOpt.value().start;
    delayEnd = rangeOpt.value().end;

    EXPECT(tok,"}");
    peek = tok->PeekToken();
  }

  int portStart = 0;
  int portEnd = 0;
  peek = tok->PeekToken();
  if(CompareString(peek,":")){
    tok->AdvancePeek();

    Opt<Range<int>> rangeOpt = ParseRange(tok);
    PROPAGATE(rangeOpt);
    portStart = rangeOpt.value().start;
    portEnd = rangeOpt.value().end;
  }

  var.name = name;
  var.extra.delay.start = delayStart;
  var.extra.delay.end = delayEnd;
  var.extra.port.start = portStart;
  var.extra.port.end = portEnd;

  return var;
}

// A group can also be a single var. It does not necessary mean that it must be of the form {...}
Opt<VarGroup> ParseVarGroup(Tokenizer* tok,Arena* out){
  auto tokMark = tok->Mark();

  Token peek = tok->PeekToken();
  
  if(CompareString(peek,"{")){
    tok->AdvancePeek();

    auto arr = StartArray<Var>(out);
    while(!tok->Done()){
      Var* var = arr.PushElem();
      Opt<Var> optVar = ParseVar(tok);
      PROPAGATE(optVar);

      *var = optVar.value();

      Token sepOrEnd = tok->NextToken();
      if(CompareString(sepOrEnd,",")){
        continue;
      } else if(CompareString(sepOrEnd,"}")){
        break;
      } else {
        UNEXPECTED(sepOrEnd);
      }
    }
    VarGroup res = {};
    res.vars = EndArray(arr);
    res.fullText = tok->Point(tokMark);
    return res;
  } else {
    Opt<Var> optVar = ParseVar(tok);
    PROPAGATE(optVar);

    Var var = optVar.value();
    VarGroup res = {};
    res.fullText = tok->Point(tokMark);
    res.vars = PushArray<Var>(out,1);
    res.vars[0] = var;
    return res;
  }
}

SpecExpression* ParseAtom(Tokenizer* tok,Arena* out){
  Token peek = tok->PeekToken();

  bool negate = false;
  const char* negateType = nullptr;
  if(CompareString(peek,"~")){
    negate = true;
    negateType = "~";
    tok->AdvancePeek();
  }
  if(CompareString(peek,"-")){
    negate = true;
    negateType = "-";
    tok->AdvancePeek();
  }

  SpecExpression* expr = PushStruct<SpecExpression>(out);
  
  peek = tok->PeekToken();
  if(peek[0] >= '0' && peek[0] <= '9'){  // TODO: Need to call ParseNumber
    tok->AdvancePeek();

    int digit = ParseInt(peek);

    expr->type = SpecExpression::LITERAL;
    expr->val = MakeValue(digit);
  } else {
    expr->type = SpecExpression::VAR;
    Opt<Var> optVar = ParseVar(tok);
    PROPAGATE(optVar);

    expr->var = optVar.value();
  }

  if(negate){
    SpecExpression* negateExpr = PushStruct<SpecExpression>(out);
    negateExpr->op = negateType;
    negateExpr->type = SpecExpression::OPERATION;
    negateExpr->expressions = PushArray<SpecExpression*>(out,1);
    negateExpr->expressions[0] = expr;

    expr = negateExpr;
  }

  return expr;
}

SpecExpression* ParseSpecExpression(Tokenizer* tok,Arena* out);
SpecExpression* ParseTerm(Tokenizer* tok,Arena* out){
  Token peek = tok->PeekToken();

  SpecExpression* expr = nullptr;
  if(CompareString(peek,"(")){
    tok->AdvancePeek();
    expr = ParseSpecExpression(tok,out);
    tok->AssertNextToken(")");
  } else {
    expr = ParseAtom(tok,out);
  }

  return expr;
}

SpecExpression* ParseSpecExpression(Tokenizer* tok,Arena* out){
  SpecExpression* expr = ParseOperationType<SpecExpression>(tok,{{"+","-"},{"&","|","^"},{">><",">>","><<","<<"}},ParseTerm,out);

  return expr;
}

String GetUniqueName(String name,Arena* out,InstanceTable* names){
  int counter = 0;
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

// Right now, not using the full portion of PortExpression because technically we would need to instantiate multiple things. Not sure if there is a need, when a case occurs then make the change then
PortExpression InstantiateSpecExpression(SpecExpression* root,Accelerator* circuit,InstanceTable* table){
  TEMP_REGION(temp,nullptr);
  Arena* perm = globalPermanent;
  PortExpression res = {};

  switch(root->type){
    // Just to remove warnings. TODO: Change expression so that multiple locations have their own expression struct, instead of reusing the same one.
  case SpecType_LITERAL:{
    int number = root->val.number;

    String toSearch = PushString(temp,"N%d",number);

    FUInstance** found = table->Get(toSearch);

    if(!found){
      String uniqueName = GetUniqueName(toSearch,perm,table);

      FUInstance* digitInst = (FUInstance*) CreateFUInstance(circuit,GetTypeByName("Literal"),uniqueName);
      digitInst->literal = number;
      table->Insert(digitInst->name,digitInst);
      res.inst = digitInst;
    } else {
      res.inst = *found;
    }
  }break;
  case SpecType_VAR:{
    Var var = root->var;  
    String name = var.name;

    if(var.isArrayAccess){
      name = GetActualArrayName(var.name,var.index.bottom,globalPermanent);
    }
    
    FUInstance* inst = table->GetOrFail(name);

    res.inst = inst;
    res.extra = var.extra;
  } break;
  case SpecType_OPERATION:{
    PortExpression expr0 = InstantiateSpecExpression(root->expressions[0],circuit,table);

    // Assuming right now very simple cases, no port range and no delay range
    Assert(expr0.extra.port.start == expr0.extra.port.end);
    Assert(expr0.extra.delay.start == expr0.extra.delay.end);

    if(root->expressions.size == 1){
      Assert(root->op[0] == '~' || root->op[0] == '-');

      String typeName = {};

      switch(root->op[0]){
      case '~':{
        typeName = "NOT";
      }break;
      case '-':{
        typeName = "NEG";
      }break;
      }

      String permName = GetUniqueName(typeName,perm,table);
      FUInstance* inst = CreateFUInstance(circuit,GetTypeByName(typeName),permName);
      table->Insert(inst->name,inst);

      ConnectUnits(expr0.inst,expr0.extra.port.start,inst,0,expr0.extra.delay.start);

      res.inst = inst;
      res.extra.port.end  = res.extra.port.start  = 0;
      res.extra.delay.end = res.extra.delay.start = 0;

      return res;
    } else {
      Assert(root->expressions.size == 2);
    }

    PortExpression expr1 = InstantiateSpecExpression(root->expressions[1],circuit,table);

    // Assuming right now very simple cases, no port range and no delay range
    Assert(expr1.extra.port.start == expr1.extra.port.end);
    Assert(expr1.extra.delay.start == expr1.extra.delay.end);

    String op = root->op;
    const char* typeName;
    if(CompareString(op,"&")){
      typeName = "AND";
    } else if(CompareString(op,"|")){
      typeName = "OR";
    } else if(CompareString(op,"^")){
      typeName = "XOR";
    } else if(CompareString(op,">><")){
      typeName = "RHR";
    } else if(CompareString(op,">>")){
      typeName = "SHR";
    } else if(CompareString(op,"><<")){
      typeName = "RHL";
    } else if(CompareString(op,"<<")){
      typeName = "SHL";
    } else if(CompareString(op,"+")){
      typeName = "ADD";
    } else if(CompareString(op,"-")){
      typeName = "SUB";
    } else {
      // TODO: Proper error reporting
      printf("%.*s\n",UN(op));
      Assert(false);
    }

    String typeStr = typeName;
    FUDeclaration* type = GetTypeByName(typeStr);
    String uniqueName = GetUniqueName(type->name,perm,table);

    FUInstance* inst = CreateFUInstance(circuit,type,uniqueName);
    table->Insert(inst->name,inst);

    ConnectUnits(expr0.inst,expr0.extra.port.start,inst,0,expr0.extra.delay.start);
    ConnectUnits(expr1.inst,expr1.extra.port.start,inst,1,expr1.extra.delay.start);

    res.inst = inst;
    res.extra.port.end  = res.extra.port.start  = 0;
    res.extra.delay.end = res.extra.delay.start = 0;
  } break;
  }

  Assert(res.inst);
  return res;
}

Opt<VarDeclaration> ParseVarDeclaration(Tokenizer* tok){
  VarDeclaration res = {};

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
  
  return res;
}

Array<Token> CheckAndParseConnectionTransforms(Tokenizer* tok,Arena* out){
  auto arr = StartArray<Token>(out);
  while(!tok->Done()){
    auto mark = tok->Mark();
    Token first = tok->NextToken();
    Token second = tok->NextToken();
    
    if(!CompareString(second,"->")){
      tok->Rollback(mark);
      break;
    }

    *arr.PushElem() = first;
  }
  return EndArray(arr);
}

Opt<ConnectionDef> ParseConnection(Tokenizer* tok,Arena* out){
  ConnectionDef def = {};

  Opt<VarGroup> optOutPortion = ParseVarGroup(tok,out);
  PROPAGATE(optOutPortion);

  def.output = optOutPortion.value();

  Token type = tok->NextToken();
  if(CompareString(type,"=")){
    def.type = ConnectionType_EQUALITY;
  } else if(CompareString(type,"^=")){
    // TODO: Only added this to make it easier to port a piece of C code.
    //       Do not know if needed or worth it to add.
  } else if(CompareString(type,"->")){
    def.transforms = CheckAndParseConnectionTransforms(tok,out);
    def.type = ConnectionType_CONNECTION;
  } else {
    UNEXPECTED(type);
  }
  
  if(def.type == ConnectionType_EQUALITY){
    def.expression = ParseSpecExpression(tok,out);
  } else if(def.type == ConnectionType_CONNECTION){
    Opt<VarGroup> optInPortion = ParseVarGroup(tok,out);
    PROPAGATE(optInPortion);

    def.input = optInPortion.value();
  }

  EXPECT(tok,";");
  
  return def;
}

Opt<Array<VarDeclaration>> ParseModuleInputDeclaration(Tokenizer* tok,Arena* out){
  auto array = StartArray<VarDeclaration>(out);

  EXPECT(tok,"(");

  if(tok->IfNextToken(")")){
    return EndArray(array);
  }
  
  while(!tok->Done()){
    Opt<VarDeclaration> var = ParseVarDeclaration(tok);
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

Opt<InstanceDeclaration> ParseInstanceDeclaration(Tokenizer* tok,Arena* out){
  TEMP_REGION(temp,out);
  InstanceDeclaration res = {};

  while(1){
    Token potentialModifier = tok->PeekToken();

    if(CompareString(potentialModifier,"debug")){
      tok->AdvancePeek();

      res.debug = true;
    } else if(CompareString(potentialModifier,"static")){
      if(res.modifier == InstanceDeclarationType_SHARE_CONFIG){
        ReportError(tok,potentialModifier,"We already seen a static modifier. Versat currently does not support static and share at the same time inside the same modifier");
        return {};
      }

      tok->AdvancePeek();
      res.modifier = InstanceDeclarationType_STATIC;
    } else if(CompareString(potentialModifier,"share")){
      if(res.modifier == InstanceDeclarationType_STATIC){
        ReportError(tok,potentialModifier,"We already seen a static modifier. Versat currently does not support static and share at the same time inside the same modifier");
        return {};
      }

      tok->AdvancePeek();

      EXPECT(tok,"(");

      // We only support one, so no need to parse a list for now.
      // When adding delay we are gonna need a list here.
      EXPECT(tok,"config"); // Only choice that is enabled, for now

      EXPECT(tok,")");
      res.modifier = InstanceDeclarationType_SHARE_CONFIG;
      
      res.typeName = tok->NextToken();
      CHECK_IDENTIFIER(res.typeName);

      if(tok->IfNextToken("(")){
        // TODO: For now, we assume that every wire specified inside the spec file is a negative (remove share).
        auto toShare = StartArray<Token>(out);
        while(!tok->Done()){
          Token name = tok->NextToken();
          CHECK_IDENTIFIER(name);

          *toShare.PushElem() = name;

          if(tok->IfNextToken(",")){
            continue;
          } else {
            break;
          }
        }
      
        EXPECT(tok,")");

        res.shareNames = EndArray(toShare);
      }

      EXPECT(tok,"{");

      auto array = StartArray<VarDeclaration>(out);
    
      while(!tok->Done()){
        String peek = tok->PeekToken();

        if(CompareString(peek,"}")){
          break;
        }

        Opt<VarDeclaration> optVarDecl = ParseVarDeclaration(tok);
        PROPAGATE(optVarDecl);
      
        *array.PushElem() = optVarDecl.value();
      
        EXPECT(tok,";");
      }
      res.declarations = EndArray(array);
    
      EXPECT(tok,"}");
      return res;
    } else if(CompareString(potentialModifier,"using")){
      tok->AdvancePeek();

      EXPECT(tok,"(");

      auto array = StartArray<Token>(out);
      while(!tok->Done()){
        Token name = tok->NextToken();
        CHECK_IDENTIFIER(name);

        *array.PushElem() = name;
        
        if(tok->IfNextToken(")")){
          break;
        }
        EXPECT(tok,",");
      }
      res.addressGenUsed = EndArray(array);
    } else {
      break;
    }
  }

  res.typeName = tok->NextToken();
  CHECK_IDENTIFIER(res.typeName);

  Token possibleParameters = tok->PeekToken();
  auto list = PushArenaList<Pair<String,SymbolicExpression*>>(temp);
  if(CompareString(possibleParameters,"#")){
    tok->AdvancePeek();
    EXPECT(tok,"(");

    while(!tok->Done()){
      EXPECT(tok,".");
      String parameterName = tok->NextToken();

      EXPECT(tok,"(");

      SymbolicExpression* expr = ParseSymbolicExpression(tok,out);

#if 0
      String content = {};

      Opt<Token> remaining = tok->NextFindUntil(")");
      PROPAGATE(remaining);
      content = remaining.value();
#endif      

      EXPECT(tok,")");
      
      String savedParameter = PushString(out,parameterName);
      *list->PushElem() = {savedParameter,expr}; 

      if(tok->IfNextToken(",")){
        continue;
      }

      break;
    }
    EXPECT(tok,")");

    res.parameters = PushArrayFromList(out,list);
  }

  Opt<VarDeclaration> optVarDecl = ParseVarDeclaration(tok);
  PROPAGATE(optVarDecl);

  res.declarations = PushArray<VarDeclaration>(out,1);
  res.declarations[0] = optVarDecl.value();

  EXPECT(tok,";");

  return res;
}

// Any returned String points to tokenizer content.
// As long as tokenizer is valid, strings returned by this function are also valid.
Opt<ModuleDef> ParseModuleDef(Tokenizer* tok,Arena* out){
  TEMP_REGION(temp,out);
  ModuleDef def = {};

  tok->AssertNextToken("module");

  def.name = tok->NextToken();
  CHECK_IDENTIFIER(def.name);
  
  Opt<Array<VarDeclaration>> optVar = ParseModuleInputDeclaration(tok,out);
  PROPAGATE(optVar);

  def.inputs = optVar.value();

  Token peek = tok->PeekToken();
  if(CompareString(peek,"->")){
    tok->AdvancePeek();
    def.numberOutputs = tok->NextToken();
    // TODO : Need to check that numberOutputs is actually a number.
  }
  
  ArenaList<InstanceDeclaration>* decls = PushArenaList<InstanceDeclaration>(temp);
  EXPECT(tok,"{");
  while(!tok->Done()){
    Token peek = tok->PeekToken();

    if(CompareString(peek,";")){
      tok->AdvancePeek();
      continue;
    }

    if(CompareString(peek,"#")){
      break;
    }
    
    Opt<InstanceDeclaration> optDecl = ParseInstanceDeclaration(tok,out);
    PROPAGATE(optDecl); // TODO: We could try to keep going and find more errors

    *decls->PushElem() = optDecl.value();
  }
  def.declarations = PushArrayFromList(out,decls);

  EXPECT(tok,"#");
  
  ArenaList<ConnectionDef>* cons = PushArenaList<ConnectionDef>(temp);
  while(!tok->Done()){
    Token peek = tok->PeekToken();

    if(CompareString(peek,";")){
      tok->AdvancePeek();
      continue;
    }

    if(CompareString(peek,"}")){
      break;
    }
    if(CompareString(peek,"##")){
      break;
    }

    Opt<ConnectionDef> optCon = ParseConnection(tok,out);
    PROPAGATE(optCon);
 
    *cons->PushElem() = optCon.value();
  }

  auto configFunctions = PushArenaList<ConfigFunctionDef>(temp);

  if(tok->IfNextToken("##")){
    while(!tok->Done()){
      if(IsNextTokenConfigFunctionStart(tok)){
        ConfigFunctionDef* func = ParseConfigFunction(tok,out);

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
  
  EXPECT(tok,"}");
  def.connections = PushArrayFromList(out,cons);
  def.configs = PushArrayFromList(out,configFunctions);
  
  return def;
}

FUDeclaration* ParseIterative(Tokenizer* tok){
  TEMP_REGION(temp,nullptr);
  TEMP_REGION(temp2,temp);
#if 0
  Arena* perm = globalPermanent;
  tok->AssertNextToken("iterative");

  BLOCK_REGION(temp);

  InstanceTable* table = PushHashmap<String,FUInstance*>(temp,1000);
  Set<String>* names = PushSet<String>(temp,1000);

  String moduleName = tok->NextToken();
  String name = PushString(perm,moduleName);

  Accelerator* iterative = CreateAccelerator(name,AcceleratorPurpose_MODULE);

  tok->AssertNextToken("(");
  // Arguments
  int insertedInputs = 0;
  while(1){
    Token argument = tok->PeekToken();

    if(CompareString(argument,")")){
      break;
    }
    tok->AdvancePeek();

    Token peek = tok->PeekToken();
    if(CompareString(peek,",")){
      tok->AdvancePeek();
    }

    String name = PushString(perm,argument);

    table->Insert(name,CreateOrGetInput(iterative,name,insertedInputs++));
  }
  tok->AssertNextToken(")");
  tok->AssertNextToken("{");
  
  FUInstance* unit = nullptr;
  // Instance instantiation;
  while(1){
    if(tok->IfPeekToken("#")){
      break;
    }

    Token instanceTypeName = tok->NextToken();
    Token instanceName = tok->NextToken();
    tok->AssertNextToken(";");

    FUDeclaration* type = GetTypeByName(instanceTypeName);
    String name = PushString(perm,instanceName);

    FUInstance* created = CreateFUInstance(iterative,type,name);
    table->Insert(name,created);
    
    if(!unit){
      unit = created;
    }
  }
  tok->AssertNextToken("#");

  String latencyStr = tok->NextToken();
  int latency = ParseInt(latencyStr); // TODO: Need to have actual error handling

  FUInstance* outputInstance = nullptr;

  Hashmap<PortInstance,FUInstance*>* portInstanceToMux = PushHashmap<PortInstance,FUInstance*>(temp,10);

  FUDeclaration* type = BasicDeclaration::stridedMerge;
  int index = 0;
  // For in
  while(1){
    if(tok->IfPeekToken("}")){
      break;
    }

    Token peek = tok->PeekToken();

    int num = -1;
    if(CompareString(peek,"%")){
      tok->AdvancePeek();
      String number = tok->NextToken();
      num = ParseInt(number); // TODO: Need to have actual error handling
    }

    Var start = ParseVar(tok).value();  // TODO: Handle errors

    tok->AssertNextToken("->");

    Var end = ParseVar(tok).value();  // TODO: Handle errors
    tok->AssertNextToken(";");

    FUInstance* inst1 = nullptr;
    FUInstance* inst2 = nullptr;

    inst1 = table->GetOrFail(start.name);

    if(CompareString(end.name,"out")){
      if(!outputInstance){
        outputInstance = (FUInstance*) CreateFUInstance(iterative,BasicDeclaration::output,"out");
        table->Insert("out",outputInstance);
      }

      inst2 = outputInstance;
    } else {
      inst2 = table->GetOrFail(end.name);
    }

    if(num == -1){
      ConnectUnit((PortExpression){inst1,start.extra},(PortExpression){inst2,end.extra});
      continue;
    }

    PortInstance instance = {};
    instance.inst = (FUInstance*) inst2;
    instance.port = end.extra.port.end;

    Assert(end.extra.port.start == end.extra.port.end); // For now do not handle ranges.

    GetOrAllocateResult<FUInstance*> res = portInstanceToMux->GetOrAllocate(instance);
    
    if(!res.alreadyExisted){
      static String names[] = {"Merge0",
                               "Merge1",
                               "Merge2",
                               "Merge3",
                               "Merge4",
                               "Merge5",
                               "Merge6",
                               "Merge7"};

      *res.data = CreateFUInstance(iterative,type,names[index]);
      table->Insert(names[index],*res.data);
      index += 1;

      ConnectUnit((PortExpression){*res.data,start.extra},(PortExpression){inst2,end.extra});
    }

    Assert(num >= 0);
    end.extra.port.end = end.extra.port.start = num;
    ConnectUnit((PortExpression){inst1,start.extra},(PortExpression){*res.data,end.extra});
  }
  tok->AssertNextToken("}");

  return RegisterIterativeUnit(iterative,unit,latency,name,temp,temp2);
#endif
  NOT_POSSIBLE();
}

Opt<TypeAndInstance> ParseTypeAndInstance(Tokenizer* tok){
  Token typeName = tok->NextToken();

  CHECK_IDENTIFIER(typeName);

  tok->PeekToken();

  Token instanceName = {};
  if(tok->IfNextToken(":")){
    instanceName = tok->NextToken();

    CHECK_IDENTIFIER(instanceName);
  }

  TypeAndInstance res = {};
  res.typeName = typeName;
  res.instanceName = instanceName;
  
  return res;
}

Opt<HierarchicalName> ParseHierarchicalName(Tokenizer* tok){
  Token topInstance = tok->NextToken();

  CHECK_IDENTIFIER(topInstance);

  EXPECT(tok,".");

  Opt<Var> var = ParseVar(tok);
  PROPAGATE(var);

  HierarchicalName res = {};
  res.instanceName = topInstance;
  res.subInstance = var.value();

  return res;
}

Opt<MergeDef> ParseMerge(Tokenizer* tok,Arena* out){
  TEMP_REGION(temp,out);
  
  tok->AssertNextToken("merge");

  Array<Token> mergeModifiers = {};
  if(tok->IfNextToken("(")){
    auto tokenList = PushArenaList<Token>(temp);
    
    while(!tok->Done()){
      Token peek = tok->PeekToken();
      
      if(CompareString(peek,")")){
        break;
      }

      CHECK_IDENTIFIER(peek);
      *tokenList->PushElem() = peek;

      tok->AdvancePeek();
      
      tok->IfNextToken(",");
    }

    mergeModifiers = PushArrayFromList(out,tokenList);
    
    EXPECT(tok,")");
  }

  Token mergeName = tok->NextToken();
  CHECK_IDENTIFIER(mergeName);
  
  EXPECT(tok,"=");

  ArenaList<TypeAndInstance>* declarationList = PushArenaList<TypeAndInstance>(temp);
  while(!tok->Done()){
    Opt<TypeAndInstance> optType = ParseTypeAndInstance(tok);
    PROPAGATE(optType); // TODO: Maybe Synchronize? At this point it is a bit of a problem trying to keep the parser working

    *declarationList->PushElem() = optType.value();

    Token peek = tok->PeekToken();
    if(CompareString(peek,"|")){
      tok->AdvancePeek();
      continue;
    } else if(CompareString(peek,"{")){
      break;
    } else if(CompareString(peek,";")){
      tok->AdvancePeek();
      break;
    }
  }
  Array<TypeAndInstance> declarations = PushArrayFromList(out,declarationList);

  Array<SpecNode> specNodes = {};
  if(tok->IfNextToken("{")){
    ArenaList<SpecNode>* specList = PushArenaList<SpecNode>(temp);
    while(!tok->Done()){
      Token peek = tok->PeekToken();
      if(CompareString(peek,"}")){
        break;
      }

      Opt<HierarchicalName> leftSide = ParseHierarchicalName(tok);
      PROPAGATE(leftSide);

      EXPECT(tok,"-");

      Opt<HierarchicalName> rightSide = ParseHierarchicalName(tok);
      PROPAGATE(rightSide);

      EXPECT(tok,";");

      *specList->PushElem() = {leftSide.value(),rightSide.value()};
    }
    specNodes = PushArrayFromList(out,specList);

    EXPECT(tok,"}");
  }
  
  auto specificsArr = StartArray<SpecificMergeNode>(out);
  for(SpecNode node : specNodes){
    int firstIndex = -1;
    int secondIndex = -1;
    for(int i = 0; i < declarations.size; i++){
      TypeAndInstance& decl = declarations[i];
      if(CompareString(node.first.instanceName,decl.instanceName)){
        firstIndex = i;
      } 
      if(CompareString(node.second.instanceName,decl.instanceName)){
        secondIndex = i;
      } 
    }

    if(firstIndex == -1){
      Assert(false);
      // ReportError
    }
    if(secondIndex == -1){
      Assert(false);
      // ReportError
    }

    *specificsArr.PushElem() = {firstIndex,node.first.subInstance.name,secondIndex,node.second.subInstance.name};
  }
  Array<SpecificMergeNode> specifics = EndArray(specificsArr);

  MergeDef result = {};
  result.name = mergeName;
  result.declarations = declarations;
  result.specifics = specifics;
  result.mergeModifiers = mergeModifiers;
  
  return result;
}

FUDeclaration* InstantiateMerge(MergeDef def){
  TEMP_REGION(temp,nullptr);
  
  auto declArr = StartArray<FUDeclaration*>(temp);
  for(TypeAndInstance tp : def.declarations){
    FUDeclaration* decl = GetTypeByNameOrFail(tp.typeName); // TODO: Rewrite stuff so that at this point we know that the type must exist
    *declArr.PushElem() = decl;
  }
  Array<FUDeclaration*> decl = EndArray(declArr);

  String name = PushString(globalPermanent,def.name);

  bool error = false;
  MergeModifier modifier = MergeModifier_NONE;

  for(Token t : def.mergeModifiers){
    Opt<MergeModifier> parsed = META_mergeModifiers_ReverseMap(t);

    if(!parsed.has_value()){
      printf("Error, merge does not support option: %.*s\n",UN(t));
      error = true;
    }

    modifier = (MergeModifier) (modifier | parsed.value());
  }

  if(error){
    return nullptr;
  }
  
  return Merge(decl,name,def.specifics,modifier);
}

int GetRangeCount(Range<int> range){
  Assert(range.end >= range.start);
  return (range.end - range.start + 1);
}

// Connection type and number of connections
Pair<PortRangeType,int> GetConnectionInfo(Var var){
  int indexCount = GetRangeCount(var.index);
  int portCount = GetRangeCount(var.extra.port);
  int delayCount = GetRangeCount(var.extra.delay);

  if(indexCount == 1 && portCount == 1 && delayCount == 1){
    return {PortRangeType_SINGLE,1};
  }

  // We cannot have more than one range at the same time because otherwise how do we decide how to connnect them?
  if((indexCount != 1 && portCount != 1) ||
     (indexCount != 1 && delayCount != 1) ||
     (portCount != 1 && delayCount != 1)){
    return {PortRangeType_ERROR,0};
  }
  
  if(var.isArrayAccess && indexCount != 1){
    return {PortRangeType_ARRAY_RANGE,indexCount};
  }

  if(portCount != 1){
    return {PortRangeType_PORT_RANGE,portCount};
  } else if(delayCount != 1){
    return {PortRangeType_DELAY_RANGE,delayCount};
  }

  NOT_POSSIBLE("Every condition should have been checked by now");
}

bool IsValidGroup(VarGroup group){
  // TODO: Wether we can match the group or not.
  //       It depends on wether the ranges line up or not. 
  for(Var& var : group.vars){
    if(GetConnectionInfo(var).first == PortRangeType_ERROR){
      return false;
    }
  }

  return true;
}

int NumberOfConnections(VarGroup group){
  Assert(IsValidGroup(group));

  int count = 0;

  for(Var& var : group.vars){
    count += GetConnectionInfo(var).second;
  }

  return count;
}

GroupIterator IterateGroup(VarGroup group){
  GroupIterator iter = {};
  iter.group = group;
  return iter;
}

bool HasNext(GroupIterator iter){
  if(iter.groupIndex >= iter.group.vars.size){
    return false;
  }

  return true;
}

Var Next(GroupIterator& iter){
  Assert(HasNext(iter));

  Var var = iter.group.vars[iter.groupIndex];
  Pair<PortRangeType,int> info = GetConnectionInfo(var);
  
  PortRangeType type = info.first;
  int maxCount = info.second;

  Assert(type != PortRangeType_ERROR);

  Var res = var;

  if(type == PortRangeType_SINGLE){
    iter.groupIndex += 1;
  } else {
    switch(type){
    case PortRangeType_SINGLE: break;
    case PortRangeType_ERROR: break;
    case PortRangeType_PORT_RANGE:{
      int portBase = var.extra.port.start;
    
      res.extra.port.start = portBase + iter.varIndex; 
      res.extra.port.end = portBase + iter.varIndex; 
    } break;
    case PortRangeType_DELAY_RANGE:{
      int delayBase = var.extra.delay.start;
    
      res.extra.delay.start = delayBase + iter.varIndex; 
      res.extra.delay.end = delayBase + iter.varIndex; 
    } break;
    case PortRangeType_ARRAY_RANGE:{
      int indexBase = var.index.start;

      res.index.start = indexBase + iter.varIndex; 
      res.index.end = indexBase + iter.varIndex; 
    } break;
    }

    iter.varIndex += 1;
    if(iter.varIndex >= maxCount){
      iter.varIndex = 0;
      iter.groupIndex += 1;
    }
  }
  
  return res;
}

FUInstance* CreateFUInstanceWithParameters(Accelerator* accel,FUDeclaration* type,String name,InstanceDeclaration decl){
  FUInstance* inst = CreateFUInstance(accel,type,name);
  
  for(auto pair : decl.parameters){
    bool result = SetParameter(inst,pair.first,pair.second);

    if(!result){
      printf("Warning: Parameter %.*s for instance %.*s in module %.*s does not exist\n",UN(pair.first),UN(inst->name),UN(accel->name));
    }
  }

  return inst;
}

// TODO: Move this function to a better place
FUDeclaration* InstantiateModule(String content,ModuleDef def){
  Arena* perm = globalPermanent;
  Accelerator* circuit = CreateAccelerator(def.name,AcceleratorPurpose_MODULE);

  FREE_ARENA(envArena);
  FREE_ARENA(envArena2);
  Env* env = StartEnvironment(envArena,envArena2);

  env->circuit = circuit;

  for(VarDeclaration& decl : def.inputs){
    env->AddInput(decl);
  }

  int shareIndex = 0;
  for(InstanceDeclaration& decl : def.declarations){
    if(decl.modifier | InstanceDeclarationType_SHARE_CONFIG){
      decl.shareIndex = shareIndex++;
    }
  }

  for(InstanceDeclaration& decl : def.declarations){
    for(VarDeclaration& var : decl.declarations){
      env->AddInstance(decl,var);
    }
  }

  for(ConnectionDef& decl : def.connections){
    if(decl.type == ConnectionType_EQUALITY){
      env->AddEquality(decl);
    } else if(decl.type == ConnectionType_CONNECTION){
      env->AddConnection(decl);
    }
  }

  if(!Empty(env->errors)){
    for(String str : env->errors){
      printf("%.*s\n",UN(str));
    }
    
    exit(-1);
  }
  
  FUDeclaration* res = RegisterSubUnit(circuit,SubUnitOptions_BAREBONES);
  
  {
    TEMP_REGION(temp,nullptr);
    auto list = PushArenaList<ConfigFunction*>(temp);
    for(auto funcDecl : def.configs){
      *list->PushElem() = InstantiateConfigFunction(&funcDecl,res,content,globalPermanent);
    };
    
    if(res->info.infos.size){
      res->info.infos[0].userFunctions = PushArrayFromList(perm,list);
    }
  }
  
  return res;
}

void Synchronize(Tokenizer* tok,BracketList<String> syncPoints){
  while(!tok->Done()){
    Token peek = tok->PeekToken();

    for(String point : syncPoints){
      if(CompareString(peek,point)){
        return;
      }
    }

    tok->AdvancePeek();
  }
}

Array<ConstructDef> ParseVersatSpecification(String content,Arena* out){
  TEMP_REGION(temp,out);
  Tokenizer tokenizer = Tokenizer(content,".%=#[](){}+:;,*~-",{"##","->=","->",">><","><<",">>","<<","..","^="});
  Tokenizer* tok = &tokenizer;

  ArenaList<ConstructDef>* typeList = PushArenaList<ConstructDef>(temp);
  
  bool anyError = false;
  while(!tok->Done()){
    Token peek = tok->PeekToken();

    if(CompareString(peek,"module")){
      Opt<ModuleDef> moduleDef = ParseModuleDef(tok,out);

      if(moduleDef.has_value()){
        ConstructDef def = {};
        def.type = ConstructType_MODULE;
        def.module = moduleDef.value();

        *typeList->PushElem() = def;
      } else {
        anyError = true;
      }
    } else if(CompareString(peek,"merge")){
      Opt<MergeDef> mergeDef = ParseMerge(tok,out);
      
      if(mergeDef.has_value()){
        ConstructDef def = {};
        def.type = ConstructType_MERGE;
        def.merge = mergeDef.value();

        *typeList->PushElem() = def;
      } else {
        anyError = true;
      }
    } else if(CompareString(peek,"addressGen")){
      Opt<AddressGenDef> addressDef = ParseAddressGen(tok,out);
      
      if(addressDef.has_value()){
        ConstructDef def = {};
        def.type = ConstructType_ADDRESSGEN;
        def.addressGen = addressDef.value();

        *typeList->PushElem() = def;
      } else {
        anyError = true;
      }
    } else {
      ReportError(tok,peek,"Unexpected token in global scope");
      tok->AdvancePeek();
      Synchronize(tok,{"module","merge","addressGen"});
    }
  }

  if(anyError){
    // NOTE: Error messages have already been printed at this point. Just terminate the program 
    exit(-1);
  }

  return PushArrayFromList(out,typeList);
}

bool IsModuleLike(ConstructDef def){
  FULL_SWITCH(def.type){
  case ConstructType_MODULE:
  case ConstructType_MERGE:
  case ConstructType_ITERATIVE:
    return true;
    break;
  case ConstructType_ADDRESSGEN:
    return false;
    break;
} END_SWITCH();

  Assert(false);
  return false;
}

Array<Token> TypesUsed(ConstructDef def,Arena* out){
  TEMP_REGION(temp,out);

  FULL_SWITCH(def.type){
  case ConstructType_MERGE: {
    // TODO: How do we deal with same types being used?
    //       Do we just ignore it?
    Array<Token> result = Extract(def.merge.declarations,out,&TypeAndInstance::typeName);
    
    return result;
  } break;
  case ConstructType_MODULE: {
    Array<Token> result = Extract(def.module.declarations,temp,&InstanceDeclaration::typeName);

    return Unique(result,out);
  } break;
  case ConstructType_ITERATIVE:{
    NOT_IMPLEMENTED("yet");
  };
  case ConstructType_ADDRESSGEN:{

  } break;
} END_SWITCH();

  return {};
}

Array<Token> AddressGenUsed(ConstructDef def,Array<ConstructDef> allConstructs,Arena* out){
  TEMP_REGION(temp,out);

  auto list = PushArenaList<Token>(temp);

  FULL_SWITCH(def.type){
  case ConstructType_MODULE: {
    ModuleDef mod = def.module;

    for(InstanceDeclaration decl : mod.declarations){
      for(Token tok : decl.addressGenUsed){
        *list->PushElem() = tok;
      }
    }
  } break;
  case ConstructType_MERGE: {
    MergeDef merge = def.merge;

    for(TypeAndInstance tp : merge.declarations){
      for(ConstructDef defs : allConstructs){
        if(CompareString(defs.base.name,tp.typeName)){
          Array<Token> used = AddressGenUsed(defs,allConstructs,temp);

          for(Token t : used){
            *list->PushElem() = t;
          }
        }
      }
    }
  } break;
  case ConstructType_ITERATIVE: {
    NOT_IMPLEMENTED("yet");
  } break;
  case ConstructType_ADDRESSGEN: {
    // This function returns address gens that module like constructs used. It is not supposed to be called with an actual AddressGen construct
    Assert(false);
  } break;
    
} END_SWITCH();

  return PushArrayFromList(out,list);
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
  case ConstructType_ADDRESSGEN:{
    Assert(false);
  } break;
  default: Assert(false);
  } END_SWITCH();

  return nullptr;
}

Opt<AddressGenDef> ParseAddressGen(Tokenizer* tok,Arena* out){
  TEMP_REGION(temp,out);

  EXPECT(tok,"addressGen");
  
  Token name = tok->NextToken();
  CHECK_IDENTIFIER(name);

  Array<Token> inputsArr = {};
  if(tok->IfNextToken("(")){
    if(tok->IfNextToken(")")){
      // Nothing
    } else {
      ArenaList<Token>* inputs = PushArenaList<Token>(temp);

      while(!tok->Done()){
        Token name = tok->NextToken();
        CHECK_IDENTIFIER(name);
        *inputs->PushElem() = name;
      
        if(tok->IfNextToken(",")){
          continue;
        } else {
          break;
        }
      }

      inputsArr = PushArrayFromList(out,inputs);
      EXPECT(tok,")");
    }
  }

  EXPECT(tok,"{");

  ArenaList<AddressGenForDef>* loops = PushArenaList<AddressGenForDef>(temp);
  Array<Token> symbolicTokens = {};
  while(!tok->Done()){
    Token construct = tok->PeekToken();
    
    if(CompareString(construct,"}")){
      break;
    }
    
    if(CompareString(construct,"for")){
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

      EXPECT(tok,":");
      
      *loops->PushElem() = (AddressGenForDef){.loopVariable = loopVariable,.startSym = startSym,.endSym = endSym};
    } else if(CompareString(construct,"addr")){
      tok->AdvancePeek();

      EXPECT(tok,"=");
      
      symbolicTokens = TokenizeSymbolicExpression(tok,out);
      
      if(symbolicTokens.size == 0){
        return {};
      }

      EXPECT(tok,";");
      break;
    }
  }

  EXPECT(tok,"}");

  // TODO: We actually want to return something here. An "Empty" address gen, which basically does nothing but still lets the program run the normal flow. It just does nothing in the sense that we do not actually generate anything for such address gen.
  if(Empty(symbolicTokens)){
    return {};
  }
  
  AddressGenDef def = {};
  def.name = name;
  def.inputs = inputsArr;
  def.loops = PushArrayFromList(out,loops);
  def.symbolicTokens = symbolicTokens;
  
  return def;
}

// ======================================
// Hierarchical access

Opt<Entity> GetEntityFromHierAccess(AccelInfo* info,Array<String> accessExpr){
  // TODO: This function can be simplified.
  AccelInfoIterator iter = StartIteration(info);

  // NOTE: Logic below assumes that we start with an iterator already pointing to the first unit
  for(; iter.IsValid();){
    InstanceInfo* info = iter.CurrentUnit();
    if(info->name == accessExpr[0]){
      break;
    } else {
      iter = iter.Next();
    }
  }
  
  if(!iter.IsValid()){
    return {};
  }

  for(int i = 1; i < accessExpr.size; i++){
    String access = accessExpr[i];
    
    if(i == accessExpr.size - 1){
      InstanceInfo* outerInfo = iter.CurrentUnit();

      for(; iter.IsValid();){
        InstanceInfo* info = iter.CurrentUnit();
        if(info->name == access){
          iter = iter.StepInsideOnly();

          if(!iter.IsValid()){
            break;
          }
          
          Entity res = {};
          res.info = iter.CurrentUnit();
          res.type = EntityType_NODE;

          return res;
        } else {
          iter = iter.Next();
        }
      }
      
      // Did not find node inside which means it must be a wire or a function.
      for(Wire& config : outerInfo->configs){
        if(config.name == access){
          Entity res = {};
          res.wire = &config;
          res.type = EntityType_CONFIG_WIRE;
          res.info = outerInfo;
          
          return res;
        }
      }

      for(Wire& state : outerInfo->states){
        if(state.name == access){
          Entity res = {};
          res.wire = &state;
          res.type = EntityType_STATE_WIRE;
          res.info = outerInfo;
          
          return res;
        }
      }

      for(MergePartition part : outerInfo->decl->info.infos){
        for(ConfigFunction* func : part.userFunctions){
          if(func->individualName == access){
            Entity res = {};
            res.func = func;
            res.type = EntityType_CONFIG_FUNCTION;
            res.info = outerInfo;
          
            return res;
          }
        }
      }
    } else {
      for(; iter.IsValid();){
        InstanceInfo* info = iter.CurrentUnit();
        if(info->name == access){
          iter = iter.StepInsideOnly();
          break;
        } else {
          iter = iter.Next();
        }
      }

      if(!iter.IsValid()){
        return {};
      }
    }
  }

  if(iter.IsValid()){
    Entity res = {};
    res.info = iter.CurrentUnit();
    res.type = EntityType_NODE;
    
    return res;
  }

  return {};
}

Env* StartEnvironment(Arena* freeUse,Arena* freeUse2){
  Env* env = PushStruct<Env>(freeUse);
  env->scopeArena = freeUse;
  env->miscArena = freeUse2;
  env->scopes = PushArray<EnvScope*>(freeUse,99);

  env->currentScope = -1;
  env->PushScope();

  env->errors = PushArenaList<String>(freeUse2);
  env->table = PushTrieMap<String,FUInstance*>(freeUse2);

  return env;
}

void Env::ReportError(Token badToken,String msg){
  // TODO: More detailed error message
  String error = PushString(miscArena,"[%.*s] %.*s: '%.*s'",UN(circuit->name),UN(msg),UN(badToken));
  *this->errors->PushElem() = error;
}

void Env::PushScope(){
  this->currentScope += 1;

  ArenaMark mark = MarkArena(this->scopeArena);
  this->scopes[this->currentScope] = PushStruct<EnvScope>(this->scopeArena);
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
  FUInstance* inst = CreateFUInstance(circuit,type,name);

  Token tok = {};
  tok = *((Token*) &inst->name);
  Entity* ent = PushNewEntity(tok);
  ent->type = EntityType_FU;
  ent->instance = inst;

  return inst;
}

FUInstance* Env::GetFUInstance(Var var){
  TEMP_REGION(temp,nullptr);
  
  FUInstance* res = nullptr;
  if(var.name == "out"){
    if(var.isArrayAccess){
      ReportError(var.name,"'out' special unit cannot have array subscriptions");
    }
    
    res = GetOutputInstance();
  } else {
    Entity* ent = GetEntity(var.name);
    
    String name = var.name;
    if(ent->type == EntityType_FU_ARRAY){
      name = GetActualArrayName(name,var.index.low,temp);
    }

    res = table->GetOrElse(name,nullptr);
  }
  return res;
}

FUInstance* Env::GetOutputInstance(){
  FUInstance* res = GetUnit(circuit,"out");

  if(!res){
    res = CreateFUInstance(circuit,BasicDeclaration::output,"out");
  }
  
  return res;
}

Entity* Env::PushNewEntity(Token name){
  if(name == "out"){
    ReportError(name,"Cannot have a variable with the reserved name out");
  }

  auto res = this->scopes[this->currentScope]->variable->GetOrAllocate(name);
  
  if(res.alreadyExisted){
    ReportError(name,"Entity already exists. Rename entity to resolve conflict");
  }

  return res.data;
}

Entity* Env::GetEntity(Token name){
  for(int i = this->currentScope; i >= 0; i--){
    Entity* ent = this->scopes[i]->variable->Get(name);

    if(ent){
      return ent;
    }
  }

  return nullptr;
}

void Env::AddInput(VarDeclaration var){
  TEMP_REGION(temp,nullptr);

  Entity* ent = PushNewEntity(var.name);

  if(var.isArray){
    ent->type = EntityType_FU_ARRAY;
    ent->arrayBaseName = var.name;
    ent->arraySize = var.arraySize;

    for(int i = 0; i < var.arraySize; i++){
      String actualName = GetActualArrayName(var.name,i,temp);
      FUInstance* input = CreateOrGetInput(circuit,actualName,insertedInputs++);
      table->Insert(input->name,input);
    }
  } else {
    FUInstance* input = CreateOrGetInput(circuit,var.name,insertedInputs++);
    table->Insert(input->name,input);
    
    ent->type = EntityType_FU;
    ent->instance = input;
  }

  ent->isInput = true;
}

void Env::AddInstance(InstanceDeclaration decl,VarDeclaration var){
  TEMP_REGION(temp,nullptr);

  FUDeclaration* type = GetTypeByName(decl.typeName);
  
  if(!type){
    ReportError(decl.typeName,"Typename does not exist");
  }

  Entity* ent = PushNewEntity(var.name);

  if(var.isArray){
    ent->type = EntityType_FU_ARRAY;
    ent->arrayBaseName = var.name;
    ent->arraySize = var.arraySize;

    for(int i = 0; i < var.arraySize; i++){
      String actualName = GetActualArrayName(var.name,i,temp);
      FUInstance* inst = CreateFUInstanceWithParameters(circuit,type,actualName,decl);
      table->Insert(inst->name,inst);
      inst->addressGenUsed = CopyArray<String,Token>(decl.addressGenUsed,globalPermanent);
    }
  } else {
    FUInstance* inst = CreateFUInstanceWithParameters(circuit,type,var.name,decl);
    table->Insert(inst->name,inst);
    inst->addressGenUsed = CopyArray<String,Token>(decl.addressGenUsed,globalPermanent);

    ent->type = EntityType_FU;
    ent->instance = inst;
  }

  for(auto iter = StartIteration(this,ent); iter.IsValid(); iter = iter.Next()){
    FUInstance* inst = iter.Current();
    
    switch(decl.modifier){
    case InstanceDeclarationType_NONE: break;
    case InstanceDeclarationType_SHARE_CONFIG:{
      ShareInstanceConfig(inst,decl.shareIndex);
      
      for(Token partialShareName : decl.shareNames){
        bool foundOne = false;
        for(int ii = 0; ii < inst->declaration->configs.size; ii++){
          if(inst->declaration->configs[ii].name == partialShareName){
            inst->isSpecificConfigShared[ii] = false;
            foundOne = true;
          }
        }

        if(!foundOne){
          TEMP_REGION(temp,nullptr);
          String errorMsg = PushString(temp,"Cannot share config wire since it does not exist for instance '%.*s' of type '%.*s'",UN(inst->name),UN(decl.typeName));
          ReportError(partialShareName,errorMsg);
        }
      }
    } break;
    case InstanceDeclarationType_STATIC:{
      SetStatic(inst);
    } break;
    }
  }
}

void Env::AddConnection(ConnectionDef decl){
  Assert(decl.type == ConnectionType_CONNECTION);

  int nOutConnections = NumberOfConnections(decl.output);
  int nInConnections = NumberOfConnections(decl.input);

  if(nOutConnections != nInConnections){
    ReportError({},"Connection missmatch");
  }

  GroupIterator out = IterateGroup(decl.output);
  GroupIterator in  = IterateGroup(decl.input);

  while(HasNext(out) && HasNext(in)){
    Var outVar = Next(out);
    Var inVar = Next(in);
        
    Assert(inVar.extra.delay.high == 0); // For now, inputs cannot have delay.
        
    FUInstance* outInstance = GetFUInstance(outVar);
    FUInstance* inInstance = GetFUInstance(inVar);

    int outPort = outVar.extra.port.low;
    int inPort  = inVar.extra.port.low;
    ConnectUnits(outInstance,outPort,inInstance,inPort,outVar.extra.delay.low);
  }

  Assert(HasNext(out) == HasNext(in));
}

void Env::AddEquality(ConnectionDef decl){
  TEMP_REGION(temp,nullptr);
  
  Assert(decl.type == ConnectionType_EQUALITY);

  // Only allow one for equality, for now
  Assert(decl.output.vars.size == 1);

  Var outVar = decl.output.vars[0];
  PortExpression portSpecExpression = InstantiateSpecExpression(decl.expression);

  // When dealing with equality, we can just increase array size by accessing higher and higher values.
  if(outVar.isArrayAccess){
    auto res = this->scopes[this->currentScope]->variable->GetOrAllocate(outVar.name);
    Entity* ent = res.data;

    ent->type = EntityType_FU_ARRAY;
    ent->arrayBaseName = outVar.name;
    ent->arraySize = MAX(ent->arraySize,outVar.index.low);
  }

  String name = outVar.name;
  if(outVar.isArrayAccess){
    name = GetActualArrayName(name,outVar.index.low,temp);
  }

  FUInstance* inst = portSpecExpression.inst;
  String uniqueName = GetUniqueName(name,globalPermanent,table);
  inst->name = PushString(globalPermanent,uniqueName);

  Token tok = {};
  tok = *((Token*) &inst->name);
  Entity* ent = PushNewEntity(tok);
  ent->type = EntityType_FU;
  ent->instance = inst;

  table->Insert(inst->name,inst);
}

Opt<Entity> GetEntityFromHierAccessWithEnvironment(AccelInfo* info,Env* env,Array<String> accessExpr){
#if 1
  if(accessExpr.size == 1){
    String name = accessExpr[0];
    int currentScope = env->currentScope;

    // Up the scope chain.
    for(; currentScope >= 0; currentScope -= 1){
      for(Pair<String,Entity> p : env->scopes[env->currentScope]->variable){
        String str = p.first;
        if(str == name){
          return p.second;
        }
      }
    }
  }

  return GetEntityFromHierAccess(info,accessExpr);
#endif
}

FUInstanceIterator FUInstanceIterator::Next(){
  FUInstanceIterator next = *this;
  next.index += 1;
  return next;
}

bool FUInstanceIterator::IsValid(){
  bool res;
  if(max == 0){
    res = (index == 0);
  } else {
    res = (index < max);
  }
  
  return res;
}

FUInstance* FUInstanceIterator::Current(){
  Assert(IsValid());

  FUInstance* inst;
  if(this->max == 0){
    return ent->instance;
  } else {
    TEMP_REGION(temp,nullptr);
    String baseName = ent->arrayBaseName;
    String actualName = GetActualArrayName(baseName,index,temp);

    inst = GetUnit(this->env->circuit,actualName);
  }
  
  return inst;
}

FUInstanceIterator StartIteration(Env* env,Entity* ent){
  FUInstanceIterator iter = {};
  iter.env = env;
  iter.ent = ent;

  if(ent->type == EntityType_FU_ARRAY){
    iter.max = ent->arraySize;
  }

  return iter;
}

PortExpression Env::InstantiateSpecExpression(SpecExpression* root){
  Arena* perm = globalPermanent;
  PortExpression res = {};

  switch(root->type){
    // Just to remove warnings. TODO: Change expression so that multiple locations have their own expression struct, instead of reusing the same one.
  case SpecType_LITERAL:{
    int number = root->val.number;

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
    String name = var.name;

    if(var.isArrayAccess){
      name = GetActualArrayName(var.name,var.index.bottom,globalPermanent);
    }
    
    FUInstance* inst = table->GetOrFail(name);

    res.inst = inst;
    res.extra = var.extra;
  } break;
  case SpecType_OPERATION:{
    PortExpression expr0 = InstantiateSpecExpression(root->expressions[0]);

    // Assuming right now very simple cases, no port range and no delay range
    Assert(expr0.extra.port.start == expr0.extra.port.end);
    Assert(expr0.extra.delay.start == expr0.extra.delay.end);

    if(root->expressions.size == 1){
      Assert(root->op[0] == '~' || root->op[0] == '-');

      String typeName = {};

      switch(root->op[0]){
      case '~':{
        typeName = "NOT";
      }break;
      case '-':{
        typeName = "NEG";
      }break;
      }

      String permName = GetUniqueName(typeName,perm,table);
      FUInstance* inst = CreateInstance(GetTypeByName(typeName),permName);
      table->Insert(inst->name,inst);

      ConnectUnits(expr0.inst,expr0.extra.port.start,inst,0,expr0.extra.delay.start);

      res.inst = inst;
      res.extra.port.end  = res.extra.port.start  = 0;
      res.extra.delay.end = res.extra.delay.start = 0;

      return res;
    } else {
      Assert(root->expressions.size == 2);
    }

    PortExpression expr1 = InstantiateSpecExpression(root->expressions[1]);

    // Assuming right now very simple cases, no port range and no delay range
    Assert(expr1.extra.port.start == expr1.extra.port.end);
    Assert(expr1.extra.delay.start == expr1.extra.delay.end);

    String op = root->op;
    const char* typeName;
    if(CompareString(op,"&")){
      typeName = "AND";
    } else if(CompareString(op,"|")){
      typeName = "OR";
    } else if(CompareString(op,"^")){
      typeName = "XOR";
    } else if(CompareString(op,">><")){
      typeName = "RHR";
    } else if(CompareString(op,">>")){
      typeName = "SHR";
    } else if(CompareString(op,"><<")){
      typeName = "RHL";
    } else if(CompareString(op,"<<")){
      typeName = "SHL";
    } else if(CompareString(op,"+")){
      typeName = "ADD";
    } else if(CompareString(op,"-")){
      typeName = "SUB";
    } else {
      // TODO: Proper error reporting
      printf("%.*s\n",UN(op));
      Assert(false);
    }

    String typeStr = typeName;
    FUDeclaration* type = GetTypeByName(typeStr);
    String uniqueName = GetUniqueName(type->name,perm,table);

    FUInstance* inst = CreateInstance(type,uniqueName);
    table->Insert(inst->name,inst);

    ConnectUnits(expr0.inst,expr0.extra.port.start,inst,0,expr0.extra.delay.start);
    ConnectUnits(expr1.inst,expr1.extra.port.start,inst,1,expr1.extra.delay.start);

    res.inst = inst;
    res.extra.port.end  = res.extra.port.start  = 0;
    res.extra.delay.end = res.extra.delay.start = 0;
  } break;
  }

  Assert(res.inst);
  return res;
}
