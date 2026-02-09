#include "versatSpecificationParser.hpp"

#include "accelerator.hpp"
#include "debug.hpp"
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

#include "newParser.hpp"

// ======================================
// Constants

static SpecExpression SPEC_LITERAL_0 = {.val = 0,.type = SpecExpression::LITERAL};

// nocheckin
// TODO: Need to take an Env to properly check stuff.
SymbolicExpression* SymbolicFromSpecExpression(SpecExpression* spec,Arena* out){
  TEMP_REGION(temp,out);  

  auto Recurse = [temp](auto Recurse,SpecExpression* top) -> SymbolicExpression*{
    SymbolicExpression* res = nullptr;

    switch(top->type){
    case SpecExpression::OPERATION:{
      SymbolicExpression* left  = Recurse(Recurse,top->expressions[0]);
      SymbolicExpression* right = Recurse(Recurse,top->expressions[1]);

      if(top->op[0] == '+'){
        res = SymbolicAdd(left,right,temp);
      }
      if(top->op[0] == '-'){
        res = SymbolicSub(left,right,temp);
      }
      if(top->op[0] == '*'){
        res = SymbolicMult(left,right,temp);
      }
      if(top->op[0] == '/'){
        res = SymbolicDiv(left,right,temp);
      }
    } break;
    case SpecExpression::NAME:
    case SpecExpression::VAR:{
      res = PushVariable(temp,top->name);
    } break;
    case SpecExpression::LITERAL:{
      res = PushLiteral(temp,top->val);
    } break;
    case SpecExpression::SINGLE_ACCESS:
    case SpecExpression::ARRAY_ACCESS:
    case SpecExpression::FUNCTION_CALL: Assert(false);
    }

    return res;
  };

  SymbolicExpression* res = Recurse(Recurse,spec);

  return Normalize(res,out);
}

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
    expr->val = digit;
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

// nocheckin
SpecExpression* ParseSpecExpression2(Parser* parser,Arena* out,int bindingPower = 99);

bool Equal(SpecExpression* left,SpecExpression* right){
  if(left->type != right->type){
    return false;
  }
  
  if(left->type == SpecExpression::OPERATION){
    if(left->op != right->op){
      return false;
    }

    if(left->expressions.size != right->expressions.size){
      return false;
    }

    for(int i = 0; i < left->expressions.size; i++){
      if(!Equal(left->expressions[i],right->expressions[i])){
        return false;
      }
    }
  }

  if(left->type == SpecExpression::VAR){
    if(left->var.name != right->var.name){
      return false;
    }
  }

  if(left->type == SpecExpression::LITERAL){
    if(left->val != right->val){
      return false;
    }
  }

  return true;
}

SpecExpression* ParseSpecExpression(Tokenizer* tok,Arena* out){
  TEMP_REGION(temp,out);
  String res = tok->PeekRemainingLine();

  auto TokenizeFunction = [](const char* start,const char* end) -> TokenizeResult{
    TokenizeResult res = ParseWhitespace(start,end);
    res |= ParseComments(start,end);

    res |= ParseMultiSymbol(start,end,">><",NewTokenType_ROTATE_RIGHT);
    res |= ParseMultiSymbol(start,end,"><<",NewTokenType_ROTATE_LEFT);

    res |= ParseMultiSymbol(start,end,"..",NewTokenType_DOUBLE_DOT);
    res |= ParseMultiSymbol(start,end,"##",NewTokenType_DOUBLE_HASHTAG);
    res |= ParseMultiSymbol(start,end,"->",NewTokenType_ARROW);
    res |= ParseMultiSymbol(start,end,">>",NewTokenType_SHIFT_RIGHT);
    res |= ParseMultiSymbol(start,end,"<<",NewTokenType_SHIFT_LEFT);

    res |= ParseSymbols(start,end);
    res |= ParseNumber(start,end);

    res |= ParseIdentifier(start,end);

    if(res.token.type == NewTokenType_IDENTIFIER){
      String id = res.token.identifier;
      
      NewTokenType type = NewTokenType_INVALID;

      if(id == "module")     type = NewTokenType_KEYWORD_MODULE;
      if(id == "merge")      type = NewTokenType_KEYWORD_MERGE;
      if(id == "share")      type = NewTokenType_KEYWORD_SHARE;
      if(id == "static")     type = NewTokenType_KEYWORD_STATIC;
      if(id == "debug")      type = NewTokenType_KEYWORD_DEBUG;
      if(id == "config")     type = NewTokenType_KEYWORD_CONFIG;
      if(id == "state")      type = NewTokenType_KEYWORD_STATE;
      if(id == "mem")        type = NewTokenType_KEYWORD_MEM;

      if(type != NewTokenType_INVALID){
        res.token.type = type;
      }
    }

    return res;
  };
 
  
  Parser* parser = StartParsing(res,TokenizeFunction,temp);

  SpecExpression* test = ParseSpecExpression2(parser,temp);
  
  SpecExpression* expr = ParseOperationType<SpecExpression>(tok,{{"+","-"},{"&","|","^"},{">><",">>","><<","<<"}},ParseTerm,out);

  Assert(Equal(test,expr));
  
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
  case SpecType_SINGLE_ACCESS:{
    Assert(false);
  } break;
  case SpecType_ARRAY_ACCESS:{
    Assert(false);
  } break;
  case SpecType_FUNCTION_CALL:{
    Assert(false);
  } break;
  case SpecType_LITERAL:{
    int number = root->val;

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
  case SpecType_NAME:{
    Assert(false);
  } break;
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
    // NOTE: This is actually wrong, it is not equality but we only care about it if we end up implementing more
    //       x= forms.
    def.type = ConnectionType_EQUALITY;
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
      *list->PushElem() = InstantiateConfigFunction(env,&funcDecl,res,content,globalPermanent);
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

// nocheckin
Array<ConstructDef> ParseVersatSpecification2(String content,Arena* out);

Array<ConstructDef> ParseVersatSpecification(String content,Arena* out){
  return ParseVersatSpecification2(content,out);
  
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
    } else {
      ReportError(tok,peek,"Unexpected token in global scope");
      tok->AdvancePeek();
      Synchronize(tok,{"module","merge"});
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
} END_SWITCH();

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
  } END_SWITCH();

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

Entity* Env::GetEntity(ConfigIdentifier* id,Arena* out){
  TEMP_REGION(temp,out);

  ConfigIdentifier* ptr = id;
  Assert(ptr->type == ConfigAccessType_BASE);

  Entity* ent = GetEntity(ptr->name);
  
  ptr = ptr->parent;
  for(; ent && ptr; ptr = ptr->parent){
    Entity* nextEnt = nullptr;

    switch(ptr->type){
      case ConfigAccessType_BASE:{
        Assert(false);
      } break;
      case ConfigAccessType_ARRAY:{
        if(ent->type != EntityType_FU_ARRAY){
          ReportError({},"Cannot access array since entity is not an array");
        }

        NOT_IMPLEMENTED();

#if 0
        // TODO: We need to be able to calculate the index at this point.
        //       This also means that the symbolic expression must be a constant, right? Or at least something that we can calculate directly.
        //       If we are inside a for loop and we are using a loop variable than the problem becomes more complicated, but currently we are trying to simplify this part of the code meaning that we will tackle this in the future if needed.
        if(ent->type == EntityType_FU_ARRAY){
          String name = GetActualArrayName(ent->arrayBaseName,index.low,temp);
        }
#endif
        
      } break;
      case ConfigAccessType_ACCESS:{
        Token access = ptr->name;

        SWITCH(ent->type){
        case EntityType_FU:{
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
            Entity* virtualWire = PushStruct<Entity>(out);
            virtualWire->type = EntityType_MEM_PORT;
            virtualWire->dir = dir;
            virtualWire->port = port;
            virtualWire->parent = ent;
            
            if(nextEnt) ReportError({},"Name collision");
            nextEnt = virtualWire;
          } 
          
          FUDeclaration* decl = ent->instance->declaration;
            
          // TODO: We need to check if we have the same name for stuff. If
          //       wire has the same name as a userConfig function then we have a problem and this
          //       code will not act correctly. Furthermore, this check needs to be done elsewhere.
          //       By the time we reach this point we are probably too far. Otherwise we need to 
          for(Wire& w : decl->configs){
            if(w.name == access){
              Entity* wire = PushStruct<Entity>(out);
              wire->wire = &w;
              wire->type = EntityType_CONFIG_WIRE;
              
              if(nextEnt) ReportError({},"Name collision");
              nextEnt = wire;
              break;
            }
          }

          for(Wire& w : decl->states){
            if(w.name == access){
              Entity* wire = PushStruct<Entity>(out);
              wire->wire = &w;
              wire->type = EntityType_STATE_WIRE;
              
              if(nextEnt) ReportError({},"Name collision");
              nextEnt = wire;
              break;
            }
          }
 
          for(MergePartition part : decl->info.infos){
            for(ConfigFunction* func : part.userFunctions){
              if(func->individualName == access){
                Entity* userFunc = PushStruct<Entity>(out);

                userFunc->func = func;
                userFunc->type = EntityType_CONFIG_FUNCTION;
          
                if(nextEnt) ReportError({},"Name collision");
                nextEnt = userFunc;
              }
            }
          }
        } break;
        case EntityType_NODE:{
          Assert(false); // We do not have nodes since Env is only used currently to store FUInstances and other parseed data stuff. Nothing concrete exists at this point in the code.
        } break;

        case EntityType_CONFIG_FUNCTION:
        case EntityType_FU_ARRAY:
        case EntityType_MEM_PORT:
        case EntityType_CONFIG_WIRE:
        case EntityType_STATE_WIRE:
        case EntityType_VARIABLE_INPUT:
        case EntityType_VARIABLE_SPECIAL:{
          ReportError(access,"Cannot have access expression for entity of this type");
        } break;
      }
      } break;
    }

    ent = nextEnt;
  }

  return ent;
}

Entity* Env::GetEntity(SpecExpression* id,Arena* out){
  TEMP_REGION(temp,out);

  SpecExpression* ptr = id;

  Entity* ent = nullptr;

  if(id->type == SpecExpression::NAME){
    ent = GetEntity(id->name);
  }
  if(id->type == SpecExpression::ARRAY_ACCESS){
    NOT_IMPLEMENTED();
  }
  if(id->type == SpecExpression::SINGLE_ACCESS){
    ent = GetEntity(id->name);
    
    SpecExpression* accessExpr = id->expressions[0];
    Assert(accessExpr->type == SpecExpression::NAME);
    
    Token access = accessExpr->name;

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

    Entity* nextEnt = nullptr;
    if(dir != Direction_NONE){
      Entity* virtualWire = PushStruct<Entity>(out);
      virtualWire->type = EntityType_MEM_PORT;
      virtualWire->dir = dir;
      virtualWire->port = port;
      virtualWire->parent = ent;
            
      nextEnt = virtualWire;
    } 
          
    FUDeclaration* decl = ent->instance->declaration;
            
    // TODO: We need to check if we have the same name for stuff. If
    //       wire has the same name as a userConfig function then we have a problem and this
    //       code will not act correctly. Furthermore, this check needs to be done elsewhere.
    //       By the time we reach this point we are probably too far. Otherwise we need to 
    for(Wire& w : decl->configs){
      if(w.name == access){
        Entity* wire = PushStruct<Entity>(out);
        wire->wire = &w;
        wire->type = EntityType_CONFIG_WIRE;
              
        if(nextEnt) ReportError({},"Name collision");
        nextEnt = wire;
        break;
      }
    }

    for(Wire& w : decl->states){
      if(w.name == access){
        Entity* wire = PushStruct<Entity>(out);
        wire->wire = &w;
        wire->type = EntityType_STATE_WIRE;
              
        if(nextEnt) ReportError({},"Name collision");
        nextEnt = wire;
        break;
      }
    }
 
    for(MergePartition part : decl->info.infos){
      for(ConfigFunction* func : part.userFunctions){
        if(func->individualName == access){
          Entity* userFunc = PushStruct<Entity>(out);

          userFunc->func = func;
          userFunc->type = EntityType_CONFIG_FUNCTION;
          
          if(nextEnt) ReportError({},"Name collision");
          nextEnt = userFunc;
        }
      }
    }
    
    if(nextEnt){
      nextEnt->parent = ent;
      ent = nextEnt;
    }
  }

  return ent;
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
      //inst->addressGenUsed = CopyArray<String,Token>(decl.addressGenUsed,globalPermanent);
    }
  } else {
    FUInstance* inst = CreateFUInstanceWithParameters(circuit,type,var.name,decl);
    table->Insert(inst->name,inst);
    //inst->addressGenUsed = CopyArray<String,Token>(decl.addressGenUsed,globalPermanent);

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

// nochecking
// TODO: Just to remove the syntax errors from the compiler
Token C(NewToken t){
  Token res = {};
  res.data = t.ptr;
  res.size = t.identifier.size;
  return res;
}

Range<SpecExpression*> ParseRange2(Parser* parser,Arena* out){
  Range<SpecExpression*> res = {};

  SpecExpression* n1 = ParseSpecExpression2(parser,out);

  res.start = n1;
  res.end = n1;

  if(parser->IfNextToken(NewTokenType_DOUBLE_DOT)){
    res.end = ParseSpecExpression2(parser,out);
  }
  
  return res;
}

Var ParseVar2(Parser* parser,Arena* out){
  Var var = {};

  Token name = C(parser->ExpectNext(NewTokenType_IDENTIFIER));

  if(parser->IfNextToken('[')){
    Range<SpecExpression*> range = ParseRange2(parser,out);

    var.isArrayAccess = true;
    var.trueIndex = range;

    parser->ExpectNext(']');
  }
  
  SpecExpression* delayStart = &SPEC_LITERAL_0;
  SpecExpression* delayEnd = &SPEC_LITERAL_0;
  if(parser->IfNextToken('{')){
    Range<SpecExpression*> range = ParseRange2(parser,out);
    delayStart = range.start;
    delayEnd = range.end;

    parser->ExpectNext('}');
  }

  SpecExpression* portStart = &SPEC_LITERAL_0;
  SpecExpression* portEnd = &SPEC_LITERAL_0;
  if(parser->IfNextToken(':')){
    Range<SpecExpression*> range = ParseRange2(parser,out);

    portStart = range.start;
    portEnd = range.end;
  }

  var.name = name;
  var.trueExtra.delay.start = delayStart;
  var.trueExtra.delay.end = delayEnd;
  var.trueExtra.port.start = portStart;
  var.trueExtra.port.end = portEnd;

  return var;
}

VarDeclaration ParseVarDeclaration2(Parser* parser){
  VarDeclaration res = {};

  res.name = C(parser->ExpectNext(NewTokenType_IDENTIFIER));
  
  // TODO: We should integrate the array parsing logic with this one
  if(parser->IfNextToken('[')){
    NewToken number = parser->ExpectNext(NewTokenType_NUMBER);
    int arraySize = number.number;

    parser->ExpectNext(']');

    res.arraySize = arraySize;
    res.isArray = true;
  }
  
  return res;
}

Array<VarDeclaration> ParseModuleInputDeclaration2(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);

  auto vars = PushArenaList<VarDeclaration>(temp);

  parser->ExpectNext('(');

  if(parser->IfNextToken(')')){
    return {};
  }
  
  while(!parser->Done()){
    VarDeclaration var = ParseVarDeclaration2(parser);
    *vars->PushElem() = var;
    
    if(parser->IfNextToken(',')){
      continue;
    } else {
      break;
    }
  }

  parser->ExpectNext(')');

  Array<VarDeclaration> res = PushArrayFromList(out,vars);
  return res;
}

InstanceDeclaration ParseInstanceDeclaration2(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);
  InstanceDeclaration res = {};

  InstanceDeclarationType modifier = InstanceDeclarationType_NONE;

  // TODO: Is it correct if we see a bunch of repeated modifiers? Think 'static static static'.
  while(1){
    NewToken potentialModifier = parser->PeekToken();

    InstanceDeclarationType parsedModifier = InstanceDeclarationType_NONE;
    if(potentialModifier.type == NewTokenType_KEYWORD_DEBUG){
      parser->NextToken();

      res.debug = true;
    } else if(potentialModifier.type == NewTokenType_KEYWORD_STATIC){
      parser->NextToken();
      parsedModifier = InstanceDeclarationType_STATIC;
    } else if(potentialModifier.type == NewTokenType_KEYWORD_SHARE){
      parser->NextToken();
      parser->ExpectNext('(');
      parser->ExpectNext(NewTokenType_KEYWORD_CONFIG);
      parser->ExpectNext(')');

      parsedModifier = InstanceDeclarationType_SHARE_CONFIG;
      
      res.typeName = C(parser->ExpectNext(NewTokenType_IDENTIFIER));

      if(parser->IfNextToken('(')){
        // TODO: For now, we assume that every wire specified inside the spec file is a negative (remove share).
        auto toShare = StartArray<Token>(out);
        while(!parser->Done()){
          Token name = C(parser->ExpectNext(NewTokenType_IDENTIFIER));

          *toShare.PushElem() = name;

          if(parser->IfNextToken(',')){
            continue;
          } else {
            break;
          }
        }
      
        parser->ExpectNext(')');

        res.shareNames = EndArray(toShare);
      }

      parser->ExpectNext('{');

      auto array = StartArray<VarDeclaration>(out);
    
      while(!parser->Done()){
        NewToken peek = parser->PeekToken();

        if(peek.type == '}'){
          break;
        }

        *array.PushElem() = ParseVarDeclaration2(parser);
      
        parser->ExpectNext(';');
      }
      res.declarations = EndArray(array);
    
      parser->ExpectNext('}');
      return res;
    } else {
      break;
    }

    modifier = modifier | parsedModifier;
  }

  res.typeName = C(parser->ExpectNext(NewTokenType_IDENTIFIER));

  NewToken possibleParameters = parser->PeekToken();
  auto list = PushArenaList<Pair<String,SpecExpression*>>(temp);
  if(possibleParameters.type == '#'){
    parser->NextToken();
    parser->ExpectNext('(');

    while(!parser->Done()){
      parser->ExpectNext('.');
      Token parameterName = C(parser->ExpectNext(NewTokenType_IDENTIFIER));

      parser->ExpectNext('(');

      SpecExpression* expr = ParseSpecExpression2(parser,out);

      parser->ExpectNext(')');
      
      String savedParameter = PushString(out,parameterName);
      *list->PushElem() = {savedParameter,expr}; 

      if(parser->IfNextToken(',')){
        continue;
      }

      break;
    }
    parser->ExpectNext(')');

    res.parameters2 = PushArrayFromList(out,list);
  }

  VarDeclaration varDecl = ParseVarDeclaration2(parser);

  res.declarations = PushArray<VarDeclaration>(out,1);
  res.declarations[0] = varDecl;

  parser->ExpectNext(';');

  return res;
}

// A group can also be a single var. It does not necessary mean that it must be of the form {...}
VarGroup ParseVarGroup2(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);
  
  if(parser->IfNextToken('{')){
    auto vars = PushArenaList<Var>(temp);

    while(!parser->Done()){
      Var var = ParseVar2(parser,out);

      *vars->PushElem() = var;

      NewToken sepOrEnd = parser->NextToken();
      if(sepOrEnd.type == ','){
        continue;
      } else if(sepOrEnd.type == '}'){
        break;
      } else {
        parser->ReportUnexpectedToken(sepOrEnd,{TOK_TYPE(','),TOK_TYPE('}')});
      }
    }

    VarGroup res = {};
    res.vars = PushArrayFromList(out,vars);
    return res;
  } else {
    Var var = ParseVar2(parser,out);

    VarGroup res = {};
    res.vars = PushArray<Var>(out,1);
    res.vars[0] = var;
    return res;
  }
}

// nocheckin 
// TODO: We do actually want to separate the parsing from a connection expression and the expressions used by the address gen stuff.
//       The problem that we had before is that we parsed symbolic directly instead of parsing into an ExpressionStruct and then we tried to check the semantic.
//       Basically, in the past, instead of doing Text -> Tokens -> Symbolic we where doing Text -> Symbolic which is the problem.
SpecExpression* ParseLoopExpression2(Parser* parser,Arena* out,int bindingPower = 99);

SpecExpression* ParseLoopExpression2(Parser* parser,Arena* out,int bindingPower){
  SpecExpression* topUnary = nullptr;
  SpecExpression* innerMostUnary = nullptr;

  SpecExpression* res = nullptr;
  
  // Parse unary
  while(!parser->Done()){
    SpecExpression* parsed = nullptr;
    if(!parsed &&  parser->IfNextToken('-')){
      parsed = PushStruct<SpecExpression>(out);
      parsed->op = "-";
    }

    if(parsed){
      parsed->type = SpecExpression::OPERATION;
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
  NewToken atom = parser->PeekToken();
  if(atom.type == '('){
    parser->ExpectNext('(');

    res = ParseSpecExpression2(parser,out);

    parser->ExpectNext(')');
  } else if(atom.type == NewTokenType_NUMBER){
    NewToken number = parser->ExpectNext(NewTokenType_NUMBER);
    res = PushStruct<SpecExpression>(out);

    res->type = SpecExpression::LITERAL;
    res->val = number.number;
  } else if(atom.type == NewTokenType_IDENTIFIER){
    TEMP_REGION(temp,out);

    Token name = C(parser->ExpectNext(NewTokenType_IDENTIFIER));
    
    res = PushStruct<SpecExpression>(out);
    res->name = name;
    res->type = SpecExpression::NAME;

    if(parser->IfPeekToken('[')){
      auto accesses = PushArenaList<SpecExpression*>(temp);       
      
      while(parser->IfNextToken('[')){
        SpecExpression* insideArray = ParseSpecExpression2(parser,out);

        *accesses->PushElem() = insideArray;

        parser->ExpectNext(']');
      }

      res->expressions = PushArrayFromList(out,accesses);
      res->type = SpecExpression::ARRAY_ACCESS;
    }

    // TODO: This is mostly for state right side.
    //       We might eventually just separate this into different parsing functions.
    if(parser->IfNextToken('.')){
      NewToken singleAccessName = parser->ExpectNext(NewTokenType_IDENTIFIER);
      
      SpecExpression* singleAccess = PushStruct<SpecExpression>(out);
      singleAccess->type = SpecExpression::NAME;
      singleAccess->name = C(singleAccessName);
      
      res->expressions = PushArray<SpecExpression*>(out,1);
      res->expressions[0] = singleAccess;
      res->type = SpecExpression::SINGLE_ACCESS;
    }

    if(parser->IfNextToken('(')){
      auto args = PushArenaList<SpecExpression*>(temp);       
      
      while(!parser->Done()){
        SpecExpression* arg = ParseSpecExpression2(parser,out);

        *args->PushElem() = arg;
        
        if(parser->IfNextToken(',')){
          continue;
        }
        
        break;
      }

      parser->ExpectNext(')');
      
      res->expressions = PushArrayFromList(out,args);
      res->type = SpecExpression::FUNCTION_CALL;
    }
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
    NewTokenType type;
    int bindingPower;
    const char* op;
  };

  // TODO: This should be outside the function itself.
  TEMP_REGION(temp,out);
  auto infos = PushArray<OpInfo>(temp,4);
  infos[0] = {TOK_TYPE('/'),0,"&"};
  infos[1] = {TOK_TYPE('*'),0,"|"};

  infos[2] = {TOK_TYPE('+'),1,"+"};
  infos[3] = {TOK_TYPE('-'),1,"-"};
  
  // Parse binary ops.
  while(!parser->Done()){
    NewToken peek = parser->PeekToken();

    bool continueOuter = false;
    for(OpInfo info : infos){
      if(peek.type == info.type){
        if(info.bindingPower < bindingPower){
          parser->NextToken();

          SpecExpression* right = ParseSpecExpression2(parser,out,info.bindingPower);
      
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

SpecExpression* ParseSpecExpression2(Parser* parser,Arena* out,int bindingPower){
  SpecExpression* topUnary = nullptr;
  SpecExpression* innerMostUnary = nullptr;

  SpecExpression* res = nullptr;
  
  // Parse unary
  while(!parser->Done()){
    SpecExpression* parsed = nullptr;
    if(!parsed && parser->IfNextToken('~')){
      parsed = PushStruct<SpecExpression>(out);
      parsed->op = "~";
    }
    if(!parsed &&  parser->IfNextToken('-')){
      parsed = PushStruct<SpecExpression>(out);
      parsed->op = "-";
    }

    if(parsed){
      parsed->type = SpecExpression::OPERATION;
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
  NewToken atom = parser->PeekToken();
  if(atom.type == '('){
    parser->ExpectNext('(');

    res = ParseSpecExpression2(parser,out);

    parser->ExpectNext(')');
  } else if(atom.type == NewTokenType_NUMBER){
    NewToken number = parser->ExpectNext(NewTokenType_NUMBER);
    res = PushStruct<SpecExpression>(out);

    res->type = SpecExpression::LITERAL;
    res->val = number.number;
  } else if(atom.type == NewTokenType_IDENTIFIER){
    TEMP_REGION(temp,out);

    Var var = ParseVar2(parser,out);
    
    res = PushStruct<SpecExpression>(out);
    res->var = var;
    res->type = SpecExpression::VAR;

    if(parser->IfPeekToken('[')){
      auto accesses = PushArenaList<SpecExpression*>(temp);       
      
      while(parser->IfNextToken('[')){
        SpecExpression* insideArray = ParseSpecExpression2(parser,out);

        *accesses->PushElem() = insideArray;

        parser->ExpectNext(']');
      }

      res->expressions = PushArrayFromList(out,accesses);
      res->type = SpecExpression::ARRAY_ACCESS;
    }

    // TODO: This is mostly for state right side.
    //       We might eventually just separate this into different parsing functions.
    if(parser->IfNextToken('.')){
      NewToken singleAccessName = parser->ExpectNext(NewTokenType_IDENTIFIER);
      
      SpecExpression* singleAccess = PushStruct<SpecExpression>(out);
      singleAccess->type = SpecExpression::NAME;
      singleAccess->name = C(singleAccessName);
      
      res->expressions = PushArray<SpecExpression*>(out,1);
      res->expressions[0] = singleAccess;
      res->type = SpecExpression::SINGLE_ACCESS;
    }

    if(parser->IfNextToken('(')){
      auto args = PushArenaList<SpecExpression*>(temp);       
      
      while(!parser->Done()){
        SpecExpression* arg = ParseSpecExpression2(parser,out);

        *args->PushElem() = arg;
        
        if(parser->IfNextToken(',')){
          continue;
        }
        
        break;
      }

      parser->ExpectNext(')');
      
      res->expressions = PushArrayFromList(out,args);
      res->type = SpecExpression::FUNCTION_CALL;
    }
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
    NewTokenType type;
    int bindingPower;
    const char* op;
  };

  // TODO: This should be outside the function itself.
  TEMP_REGION(temp,out);
  auto infos = PushArray<OpInfo>(temp,11);
  // TODO: Need to check binding strength
  infos[0] = {TOK_TYPE('&'),0,"&"};
  infos[1] = {TOK_TYPE('|'),0,"|"};
  infos[2] = {TOK_TYPE('^'),0,"^"};

  infos[3] = {NewTokenType_ROTATE_LEFT,1,"><<"};
  infos[4] = {NewTokenType_ROTATE_RIGHT,1,">><"};
  infos[5] = {NewTokenType_SHIFT_LEFT,1,"<<"};
  infos[6] = {NewTokenType_SHIFT_RIGHT,1,">>"};

  infos[7] = {TOK_TYPE('+'),2,"+"};
  infos[8] = {TOK_TYPE('-'),2,"-"};

  infos[9]  = {TOK_TYPE('*'),3,"*"};
  infos[10] = {TOK_TYPE('/'),3,"/"};
  
  // Parse binary ops.
  while(!parser->Done()){
    NewToken peek = parser->PeekToken();

    bool continueOuter = false;
    for(OpInfo info : infos){
      if(peek.type == info.type){
        if(info.bindingPower < bindingPower){
          parser->NextToken();

          SpecExpression* right = ParseSpecExpression2(parser,out,info.bindingPower);
      
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

ConnectionDef ParseConnection2(Parser* parser,Arena* out){
  VarGroup outPortion = ParseVarGroup2(parser,out);

  ConnectionType type = ConnectionType_NONE;
  
  if(parser->IfNextToken('=')){
    type = ConnectionType_EQUALITY;
  } else if(parser->IfNextToken(NewTokenType_XOR_EQUAL)){
    // TODO: We parse it but we do not use it. We probably wanna remove the testcase that uses this.
    type = ConnectionType_EQUALITY;
  } else if(parser->IfNextToken(NewTokenType_ARROW)){
    type = ConnectionType_CONNECTION;
  } else {
    parser->ReportUnexpectedToken(parser->NextToken(),{TOK_TYPE('='),NewTokenType_ARROW});
  }

  ConnectionDef def = {};
  SpecExpression* expr = nullptr;
  VarGroup inPortion = {};

  if(type == ConnectionType_EQUALITY){
    expr = ParseSpecExpression2(parser,out);
  } else if(type == ConnectionType_CONNECTION){
    inPortion = ParseVarGroup2(parser,out);
  }

  parser->ExpectNext(';');
  
  def.expression = expr;
  def.output = outPortion;
  def.input = inPortion;

  return def;
}

// TODO: nocheckin - remove forward decl
ConfigFunctionDef* ParseConfigFunction2(Parser* parser,Arena* out);

// Any returned String points to tokenizer content.
// As long as tokenizer is valid, strings returned by this function are also valid.
ModuleDef ParseModuleDef2(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);

  parser->ExpectNext(NewTokenType_KEYWORD_MODULE);

  Token name = C(parser->ExpectNext(NewTokenType_IDENTIFIER));
  Array<VarDeclaration> vars = ParseModuleInputDeclaration2(parser,out);

  //Token outputs = {};
  if(parser->IfNextToken(NewTokenType_ARROW)){
    // TODO: Right now we do not care about output info being given by the user.
    //       We probably should. We still parse it, we just ignore it for now.
    /* outputs = */ parser->ExpectNext(NewTokenType_NUMBER);
  }
  
  ArenaList<InstanceDeclaration>* decls = PushArenaList<InstanceDeclaration>(temp);
  parser->ExpectNext('{');
  
  while(!parser->Done()){
    NewToken peek = parser->PeekToken();

    if(peek.type == ';'){
      parser->NextToken();
      continue;
    }

    if(peek.type == '#' || peek.type == NewTokenType_DOUBLE_HASHTAG || peek.type == '}'){
      break;
    }
    
    InstanceDeclaration decl = ParseInstanceDeclaration2(parser,out);
    *decls->PushElem() = decl;
  }
  Array<InstanceDeclaration> declarations = PushArrayFromList(out,decls);

  ArenaList<ConnectionDef>* cons = PushArenaList<ConnectionDef>(temp);
  if(parser->IfNextToken('#')){
    while(!parser->Done()){
      NewToken peek = parser->PeekToken();

      if(peek.type == ';'){
        parser->NextToken();
        continue;
      }

      if(peek.type == '}' || peek.type == NewTokenType_DOUBLE_HASHTAG){
        break;
      }

      ConnectionDef con = ParseConnection2(parser,out);
 
      *cons->PushElem() = con;
    }
  }

  auto configFunctions = PushArenaList<ConfigFunctionDef>(temp);

#if 1
  if(parser->IfNextToken(NewTokenType_DOUBLE_HASHTAG)){
    while(!parser->Done()){
      if(IsNextTokenConfigFunctionStart(parser)){
        ConfigFunctionDef* func = ParseConfigFunction2(parser,out);

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
#endif
  
  parser->ExpectNext('}');

  ModuleDef def ={};

  def.name = name;
  def.inputs = vars;
  def.declarations = declarations;
  def.connections = PushArrayFromList(out,cons);
  def.configs = PushArrayFromList(out,configFunctions);
  
  return def;
}

TypeAndInstance ParseTypeAndInstance2(Parser* parser){
  NewToken typeName = parser->ExpectNext(NewTokenType_IDENTIFIER);

  NewToken instanceName = {};
  if(parser->IfNextToken(':')){
    instanceName = parser->ExpectNext(NewTokenType_IDENTIFIER);
  }

  TypeAndInstance res = {};
  res.typeName = C(typeName);
  res.instanceName = C(instanceName);
  
  return res;
}

HierarchicalName ParseHierarchicalName2(Parser* parser,Arena* out){
  NewToken topInstance = parser->ExpectNext(NewTokenType_IDENTIFIER);

  parser->ExpectNext('.');

  Var var = ParseVar2(parser,out);

  HierarchicalName res = {};
  res.instanceName = C(topInstance);
  res.subInstance = var;

  return res;
}

MergeDef ParseMerge2(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);
  
  parser->ExpectNext(NewTokenType_KEYWORD_MERGE);

  Array<Token> mergeModifiers = {};
  if(parser->IfNextToken('(')){
    auto tokenList = PushArenaList<Token>(temp);
    
    while(!parser->Done()){
      NewToken peek = parser->PeekToken();
      
      if(peek.type == ')'){
        break;
      }

      *tokenList->PushElem() = C(parser->ExpectNext(NewTokenType_IDENTIFIER));

      parser->IfNextToken(',');
    }

    mergeModifiers = PushArrayFromList(out,tokenList);
    
    parser->ExpectNext(')');
  }

  Token mergeName = C(parser->ExpectNext(NewTokenType_IDENTIFIER));
  
  parser->ExpectNext('=');

  ArenaList<TypeAndInstance>* declarationList = PushArenaList<TypeAndInstance>(temp);
  while(!parser->Done()){
    TypeAndInstance typeInst = ParseTypeAndInstance2(parser);

    *declarationList->PushElem() = typeInst;

    NewToken peek = parser->PeekToken();
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
  Array<TypeAndInstance> declarations = PushArrayFromList(out,declarationList);

  Array<SpecNode> specNodes = {};
  if(parser->IfNextToken('{')){
    ArenaList<SpecNode>* specList = PushArenaList<SpecNode>(temp);
    while(!parser->Done()){
      NewToken peek = parser->PeekToken();
      if(peek.type == '}'){
        break;
      }

      HierarchicalName leftSide = ParseHierarchicalName2(parser,out);
      parser->ExpectNext('-');
      HierarchicalName rightSide = ParseHierarchicalName2(parser,out);
      parser->ExpectNext(';');

      *specList->PushElem() = {leftSide,rightSide};
    }
    specNodes = PushArrayFromList(out,specList);

    parser->ExpectNext('}');
  }
  
  auto specificsArr = StartArray<SpecificMergeNode>(out);
  for(SpecNode node : specNodes){
    // TODO: We do not do this in here. Parser only parses, semantic checks are performed later.
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

Array<ConstructDef> ParseVersatSpecification2(String content,Arena* out){
  TEMP_REGION(temp,out);

  auto TokenizeFunction = [](const char* start,const char* end) -> TokenizeResult{
    TokenizeResult res = ParseWhitespace(start,end);
    res |= ParseComments(start,end);

    res |= ParseMultiSymbol(start,end,">><",NewTokenType_ROTATE_RIGHT);
    res |= ParseMultiSymbol(start,end,"><<",NewTokenType_ROTATE_LEFT);

    res |= ParseMultiSymbol(start,end,"..",NewTokenType_DOUBLE_DOT);
    res |= ParseMultiSymbol(start,end,"##",NewTokenType_DOUBLE_HASHTAG);
    res |= ParseMultiSymbol(start,end,"->",NewTokenType_ARROW);
    res |= ParseMultiSymbol(start,end,">>",NewTokenType_SHIFT_RIGHT);
    res |= ParseMultiSymbol(start,end,"<<",NewTokenType_SHIFT_LEFT);
    res |= ParseMultiSymbol(start,end,"^=",NewTokenType_XOR_EQUAL);

    res |= ParseSymbols(start,end);
    res |= ParseNumber(start,end);

    res |= ParseIdentifier(start,end);

    if(res.token.type == NewTokenType_IDENTIFIER){
      String id = res.token.identifier;
      
      NewTokenType type = NewTokenType_INVALID;

      // TODO: We really need a fast way of checking this using size + character by character branching path.
      //       However this is something that we want to push to the meta function generation. We do not want to actually write this and potentially get it wrong.
      if(id == "module")     type = NewTokenType_KEYWORD_MODULE;
      if(id == "merge")      type = NewTokenType_KEYWORD_MERGE;
      if(id == "share")      type = NewTokenType_KEYWORD_SHARE;
      if(id == "static")     type = NewTokenType_KEYWORD_STATIC;
      if(id == "debug")      type = NewTokenType_KEYWORD_DEBUG;
      if(id == "config")     type = NewTokenType_KEYWORD_CONFIG;
      if(id == "state")      type = NewTokenType_KEYWORD_STATE;
      if(id == "mem")        type = NewTokenType_KEYWORD_MEM;
      if(id == "for")        type = NewTokenType_KEYWORD_FOR;

#if 0
      if(type == NewTokenType_INVALID && Parse_IsCKeyword(id)){
        res.token.type = NewTokenType_C_KEYWORD;
      }
      if(type == NewTokenType_INVALID && Parse_IsVerilogKeyword(id)){
        res.token.type = NewTokenType_VERILOG_KEYWORD;
      }
#endif

      if(type != NewTokenType_INVALID){
        res.token.type = type;
      }
    }

    return res;
  };

  FREE_ARENA(parseArena);
  Parser* parser = StartParsing(content,TokenizeFunction,parseArena,ParsingOptions_DEFAULT);

  ArenaList<ConstructDef>* typeList = PushArenaList<ConstructDef>(temp);

  while(!parser->Done()){
    NewToken tok = parser->PeekToken();

    ConstructDef def = {};
    if(tok.type == NewTokenType_KEYWORD_MODULE){
      def.type = ConstructType_MODULE;
      def.module = ParseModuleDef2(parser,out);
    } else if(tok.type == NewTokenType_KEYWORD_MERGE){
      def.type = ConstructType_MERGE;
      def.merge = ParseMerge2(parser,out);
    } else {
      parser->ReportUnexpectedToken(tok,{NewTokenType_KEYWORD_MODULE,NewTokenType_KEYWORD_MERGE});
      parser->Synch({NewTokenType_KEYWORD_MODULE,NewTokenType_KEYWORD_MERGE});
    }

    *typeList->PushElem() = def;
  }

  if(parser->errors){
    for(String str : parser->errors){
      printf("%.*s\n",UN(str));
    }
    exit(-1);
  }

  Array<ConstructDef> defs = PushArrayFromList(out,typeList);

  return defs;
}

static ConfigIdentifier* ParseConfigIdentifier2(Parser* parser,Arena* out){
  NewToken id = parser->ExpectNext(NewTokenType_IDENTIFIER);
  
  ConfigIdentifier* base = PushStruct<ConfigIdentifier>(out);
  base->type = ConfigAccessType_BASE;
  base->name = C(id);

  ConfigIdentifier* ptr = base;

  while(!parser->Done()){
    ConfigIdentifier* parsed = nullptr;

    if(!parsed && parser->IfNextToken('.')){
      Token access = C(parser->ExpectNext(NewTokenType_IDENTIFIER));

      parsed = PushStruct<ConfigIdentifier>(out);
      parsed->type = ConfigAccessType_ACCESS;
      parsed->name = access;
    }

    if(!parsed && parser->IfNextToken('[')){
      SpecExpression* expr = ParseSpecExpression2(parser,out);

      parser->ExpectNext(']');

      parsed = PushStruct<ConfigIdentifier>(out);
      parsed->type = ConfigAccessType_ARRAY;
      parsed->trueExpr = expr;
    }

    if(parsed){
      ptr->parent = parsed;
      ptr = parsed;
    }

    break;
  }

  return base;
}

// MARK
static ConfigStatement* ParseConfigStatement2(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);
  
  ConfigStatement* stmt = PushStruct<ConfigStatement>(out);

  if(parser->IfNextToken(NewTokenType_KEYWORD_FOR)){
    NewToken loopVariable = parser->ExpectNext(NewTokenType_IDENTIFIER);
 
    SpecExpression* start = ParseLoopExpression2(parser,out);
    parser->ExpectNext(NewTokenType_DOUBLE_DOT);
    SpecExpression* end = ParseLoopExpression2(parser,out);

    parser->ExpectNext('{');

    auto list = PushArenaList<ConfigStatement*>(temp);
    while(!parser->Done()){
      ConfigStatement* child = ParseConfigStatement2(parser,out);
      *list->PushElem() = child;

      if(parser->IfPeekToken('}')){
        break;
      }
    }

    parser->ExpectNext('}');
    
    stmt->def2.loopVariable = C(loopVariable);
    stmt->def2.startSym = start;
    stmt->def2.endSym = end;
    stmt->childs = PushArrayFromList(out,list);
    stmt->type = ConfigStatementType_FOR_LOOP;
  } else if(parser->IfPeekToken(NewTokenType_IDENTIFIER)) {
    stmt->lhs = ParseConfigIdentifier2(parser,out);
    parser->ExpectNext('=');
    stmt->trueRhs = ParseLoopExpression2(parser,out);
    parser->ExpectNext(';');
    stmt->type = ConfigStatementType_STATEMENT;
  } else {
    parser->ReportUnexpectedToken(parser->NextToken(),{});
  }

  return stmt;
}

static ConfigVarDeclaration ParseConfigVarDeclaration2(Parser* parser){
  ConfigVarDeclaration res = {};

  res.name = C(parser->ExpectNext(NewTokenType_IDENTIFIER));

  if(parser->IfNextToken('[')){
    NewToken number = parser->ExpectNext(NewTokenType_NUMBER);
    int arraySize = number.number;

    parser->ExpectNext(']');

    res.arraySize = arraySize;
    res.isArray = true;
  }

  Token type = {};
  if(parser->IfNextToken(':')){
    type = C(parser->NextToken());
  }

  res.type = type;
  
  return res;
}

static Array<ConfigVarDeclaration> ParseConfigFunctionArguments2(Parser* parser,Arena* out){
  auto array = StartArray<ConfigVarDeclaration>(out);

  parser->ExpectNext('(');
  
  while(!parser->Done()){
    if(parser->IfPeekToken(')')){
      break;
    }

    ConfigVarDeclaration var = ParseConfigVarDeclaration2(parser);

    *array.PushElem() = var;
    
    if(parser->IfNextToken(',')){
      continue;
    } else {
      break;
    }
  }

  parser->ExpectNext(')');

  return EndArray(array);
}

ConfigFunctionDef* ParseConfigFunction2(Parser* parser,Arena* out){
  TEMP_REGION(temp,out);

  UserConfigType type = UserConfigType_NONE;
  if(parser->IfNextToken(NewTokenType_KEYWORD_CONFIG)){
    type = UserConfigType_CONFIG;
  } else if(parser->IfNextToken(NewTokenType_KEYWORD_MEM)){
    type = UserConfigType_MEM;
  } else if(parser->IfNextToken(NewTokenType_KEYWORD_STATE)){
    type = UserConfigType_STATE;
  }

  if(type == UserConfigType_NONE){
    return nullptr;
  }

  bool debug = parser->IfNextToken(NewTokenType_KEYWORD_DEBUG);
    
  Token configName = C(parser->ExpectNext(NewTokenType_IDENTIFIER));

  Array<ConfigVarDeclaration> functionVars = {};
  if(type == UserConfigType_MEM || type == UserConfigType_CONFIG){
    functionVars = ParseConfigFunctionArguments2(parser,out);
  }

  parser->ExpectNext('{');

  auto stmts = PushArenaList<ConfigStatement*>(temp);
  while(!parser->Done()){
    NewToken peek = parser->PeekToken();

    if(peek.type == '}'){
      break;
    }
        
    ConfigStatement* config = ParseConfigStatement2(parser,out);
    *stmts->PushElem() = config;
  }

  parser->ExpectNext('}');

  ConfigFunctionDef* res = PushStruct<ConfigFunctionDef>(out);
  res->type = type;
  res->name = configName;
  res->variables = functionVars;
  res->statements = PushArrayFromList(out,stmts);
  res->debug = debug;

  return res;
}

Array<Token> AccumTokens(SpecExpression* top,Arena* out){
  TEMP_REGION(temp,out);

  auto AccumTokens = [](auto AccumTokens,SpecExpression* top,ArenaList<Token>* list) -> void {
    switch(top->type){
    case SpecExpression::LITERAL: break;
    case SpecExpression::NAME: {
      *list->PushElem() = top->name;
    } break;
    case SpecExpression::VAR: {
      *list->PushElem() = top->var.name;
    } break;
    case SpecExpression::OPERATION: {
    } break;
    case SpecExpression::SINGLE_ACCESS: {
      *list->PushElem() = top->name;
    } break;
    case SpecExpression::ARRAY_ACCESS: {
      *list->PushElem() = top->name;
    } break;
    case SpecExpression::FUNCTION_CALL: {
      *list->PushElem() = top->name;
    } break;
    }
    for(SpecExpression* child : top->expressions){
      AccumTokens(AccumTokens,child,list);
    }
  };

  auto list = PushArenaList<Token>(temp);
  AccumTokens(AccumTokens,top,list);
  return PushArrayFromList(out,list);
}
