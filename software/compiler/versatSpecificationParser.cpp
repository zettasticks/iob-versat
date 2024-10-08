#include "versatSpecificationParser.hpp"

#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cassert>
#include <cstdint>

#include "configurations.hpp"
#include "declaration.hpp"
#include "globals.hpp"
#include "memory.hpp"
#include "type.hpp"
#include "utils.hpp"
#include "utilsCore.hpp"
#include "verilogParsing.hpp"
#include "versat.hpp"
#include "parser.hpp"
#include "debug.hpp"
#include "merge.hpp"
#include "accelerator.hpp"

// TODO: Rework expression parsing to support error reporting similar to module diff.
//       A simple form of synchronization after detecting an error would vastly improve parser 
//       Change iterative and merge to follow module def.
//       Need to detect multiple inputs to the same port and report error.
//       Transform is not correctly implemented.
//         We are thinking in ports when we should think in connection sources and sinks.
//         It's probably better to implement a specific syntax for transforms in order to prevent confusion.
//         It's probably better to swap the Iterator approach with a function that generates an array.
//           Then we can implement transformations as functions that take arrays and output arrays.
//       Error reporting is very generic. Implement more specific forms.

void ReportError(String content,Token faultyToken,const char* error){
  STACK_ARENA(temp,Kilobyte(1));

  String loc = GetRichLocationError(content,faultyToken,&temp);

  printf("\n");
  printf("%s\n",error);
  printf("%.*s\n",UNPACK_SS(loc));
  printf("\n");
}

void ReportError(Tokenizer* tok,Token faultyToken,const char* error){
  String content = tok->GetContent();
  ReportError(content,faultyToken,error);
}

bool _ExpectError(Tokenizer* tok,const char* str){
  Token got = tok->NextToken();
  String expected = STRING(str);
  if(!CompareString(got,expected)){
    STACK_ARENA(temp,Kilobyte(4));
    auto mark = StartString(&temp);
    PushString(&temp,"Parser Error.\n Expected to find:");
    PushEscapedString(&temp,expected,' ');
    PushString(&temp,"\n");
    PushString(&temp,"  Got:");
    PushEscapedString(&temp,got,' ');
    PushString(&temp,"\n");
    String text = EndString(mark);
    ReportError(tok,got,StaticFormat("%*s",UNPACK_SS(text))); \
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
      STACK_ARENA(temp,Kilobyte(1)); \
      
      ReportError(tok,number,StaticFormat("%.*s is not a valid number",UNPACK_SS(number)));
      return {};
    }
  }

  int res = ParseInt(number);
  if(negate){
    res = -res;
  }
  
  return res;
}

// TODO: Make a range with a smaller start than an end give a semantic error. Let it parse correctly and error later.
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
    tok->AdvancePeek(peek);

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
    tok->AdvancePeek(peek);

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
    tok->AdvancePeek(peek);

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
    tok->AdvancePeek(peek);

    DynamicArray<Var> arr = StartArray<Var>(out);
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
        ReportError(tok,sepOrEnd,"Unexpected token"); // TODO: Implement a Expect for multiple expected tokens.
        return {};
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
    tok->AdvancePeek(peek);
  }
  if(CompareString(peek,"-")){
    negate = true;
    negateType = "-";
    tok->AdvancePeek(peek);
  }

  SpecExpression* expr = PushStruct<SpecExpression>(out);

  peek = tok->PeekToken();
  if(peek[0] >= '0' && peek[0] <= '9'){  // TODO: Need to call ParseNumber
    tok->AdvancePeek(peek);

    int digit = ParseInt(peek);

    expr->type = SpecExpression::LITERAL;
    expr->val = MakeValue(digit);
  } else {
    expr->type = SpecExpression::VAR;
    auto mark = tok->Mark();
    Opt<Var> optVar = ParseVar(tok);
    PROPAGATE(optVar); // TODO: Error reporting for expressions.

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
    tok->AdvancePeek(peek);
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

// TODO: This is bad. Remove this
int NumberOfConnections(ConnectionExtra e){
  int connections = e.port.end - e.port.start + 1;

  return connections;
}
  
int NumberOfConnections(Array<PortExpression> arr){
  int connections = 0;
  for(PortExpression& e : arr){
    connections += NumberOfConnections(e.extra);
  }
  return connections;
}

// TODO: After cleaning up iterative and merge, remove these
void ConnectUnit(PortExpression out,PortExpression in){
  FUInstance* inst1 = out.inst;
  FUInstance* inst2 = in.inst;
  ConnectionExtra outE = out.extra;
  ConnectionExtra inE = in.extra;

  int outRange = outE.port.end - outE.port.start + 1;
  int delayRange = outE.delay.end - outE.delay.start + 1;
  int inRange = inE.port.end - inE.port.start + 1;

  bool matches = (delayRange == inRange || delayRange == 1) &&
                 (outRange == inRange   || outRange == 1);

  Assert(matches);

  int delayDelta = (delayRange == 1 ? 0 : 1);
  if(outRange == 1){
    for(int i = 0; i < inRange; i++){
      ConnectUnits(inst1,outE.port.start,inst2,inE.port.start + i,outE.delay.start + delayDelta * i);
    }
  } else {
    for(int i = 0; i < inRange; i++){
      ConnectUnits(inst1,outE.port.start + i,inst2,inE.port.start + i,outE.delay.start + delayDelta * i);
    }
  }
}

// TODO: After cleaning up iterative and merge, remove these
void ConnectUnit(Array<PortExpression> outs, PortExpression in){
  if(outs.size == 1){
    // TODO: The 1-to-1 connection implements more connection logic than this function currently implements
    //       Still not have a good ideia how to handle more complex cases, implement them as they appear.
    //       Specially how to match everything together: groups + port ranges + delays.
    ConnectUnit(outs[0],in);
    return;
  }

  ConnectionExtra inE = in.extra;

  int outRange = NumberOfConnections(outs);
  int inRange = NumberOfConnections(in.extra);

  Assert(outRange == inRange);

  FUInstance* instIn = in.inst;
  int portIndex = 0;
  int unitIndex = 0;
  for(int i = 0; i < inRange; i++){
    PortExpression& expr = outs[unitIndex];

    FUInstance* inst = expr.inst;
    int portStart = expr.extra.port.start + portIndex;
    
    ConnectUnits(inst,portStart,instIn,inE.port.start + i); // TODO: For now we are assuming no delay

    portIndex += 1;

    if(portIndex >= NumberOfConnections(expr.extra)){
      unitIndex += 1;
      portIndex = 0;
    }
  }
}

String GetUniqueName(String name,Arena* out,InstanceName* names){
  int counter = 0;
  String uniqueName = name;
  auto mark = MarkArena(out);
  while(names->Exists(uniqueName)){
    PopMark(mark);
    uniqueName = PushString(out,"%.*s_%d",UNPACK_SS(name),counter++);
  }

  names->Insert(uniqueName);

  return uniqueName;
}

String GetActualArrayName(String baseName,int index,Arena* out){
  return PushString(out,"%.*s_%d",UNPACK_SS(baseName),index);
}

// Right now, not using the full portion of PortExpression because technically we would need to instantiate multiple things. Not sure if there is a need, when a case occurs then make the change then
PortExpression InstantiateSpecExpression(SpecExpression* root,Accelerator* circuit,InstanceTable* table,InstanceName* names,Arena* temp){
  Arena* perm = globalPermanent;
  PortExpression res = {};

  switch(root->type){
    // Just to remove warnings. TODO: Change expression so that multiple locations have their own expression struct, instead of reusing the same one.
  case SpecExpression::UNDEFINED: break;
  case SpecExpression::LITERAL:{
    int number = root->val.number;

    String toSearch = PushString(temp,"N%d",number);

    FUInstance** found = table->Get(toSearch);

    if(!found){
      String permName = PushString(perm,"%.*s",UNPACK_SS(toSearch));
      String uniqueName = GetUniqueName(permName,perm,names);

      FUInstance* digitInst = (FUInstance*) CreateFUInstance(circuit,GetTypeByName(STRING("Literal")),uniqueName);
      digitInst->literal = number;
      table->Insert(permName,digitInst);
      res.inst = digitInst;
    } else {
      res.inst = *found;
    }
  }break;
  case SpecExpression::VAR:{
    STACK_ARENA(temp,Kilobyte(1));

    Var var = root->var;  
    String name = var.name;

    if(var.isArrayAccess){
      name = GetActualArrayName(var.name,var.index.bottom,&temp);
    }
    
    FUInstance* inst = table->GetOrFail(name);

    res.inst = inst;
    res.extra = var.extra;
  } break;
  case SpecExpression::OPERATION:{
    PortExpression expr0 = InstantiateSpecExpression(root->expressions[0],circuit,table,names,temp);

    // Assuming right now very simple cases, no port range and no delay range
    Assert(expr0.extra.port.start == expr0.extra.port.end);
    Assert(expr0.extra.delay.start == expr0.extra.delay.end);

    if(root->expressions.size == 1){
      Assert(root->op[0] == '~' || root->op[0] == '-');

      String typeName = {};

      switch(root->op[0]){
      case '~':{
        typeName = STRING("NOT");
      }break;
      case '-':{
        typeName = STRING("NEG");
      }break;
      }

      String permName = GetUniqueName(typeName,perm,names);
      FUInstance* inst = CreateFUInstance(circuit,GetTypeByName(typeName),permName);

      ConnectUnits(expr0.inst,expr0.extra.port.start,inst,0,expr0.extra.delay.start);

      res.inst = inst;
      res.extra.port.end  = res.extra.port.start  = 0;
      res.extra.delay.end = res.extra.delay.start = 0;

      return res;
    } else {
      Assert(root->expressions.size == 2);
    }

    PortExpression expr1 = InstantiateSpecExpression(root->expressions[1],circuit,table,names,temp);

    // Assuming right now very simple cases, no port range and no delay range
    Assert(expr1.extra.port.start == expr1.extra.port.end);
    Assert(expr1.extra.delay.start == expr1.extra.delay.end);

    String op = STRING(root->op);
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
      printf("%.*s\n",UNPACK_SS(op));
      Assert(false);
    }

    String typeStr = STRING(typeName);
    FUDeclaration* type = GetTypeByName(typeStr);
    String uniqueName = GetUniqueName(type->name,perm,names);

    FUInstance* inst = CreateFUInstance(circuit,type,uniqueName);

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
  Token token = tok->NextToken();
  res.name = token;
  
  if(!CheckValidName(res.name)){
    ReportError(tok,token,StaticFormat("'%.*s' is not a valid name",UNPACK_SS(res.name)));
    return {};
  }

  Token peek = tok->PeekToken();
  if(CompareString(peek,"[")){
    tok->AdvancePeek(peek);

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
  DynamicArray<Token> arr = StartArray<Token>(out);
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
    def.type = ConnectionDef::EQUALITY;
  } else if(CompareString(type,"^=")){
    // TODO: Only added this to make it easier to port a piece of C code.
    //       Do not know if needed or worth it to add.
  } else if(CompareString(type,"->")){
    def.transforms = CheckAndParseConnectionTransforms(tok,out);
    def.type = ConnectionDef::CONNECTION;
  } else {
    ReportError(tok,type,StaticFormat("Did not expect %.*s",UNPACK_SS(type)));
    return {};
  }
  
  if(def.type == ConnectionDef::EQUALITY){
    def.expression = ParseSpecExpression(tok,out);
  } else if(def.type == ConnectionDef::CONNECTION){
    Opt<VarGroup> optInPortion = ParseVarGroup(tok,out);
    PROPAGATE(optInPortion);

    def.input = optInPortion.value();
  }

  EXPECT(tok,";");
  
  return def;
}

static bool IsValidIdentifier(String str){
  auto CheckSingle = [](char ch){
    return ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_');
  };

  if(str.size <= 0){
    return false;
  }

  if(!CheckSingle(str.data[0])){
    return false;
  }

  for(int i = 1; i < str.size; i++){
    char ch = str.data[i];
    if(!CheckSingle(ch) && !(ch >= '0' && ch <= '9')){
      return false;
    }
  }

  return true;
}

Opt<Array<VarDeclaration>> ParseModuleInputDeclaration(Tokenizer* tok,Arena* out){
  DynamicArray<VarDeclaration> array = StartArray<VarDeclaration>(out);

  EXPECT(tok,"(");

  while(!tok->Done()){
    String peek = tok->PeekToken();

    if(CompareString(peek,")")){
      break;
    }

    Opt<VarDeclaration> var = ParseVarDeclaration(tok);
    PROPAGATE(var);

    *array.PushElem() = var.value();
    
    tok->IfNextToken(",");
  }
  
  EXPECT(tok,")");

  return EndArray(array);
}

Opt<InstanceDeclaration> ParseInstanceDeclaration(Tokenizer* tok,Arena* out,Arena* temp){
  InstanceDeclaration res = {};

  Token potentialModifier = tok->PeekToken();

  if(CompareString(potentialModifier,"static")){
    tok->AdvancePeek(potentialModifier);
    res.modifier = InstanceDeclaration::STATIC;
  } else if(CompareString(potentialModifier,"share")){
    tok->AdvancePeek(potentialModifier);
    res.modifier = InstanceDeclaration::SHARE_CONFIG;
    
    EXPECT(tok,"config"); // Only choice that is enabled, for now

    res.typeName = tok->NextToken();
    if(!IsValidIdentifier(res.typeName)){ // NOTE: Not sure if types should share same restrictions on names
      ReportError(tok,res.typeName,StaticFormat("type name '%.*s' is not a valid name",UNPACK_SS(res.typeName)));
      return {};
    }

    EXPECT(tok,"{");

    DynamicArray<VarDeclaration> array = StartArray<VarDeclaration>(out);
    
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
  }

  res.typeName = tok->NextToken();
  if(!IsValidIdentifier(res.typeName)){ // NOTE: Not sure if types should share same restrictions on names
    ReportError(tok,res.typeName,StaticFormat("'%.*s' is not a valid type name",UNPACK_SS(res.typeName)));
    return {};
  }

  Token possibleParameters = tok->PeekToken();
  auto list = PushArenaList<Pair<String,String>>(temp);
  if(CompareString(possibleParameters,"#")){
    tok->AdvancePeek(possibleParameters);
    EXPECT(tok,"(");

    while(!tok->Done()){
      EXPECT(tok,".");
      String parameterName = tok->NextToken();

      EXPECT(tok,"(");

      // TODO: Actual Parse Expression
      String content = {};

      Opt<Token> remaining = tok->NextFindUntil(")");
      PROPAGATE(remaining);
      content = remaining.value();
      
      EXPECT(tok,")");
      
      String savedParameter = PushString(out,parameterName);
      String savedString = PushString(out,content);
      *list->PushElem() = {savedParameter,savedString}; 

      if(tok->IfNextToken(",")){
        continue;
      }

      break;
    }
    EXPECT(tok,")");

    // TODO: Actual parsing of parameters
    //Token fullParameters = tok->PeekIncludingDelimiterExpression({"("},{")"},0).value();
    //tok->AdvancePeek(fullParameters);
    res.parameters = PushArrayFromList(out,list);//fullParameters;
  }

  // MARK  
  Opt<VarDeclaration> optVarDecl = ParseVarDeclaration(tok);
  PROPAGATE(optVarDecl);

  res.declarations = PushArray<VarDeclaration>(out,1);
  res.declarations[0] = optVarDecl.value();

  EXPECT(tok,";");

  return res;
}

// Any returned String points to tokenizer content.
// As long as tokenizer is valid, strings returned by this function are also valid.
Opt<ModuleDef> ParseModuleDef(Tokenizer* tok,Arena* out,Arena* temp){
  ModuleDef def = {};

  tok->AssertNextToken("module");

  def.name = tok->NextToken();
  if(!IsValidIdentifier(def.name)){
    ReportError(tok,def.name,StaticFormat("module name '%.*s' is not a valid name",UNPACK_SS(def.name)));
    return {}; // TODO: We could try to keep going and find more errors
  }
  
  Opt<Array<VarDeclaration>> optVar = ParseModuleInputDeclaration(tok,out);
  PROPAGATE(optVar);

  def.inputs = optVar.value();

  Token peek = tok->PeekToken();
  if(CompareString(peek,"->")){
    tok->AdvancePeek(peek);
    def.numberOutputs = tok->NextToken();
    // TODO : Need to check that numberOutputs is actually a number.
  }
  
  ArenaList<InstanceDeclaration>* decls = PushArenaList<InstanceDeclaration>(temp);
  EXPECT(tok,"{");
  while(!tok->Done()){
    Token peek = tok->PeekToken();

    if(CompareString(peek,";")){
      tok->AdvancePeek(peek);
      continue;
    }

    if(CompareString(peek,"#")){
      break;
    }
    
    Opt<InstanceDeclaration> optDecl = ParseInstanceDeclaration(tok,out,temp);
    PROPAGATE(optDecl); // TODO: We could try to keep going and find more errors

    *PushListElement(decls) = optDecl.value();
  }
  def.declarations = PushArrayFromList(out,decls);

  EXPECT(tok,"#");
  
  ArenaList<ConnectionDef>* cons = PushArenaList<ConnectionDef>(temp);
  while(!tok->Done()){
    Token peek = tok->PeekToken();

    if(CompareString(peek,";")){
      tok->AdvancePeek(peek);
      continue;
    }

    if(CompareString(peek,"}")){
      break;
    }

    Opt<ConnectionDef> optCon = ParseConnection(tok,out);
    PROPAGATE(optCon); // TODO: We could try to keep going and find more errors
    
    *PushListElement(cons) = optCon.value();
  }
  EXPECT(tok,"}");
  def.connections = PushArrayFromList(out,cons);
  
  return def;
}

Opt<TransformDef> ParseTransformDef(Tokenizer* tok,Arena* out,Arena* temp){
  TransformDef def = {};

  tok->AssertNextToken("transform");

  def.name = tok->NextToken();
  if(!IsValidIdentifier(def.name)){
    ReportError(tok,def.name,StaticFormat("transform name '%.*s' is not a valid name",UNPACK_SS(def.name)));
    return {}; // TODO: We could try to keep going and find more errors
  }
  
  EXPECT(tok,"{");

  ArenaList<ConnectionDef>* cons = PushArenaList<ConnectionDef>(temp);
  while(!tok->Done()){
    Token peek = tok->PeekToken();

    if(CompareString(peek,"}")){
      break;
    }

    Opt<ConnectionDef> optCon = ParseConnection(tok,out);
    PROPAGATE(optCon); // TODO: We could try to keep going and find more errors
    
    *PushListElement(cons) = optCon.value();
  }
  def.connections = PushArrayFromList(out,cons);
  EXPECT(tok,"}");
  
  return def;
}

FUDeclaration* ParseIterative(Tokenizer* tok,Arena* temp,Arena* temp2){
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
    tok->AdvancePeek(argument);

    Token peek = tok->PeekToken();
    if(CompareString(peek,",")){
      tok->AdvancePeek(peek);
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
      tok->AdvancePeek(peek);
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
        outputInstance = (FUInstance*) CreateFUInstance(iterative,BasicDeclaration::output,STRING("out"));
        table->Insert(STRING("out"),outputInstance);
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
      static String names[] = {STRING("Merge0"),
                               STRING("Merge1"),
                               STRING("Merge2"),
                               STRING("Merge3"),
                               STRING("Merge4"),
                               STRING("Merge5"),
                               STRING("Merge6"),
                               STRING("Merge7")};

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
}

Opt<TypeAndInstance> ParseTypeAndInstance(Tokenizer* tok){
  Token typeName = tok->NextToken();

  if(!CheckValidName(typeName)){
    ReportError(tok,typeName,"Not a valid type name");
    return {};
  }

  Token peek = tok->PeekToken();

  String instanceName = {};
  if(tok->IfNextToken(":")){
    instanceName = tok->NextToken();

    if(!CheckValidName(instanceName)){
      ReportError(tok,typeName,"Not a valid instance name");
      return {};
    }
  }

  TypeAndInstance res = {};
  res.typeName = typeName;
  res.instanceName = instanceName;
  
  return res;
}

Opt<HierarchicalName> ParseHierarchicalName(Tokenizer* tok){
  Token topInstance = tok->NextToken();

  if(!CheckValidName(topInstance)){
    ReportError(tok,topInstance,"Not a valid instance name");
    return {};
  }

  EXPECT(tok,".");

  Opt<Var> var = ParseVar(tok);
  PROPAGATE(var);

  HierarchicalName res = {};
  res.instanceName = topInstance;
  res.subInstance = var.value();

  return res;
}

Opt<MergeDef> ParseMerge(Tokenizer* tok,Arena* out,Arena* temp){
  Arena* perm = globalPermanent;
  BLOCK_REGION(temp);
  
  tok->AssertNextToken("merge");

  Token mergeName = tok->NextToken();
  if(!CheckValidName(mergeName)){
    ReportError(tok,mergeName,"Not a valid type name");
    return {};
  }
  
  EXPECT(tok,"=");

  ArenaList<TypeAndInstance>* declarationList = PushArenaList<TypeAndInstance>(temp);
  while(!tok->Done()){
    Opt<TypeAndInstance> optType = ParseTypeAndInstance(tok);
    PROPAGATE(optType); // TODO: Maybe Synchronize? At this point it is a bit of a problem trying to keep the parser working

    *PushListElement(declarationList) = optType.value();

    Token peek = tok->PeekToken();
    if(CompareString(peek,"|")){
      tok->AdvancePeek(peek);
      continue;
    } else if(CompareString(peek,"{")){
      break;
    } else if(CompareString(peek,";")){
      tok->AdvancePeek(peek);
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

      *PushListElement(specList) = {leftSide.value(),rightSide.value()};
    }
    specNodes = PushArrayFromList(out,specList);

    EXPECT(tok,"}");
  }
  
  DynamicArray<SpecificMergeNode> specificsArr = StartArray<SpecificMergeNode>(temp);
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
  
  return result;
}

void InstantiateMerge(MergeDef def,Arena* temp,Arena* temp2){
  DynamicArray<FUDeclaration*> declArr = StartArray<FUDeclaration*>(temp);
  for(TypeAndInstance tp : def.declarations){
    FUDeclaration* decl = GetTypeByNameOrFail(tp.typeName); // TODO: Rewrite stuff so that at this point we know that the type must exist
    *declArr.PushElem() = decl;
  }
  Array<FUDeclaration*> decl = EndArray(declArr);

  String name = PushString(globalPermanent,def.name);

  Merge(decl,name,def.specifics,temp,temp2);
}

int GetRangeCount(Range<int> range){
  Assert(range.end >= range.start);
  return (range.end - range.start + 1);
}

// Connection type and number of connections
Pair<ConnectionType,int> GetConnectionInfo(Var var){
  int indexCount = GetRangeCount(var.index);
  int portCount = GetRangeCount(var.extra.port);
  int delayCount = GetRangeCount(var.extra.delay);

  if(indexCount == 1 && portCount == 1 && delayCount == 1){
    return {ConnectionType_SINGLE,1};
  }

  // We cannot have more than one range at the same time because otherwise how do we decide how to connnect them?
  if((indexCount != 1 && portCount != 1) ||
     (indexCount != 1 && delayCount != 1) ||
     (portCount != 1 && delayCount != 1)){
    return {ConnectionType_ERROR,0};
  }
  
  if(var.isArrayAccess && indexCount != 1){
    return {ConnectionType_ARRAY_RANGE,indexCount};
  }

  if(portCount != 1){
    return {ConnectionType_PORT_RANGE,portCount};
  } else if(delayCount != 1){
    return {ConnectionType_DELAY_RANGE,delayCount};
  }

  NOT_POSSIBLE("Every condition should have been checked by now");
  return {};
}

bool IsValidGroup(VarGroup group){
  // TODO: Wether we can match the group or not.
  //       It depends on wether the ranges line up or not. 
  for(Var& var : group.vars){
    if(GetConnectionInfo(var).first == ConnectionType_ERROR){
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
  Pair<ConnectionType,int> info = GetConnectionInfo(var);
  
  ConnectionType type = info.first;
  int maxCount = info.second;

  Assert(type != ConnectionType_ERROR);

  Var res = var;

  if(type == ConnectionType_SINGLE){
    iter.groupIndex += 1;
  } else {
    switch(type){
    case ConnectionType_SINGLE: break;
    case ConnectionType_ERROR: break;
    case ConnectionType_PORT_RANGE:{
      int portBase = var.extra.port.start;
    
      res.extra.port.start = portBase + iter.varIndex; 
      res.extra.port.end = portBase + iter.varIndex; 
    } break;
    case ConnectionType_DELAY_RANGE:{
      int delayBase = var.extra.delay.start;
    
      res.extra.delay.start = delayBase + iter.varIndex; 
      res.extra.delay.end = delayBase + iter.varIndex; 
    } break;
    case ConnectionType_ARRAY_RANGE:{
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

static std::unordered_map<String,Transformation> transformations = {};

Opt<int> ApplyTransforms(String content,int outPortStart,Array<Token> transforms){
  // TODO(Performance): Trying to find transforms all the time is wasteful.
  int outPort = outPortStart;
  for(Token& transformToken : transforms){
    auto iter = transformations.find(transformToken);

    if(iter == transformations.end()){
      ReportError(content,transformToken,"Following transformation does not exist");
      return {};
    }

    Transformation transform = iter->second;

    Array<int> map = transform.map;
    if(outPort >= transform.inputs){
      const char* str = StaticFormat("Port connection (%d) outside transformation range (%d)",outPort,map.size);
      ReportError(content,transformToken,str);
      return {};
    }
          
    outPort = map[outPort];
  }

  return outPort;
}

static bool InstantiateTransform(Tokenizer* tok,TransformDef def,Arena* temp){
  Arena* perm = globalPermanent;
  BLOCK_REGION(temp);

  if(transformations.find(def.name) != transformations.end()){
    ReportError(tok,def.name,"Trasform already exists");
    return true;
  }

  Hashmap<String,int>* table = PushHashmap<String,int>(temp,1000);
  int portIndex = 0;
  bool error = false;
  for(VarDeclaration& decl : def.inputs){
    if(CompareString(decl.name,STRING("out"))){
      ReportError(tok,decl.name,"Cannot use special out unit as module input");
      error = true;
    }

    if(decl.isArray){
      for(int i = 0; i < decl.arraySize; i++){
        String actualName = GetActualArrayName(decl.name,i,temp);
        table->Insert(actualName,portIndex++);
      }
    } else {
      table->Insert(decl.name,portIndex++);
    }
  }

  Set<int>* inputsSeen = PushSet<int>(temp,1000);
  Hashmap<int,int>* transformation = PushHashmap<int,int>(temp,1000);
  for(ConnectionDef& decl : def.connections){
    int nOutConnections = NumberOfConnections(decl.output);
    int nInConnections = NumberOfConnections(decl.input);

    // TODO: Proper error report by making VarGroup a proper struct that stores a token for the entire parsed text.
    
    if(nOutConnections != nInConnections){
      printf("Number of connections missmatch %d to %d\n",nOutConnections,nInConnections);
      Assert(false);
    }

    GroupIterator out = IterateGroup(decl.output);
    GroupIterator in  = IterateGroup(decl.input);
    while(HasNext(out) && HasNext(in)){
      BLOCK_REGION(temp);

      Var outVar = Next(out);
      Var inVar = Next(in);
        
      Assert(inVar.extra.delay.high == 0); // For now, inputs cannot have delay.

      String outName = outVar.name;
      if(outVar.isArrayAccess){
        outName = GetActualArrayName(outName,outVar.index.low,temp);
      }
      String inName = inVar.name;
      if(inVar.isArrayAccess){
        inName = GetActualArrayName(inName,inVar.index.low,temp);
      }

      if(CompareString(outName,STRING("out"))){
        ReportError(tok,outVar.name,"Cannot use special out unit as an input");
        error = true;
        break;
      }

      if(!CompareString(inName,STRING("out"))){
        ReportError(tok,inVar.name,"Only out is allowed as input of connection");
        error = true;
        break;
      }

      if(!table->Exists(outName)){
        ReportError(tok,outVar.name,"Did not find the following input");
        error = true;
        break;
      }
        
      int outPort = table->GetOrFail(outName);

      Opt<int> optOutPort = ApplyTransforms(tok->GetContent(),outPort,decl.transforms);
      if(!optOutPort.has_value()){
        error = true;
        break;
      }
        
      outPort = optOutPort.value();
      int inPort  = inVar.extra.port.low;

      if(inputsSeen->ExistsOrInsert(inPort)){
        ReportError(tok,inVar.name,"Input already was set once before. Cannot connect two ports to one input");
        error = true;
        break;
      }

      transformation->Insert(outPort,inPort);
    }
      
    Assert(HasNext(out) == HasNext(in));
  }

  Array<int> transform = PushArray<int>(perm,transformation->nodesUsed);
  int inputs = 0;
  int outputs = 0;
  for(Pair<int,int*> p : transformation){
    transform[p.first] = *p.second;
    inputs = std::max(inputs,p.first);
    outputs = std::max(outputs,*p.second);
  }
  
  String storedString = PushString(perm,def.name);

  Transformation trans = {};
  trans.map = transform;
  trans.inputs = inputs + 1; // Plus one because inputs is an index
  trans.outputs = outputs + 1;

  transformations.insert({storedString,trans});
  return error;
}

FUInstance* CreateFUInstanceWithParameters(Accelerator* accel,FUDeclaration* type,String name,InstanceDeclaration decl){
  FUInstance* inst = CreateFUInstance(accel,type,name);

  for(auto pair : decl.parameters){
    bool result = SetParameter(inst,pair.first,pair.second);

    if(!result){
      printf("Warning: Parameter %.*s for instance %.*s in module %.*s does not exist\n",UNPACK_SS(pair.first),UNPACK_SS(inst->name),UNPACK_SS(accel->name));
    }
  }

  return inst;
}

FUDeclaration* InstantiateModule(String content,ModuleDef def,Arena* temp,Arena* temp2){
  Arena* perm = globalPermanent;
  Accelerator* circuit = CreateAccelerator(def.name,AcceleratorPurpose_MODULE);

  InstanceTable* table = PushHashmap<String,FUInstance*>(temp,1000);
  InstanceName* names = PushSet<String>(temp,1000);
  bool error = false;

  // TODO: Need to detect when multiple instances with same name
  int insertedInputs = 0;
  for(VarDeclaration& decl : def.inputs){
    if(CompareString(decl.name,STRING("out"))){
      ReportError(content,decl.name,"Cannot use special out unit as module input");
      error = true;
    }

    if(decl.isArray){
      for(int i = 0; i < decl.arraySize; i++){
        String actualName = GetActualArrayName(decl.name,i,temp);
        FUInstance* input = CreateOrGetInput(circuit,actualName,insertedInputs++);
        names->Insert(actualName);
        table->Insert(actualName,input);
      }
    } else {
      FUInstance* input = CreateOrGetInput(circuit,decl.name,insertedInputs++);
      names->Insert(decl.name);
      table->Insert(decl.name,input);
    }
  }

  int shareIndex = 0;
  for(InstanceDeclaration& decl : def.declarations){
    for(VarDeclaration& var : decl.declarations){
      if(CompareString(var.name,STRING("out"))){
        ReportError(content,var.name,"Cannot use special out unit as module input");
        error = true;
        break;
      }
    }
    
    FUDeclaration* type = GetTypeByName(decl.typeName);
    if(type == nullptr){
      ReportError(content,decl.typeName,"Given type was not found");
      error = true;
      // TODO: Keep running using a type that contains infinity inputs and outputs.
      break; // For now skip over.
    }
    
    switch(decl.modifier){
    case InstanceDeclaration::SHARE_CONFIG:{
      for(VarDeclaration& varDecl : decl.declarations){
        if(varDecl.isArray){
          for(int i = 0; i < varDecl.arraySize; i++){
            String actualName = GetActualArrayName(varDecl.name,i,temp);
            FUInstance* inst = CreateFUInstanceWithParameters(circuit,type,actualName,decl);

            table->Insert(actualName,inst);
            ShareInstanceConfig(inst,shareIndex);
          }
        } else {
          FUInstance* input = CreateOrGetInput(circuit,varDecl.name,insertedInputs++);
          names->Insert(varDecl.name);
          table->Insert(varDecl.name,input);
        }
      }
      shareIndex += 1;
    } break;

    case InstanceDeclaration::NONE:
    case InstanceDeclaration::STATIC:
    {
      Assert(decl.declarations.size == 1);
      
      VarDeclaration varDecl = decl.declarations[0];
      
      if(varDecl.isArray){
        for(int i = 0; i < varDecl.arraySize; i++){
          String actualName = GetActualArrayName(varDecl.name,i,temp);

          FUInstance* inst = CreateFUInstanceWithParameters(circuit,type,actualName,decl);
          table->Insert(actualName,inst);

          if(decl.modifier == InstanceDeclaration::STATIC){
            SetStatic(circuit,inst);
          }
        }
      } else {
        FUInstance* inst = CreateFUInstanceWithParameters(circuit,type,varDecl.name,decl);

        table->Insert(varDecl.name,inst);

        if(decl.modifier == InstanceDeclaration::STATIC){
          SetStatic(circuit,inst);
        }
      }
    } break;
    }
  }

  FUInstance* outputInstance = nullptr;
  for(ConnectionDef& decl : def.connections){
    if(decl.type == ConnectionDef::EQUALITY){
      // Make sure that not a single equality value is named out.

      Assert(decl.output.vars.size == 1); // Only allow one for equality, for now
      Var outVar = decl.output.vars[0];

      if(CompareString(outVar.name,STRING("out"))){
        ReportError(content,outVar.name,"Cannot use special out unit as input in an equality");
        error = true;
        break;
      }
      
      String name = outVar.name;
      if(outVar.isArrayAccess){
        name = GetActualArrayName(name,outVar.index.low,temp);
      }
      
      PortExpression portSpecExpression = InstantiateSpecExpression(decl.expression,circuit,table,names,temp);
      FUInstance* inst = portSpecExpression.inst;
      String uniqueName = GetUniqueName(name,perm,names);
      inst->name = PushString(perm,uniqueName);

      names->Insert(name);
      table->Insert(name,inst);
    } else if(decl.type == ConnectionDef::CONNECTION){
      // For now only allow one var on the input side

      int nOutConnections = NumberOfConnections(decl.output);
      int nInConnections = NumberOfConnections(decl.input);

      // TODO: Proper error report by making VarGroup a proper struct that stores a token for the entire parsed text.
      if(nOutConnections != nInConnections){
        const char* text = StaticFormat("Number of connections missmatch %d to %d\n",nOutConnections,nInConnections);
        ReportError(content,decl.output.fullText,text);
        Assert(false);
      }

      GroupIterator out = IterateGroup(decl.output);
      GroupIterator in  = IterateGroup(decl.input);

      while(HasNext(out) && HasNext(in)){
        BLOCK_REGION(temp);

        Var outVar = Next(out);
        Var inVar = Next(in);
        
        Assert(inVar.extra.delay.high == 0); // For now, inputs cannot have delay.

        String outName = outVar.name;
        if(outVar.isArrayAccess){
          outName = GetActualArrayName(outName,outVar.index.low,temp);
        }
        String inName = inVar.name;
        if(inVar.isArrayAccess){
          inName = GetActualArrayName(inName,inVar.index.low,temp);
        }

        if(CompareString(outName,STRING("out"))){
          ReportError(content,outVar.name,"Cannot use special out unit as an input");
          error = true;
          break;
        }
        
        FUInstance** optOutInstance = table->Get(outName);
        if(optOutInstance == nullptr){
          ReportError(content,outVar.name,"Did not find the following instance");
          error = true;
          break;
        }
        
        FUInstance** optInInstance = nullptr;

        if(CompareString(inName,"out")){
          if(!outputInstance){
            outputInstance = (FUInstance*) CreateFUInstance(circuit,BasicDeclaration::output,STRING("out"));
          }

          optInInstance = &outputInstance;
        } else {
          optInInstance = table->Get(inName);
        }

        if(optInInstance == nullptr){
          ReportError(content,outVar.name,"Did not find the following instance");
          error = true;
          break;
        }
        
        FUInstance* outInstance = *optOutInstance;
        FUInstance* inInstance = *optInInstance;

        int outPort = outVar.extra.port.low;

        Opt<int> optOutPort = ApplyTransforms(content,outPort,decl.transforms);
        if(!optOutPort.has_value()){
          error = true;
          break;
        }
        
        outPort = optOutPort.value();
        int inPort  = inVar.extra.port.low;
        ConnectUnits(outInstance,outPort,inInstance,inPort,outVar.extra.delay.low);
      }
      
      Assert(HasNext(out) == HasNext(in));
    }
  }
  
  // Care to never put 'out' inside the table
  FUInstance** outInTable = table->Get(STRING("out"));
  Assert(!outInTable);

  return RegisterSubUnit(circuit,temp,temp2);
}

void Synchronize(Tokenizer* tok,BracketList<const char*> syncPoints){
  while(!tok->Done()){
    Token peek = tok->PeekToken();

    for(const char* point : syncPoints){
      if(CompareString(peek,point)){
        return;
      }
    }

    tok->AdvancePeek(peek);
  }
}

Array<TypeDefinition> ParseVersatSpecification(String content,Arena* out,Arena* temp){
  Tokenizer tokenizer = Tokenizer(content,".%=#[](){}+:;,*~-",{"->=","->",">><","><<",">>","<<","..","^="});
  Tokenizer* tok = &tokenizer;
  
  BLOCK_REGION(temp);

  ArenaList<TypeDefinition>* typeList = PushArenaList<TypeDefinition>(temp);
  
  bool anyError = false;
  while(!tok->Done()){
    Token peek = tok->PeekToken();

    if(CompareString(peek,"module")){
      Opt<ModuleDef> moduleDef = ParseModuleDef(tok,out,temp);

      if(moduleDef.has_value()){
        TypeDefinition def = {};
        def.type = DefinitionType_MODULE;
        def.module = moduleDef.value();

        *PushListElement(typeList) = def;
      } else {
        anyError = true;
      }
    } else if(CompareString(peek,"merge")){
      Opt<MergeDef> mergeDef = ParseMerge(tok,out,temp);
      
      if(mergeDef.has_value()){
        TypeDefinition def = {};
        def.type = DefinitionType_MERGE;
        def.merge = mergeDef.value();

        *PushListElement(typeList) = def;
      } else {
        anyError = true;
      }
    } else {
      // TODO: Report error, 
      ReportError(tok,peek,"Unexpected token in global scope");
      tok->AdvancePeek(peek);
      Synchronize(tok,{"debug","module","iterative","merge","transform"});
    }
  }

  if(anyError){
    exit(-1);
  }

  return PushArrayFromList(out,typeList);
}

Array<Token> TypesUsed(TypeDefinition def,Arena* out,Arena* temp){
  BLOCK_REGION(temp);

  switch(def.type){
  case DefinitionType_MERGE: {
    // TODO: How do we deal with same types being used?
    //       Do we just ignore it?
    Array<Token> result = Map(def.merge.declarations,out,[](TypeAndInstance def){
      return def.typeName;
    });
    
    return result;
  } break;
  case DefinitionType_MODULE: {
    Array<Token> result = Map(def.module.declarations,temp,[](InstanceDeclaration decl){
      return decl.typeName;
    });

    return Unique(result,out,temp);
  } break;
  default: Assert(false);
  }

  return {};
}

void InstantiateSpecifications(String content,TypeDefinition def,Arena* temp,Arena* temp2){
  BLOCK_REGION(temp);

  switch(def.type){
  case DefinitionType_MERGE: {
    InstantiateMerge(def.merge,temp,temp2);
  } break;
  case DefinitionType_MODULE: {
    InstantiateModule(content,def.module,temp,temp2);
  } break;
  default: Assert(false);
  }
}

