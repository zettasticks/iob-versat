#include "verilogParsing.hpp"

#include "parser.hpp"

#include "filesystem.hpp"
#include "globals.hpp"
#include "memory.hpp"
#include "templateEngine.hpp"
#include "utilsCore.hpp"

#include "symbolic.hpp"

#include <string>

typedef Value (*MathFunction)(Value f,Value g);

#define VERSAT_LATENCY "versat_latency"
#define VERSAT_STATIC "versat_static"
#define VERSAT_STAGE "versat_stage"
static String possibleAttributes_Raw[] = {VERSAT_LATENCY,VERSAT_STATIC,VERSAT_STAGE};
static Array<String> possibleAttributes = {possibleAttributes_Raw,ARRAY_SIZE(possibleAttributes_Raw)};

struct MathFunctionDescription{
  String name;
  int amountOfParameters;
  MathFunction func;
};
#define MATH_FUNC(...) [](Value f,Value g) -> Value{__VA_ARGS__}

static MathFunctionDescription verilogMathFunctions[] = {
  {"clog2",1,MATH_FUNC(return MakeValue(log2i(f.number));)},
  {"ln",1},
  {"log",1},
  {"exp",1},
  {"sqrt",1},
  {"pow",2},
  {"floor",1},
  {"ceil",1},
  {"sin",1},
  {"cos",1},
  {"tan",1},
  {"asin",1},
  {"acos",1},
  {"atan",1},
  {"atan2",2},
  {"hypot",2},
  {"sinh",1},
  {"cosh",1},
  {"tanh",1},
  {"asinh",1},
  {"acosh",1},
  {"atanh",1}
};

Opt<MathFunctionDescription> GetMathFunction(String name){
  for(int i = 0; i < ARRAY_SIZE(verilogMathFunctions); i++){
    if(CompareString(verilogMathFunctions[i].name,name)){
      return verilogMathFunctions[i];
    }
  }
  return {};
}

static void PrintExpression(StringBuilder* b,VExpr* exp,int level){
  b->PushSpaces(level);
  switch(exp->type){
  case VExpr::UNDEFINED:{
    b->PushString("UNDEFINED\n");
  }break;
  case VExpr::OPERATION:{
    b->PushString("OPERATION\n");
  }break;
  case VExpr::IDENTIFIER:{
    b->PushString("IDENTIFIER\n");
  }break;
  case VExpr::FUNCTION:{
    b->PushString("FUNCTION\n");
  }break;
  case VExpr::LITERAL:{
    Value val = exp->val;
    b->PushString("LITERAL: %ld\n",val.number);
  }break;
  }

  if(exp->op){
    b->PushSpaces(level);
    b->PushString("op: %s\n",exp->op);
  }

  if(exp->id.size){
    b->PushSpaces(level);
    b->PushString("id: %.*s\n",UN(exp->id));
  }

  for(VExpr* subExpressions : exp->expressions){
    PrintExpression(b,subExpressions,level + 2);
    b->PushString("\n");
  }
}

void PrintExpression(VExpr* exp){
  TEMP_REGION(temp,nullptr);

  auto b = StartString(temp);
  PrintExpression(b,exp,0);

  String res = EndString(temp,b);
  printf("%.*s\n",UN(res));
}

SYM_Expr SymbolicExpressionFromVerilog(VExpr* topExpr){
  SYM_Expr res = SYM_Nil;

  FULL_SWITCH(topExpr->type){
  case VExpr::UNDEFINED: {
    Assert(false);
  } break;
  case VExpr::OPERATION: {
    SYM_Expr left = SymbolicExpressionFromVerilog(topExpr->expressions[0]);
    SYM_Expr right = SymbolicExpressionFromVerilog(topExpr->expressions[1]);
    
    switch(topExpr->op[0]){
    case '+':{
      res = left + right;
    } break;
    case '-':{
      res = left - right;
    } break;
    case '*':{
      res = left * right;
    } break;
    case '/':{
      res = left / right;
    } break;
    default:{
      // TODO: Better error message
      NOT_IMPLEMENTED("");
    } break;
    } 
  } break;
  case VExpr::IDENTIFIER: {
    res = SYM_Var(topExpr->id);
  } break;
  case VExpr::FUNCTION: {
    // TODO: Better error message and we probably can do more stuff here
    NOT_IMPLEMENTED("");
  } break;
  case VExpr::LITERAL: {
    res = SYM_Lit(topExpr->val.number);
  } break;
}
  
  return res;
}

SYM_Expr SymbolicExpressionFromVerilog(ExpressionRange range){
  SYM_Expr top = SymbolicExpressionFromVerilog(range.top);
  SYM_Expr bottom = SymbolicExpressionFromVerilog(range.bottom);

  SYM_Expr res = top - bottom + SYM_1;

  return res;
}

ExternalMemorySymbolic Replace(ExternalMemorySymbolic in,TrieMap<String,SYM_Expr>* replacements){
  ExternalMemorySymbolic res = in;
  
  FULL_SWITCH(res.type){
  case ExternalMemoryType_2P:{
    res.tp.bitSizeIn = SYM_Replace(res.tp.bitSizeIn,replacements);
    res.tp.bitSizeOut = SYM_Replace(res.tp.bitSizeOut,replacements);
    res.tp.dataSizeIn = SYM_Replace(res.tp.dataSizeIn,replacements);
    res.tp.dataSizeOut = SYM_Replace(res.tp.dataSizeOut,replacements);
  } break;
  case ExternalMemoryType_DP:{
    res.dp[0].bitSize = SYM_Replace(res.dp[0].bitSize,replacements);
    res.dp[0].dataSizeIn = SYM_Replace(res.dp[0].dataSizeIn,replacements);
    res.dp[0].dataSizeOut = SYM_Replace(res.dp[0].dataSizeOut,replacements);

    res.dp[1].bitSize = SYM_Replace(res.dp[1].bitSize,replacements);
    res.dp[1].dataSizeIn = SYM_Replace(res.dp[1].dataSizeIn,replacements);
    res.dp[1].dataSizeOut = SYM_Replace(res.dp[1].dataSizeOut,replacements);
  } break;
}

  return res;
}

struct ContentState{
  FileContent content;
  
  const char* start;
  const char* ptr;
  const char* end;
};

enum DefineType{
  DefineType_FUNCTION,
  DefineType_SIMPLE_REPLACE
};

struct DefineInfo{
  DefineType type;

  Array<Token> tokens;
  Array<String> args;
  bool disabled;
};

enum VerilogTokenizerStageType{
  VerilogTokenizerStageType_FILE,
  VerilogTokenizerStageType_DEFINE,
  VerilogTokenizerStageType_COND
};

struct VerilogTokenizerStage{
  VerilogTokenizerStageType type; 
 
  // TODO: Union
  DefineInfo* define;
  int currentDefineToken;

  bool condActive;
  bool condDone;

  ContentState state;
};

static Value Eval(VExpr* expr,TrieMap<String,Value>* map){
  switch(expr->type){
  case VExpr::OPERATION:{
    Value val1 = Eval(expr->expressions[0],map);
    Value val2 = Eval(expr->expressions[1],map);

    switch(expr->op[0]){
    case '+':{
      Value res = MakeValue(val1.number + val2.number);
      return res;
    }break;
    case '-':{
      Value res = MakeValue(val1.number - val2.number);
      return res;
    }break;
    case '*':{
      Value res = MakeValue(val1.number * val2.number);
      return res;
    }break;
    case '/':{
      Value res = MakeValue(val1.number / val2.number);
      return res;
    }break;
    default:{
      NOT_IMPLEMENTED("Implemented as needed");
    } break;
    }
  }break;
  case VExpr::IDENTIFIER:{
    Value* val = map->Get(expr->id);
    if(!map){
      printf("Error, did not find parameter %.*s\n",UN(expr->id));
      NOT_IMPLEMENTED("Need to also report file position and stuff like that, similar to parser tokenizer error");
    }
    
    return *val;
  }break;
  case VExpr::LITERAL:{
    return expr->val;
  }break;
  case VExpr::FUNCTION:{ // Called Command but for verilog it is actually a math function call
    String functionName = expr->id;

    VExpr* argument = expr->expressions[0];
    MathFunctionDescription optDescription = GetMathFunction(functionName).value();

    if(!optDescription.func){
      printf("Verilog Math function not currently implemented: %.*s",UN(functionName));
      exit(0);
    }

    // NOTE: For now, hardcoded to 1 argument functions
    Value argumentValue = Eval(argument,map);
    Value result = optDescription.func(argumentValue,MakeValue());
    
    return result;
  } break;
  case VExpr::UNDEFINED:
    NOT_POSSIBLE("None of these should appear in parameters");
  }

  NOT_POSSIBLE("Implemented as needed");
  return MakeValue();
}

static ExpressionRange ParseOptionalRange(Parser* tok,Arena* out);
VExpr* VerilogParseExpression(Parser* parser,Arena* out,int bindingPower = 99);

static Array<ParameterExpression> ParseParameters(Parser* tok,TrieMap<String,Value>* map,Arena* out){
  //TODO: Add type and range to parsing
  /*
	Range currentRange;
	ParameterType type;
   */
  TEMP_REGION(temp,out);

  auto params = PushList<ParameterExpression>(temp);

  // TODO: Not used but must parse it anyway.
  ExpressionRange range = {};
  while(1){
    Token peek = tok->PeekToken();

    if(peek.type == TokenType_VERILOG_KEYWORD_PARAMETER){
      tok->Advance(peek);

      Token possibleComment = tok->PeekToken(0,ParsingOptions_ALLOW_COMMENTS);

      // We allow comments before or after parameter keyword
      ParamFlags flags = {};
      if(PARSE_IsComment(possibleComment.type)){
        tok->Advance(possibleComment);

        auto TokenizeFunction = [](void* tokenizerState,const char* start,const char* end) -> Token{
          DefaultTokenizerState* state = (DefaultTokenizerState*) tokenizerState;

          Token res = {};
          res |= ParseWhitespace(start,end);
          res |= ParseSymbols(start,end);
          res |= ParseNumber(start,end);
          res |= ParseIdentifier(start,end);

          // NOTE: Something very bad must happen to the point where the file is 1 byte after the end.
          //       We expect it to only reach file->end, not file->end + 1

          return res;
        };

        String content = PARSE_GetCommentContent(possibleComment);

        FREE_ARENA(commentParsing);
        Parser* p = StartParsing(TokenizeFunction,content,commentParsing);

        Token t = p->NextToken();
        if(t.type == TokenType_IDENTIFIER && t.val == "versat"){
          p->ExpectNext(':');
          
          while(!p->Done()){
            p->IfNextToken(',');
          
            Token paramFlag = p->ExpectNext(TokenType_IDENTIFIER);
            ParamFlags flagOpt = ParamFlags_FromName(paramFlag.val);

            if(flagOpt){
              // Better support for flag concatenation when using enums.
              flags = (ParamFlags) ((u32) flags | (u32) flagOpt);
            } else {
              // TODO: Better error reporting
              printf("%.*s is not a valid param flag\n",UN(paramFlag.val));
              ENTER_DEBUG();
            }
          }
        }
        
        for(String err : p->errors){
          tok->ReportError(err);
        }
      }

      tok->IfNextToken(TokenType_VERILOG_KEYWORD_SIGNED);

      range = ParseOptionalRange(tok,out);

      Token paramName = tok->NextToken();

      tok->ExpectNext('=');

      VExpr* expr = VerilogParseExpression(tok,out);
      Value val = Eval(expr,map);

      map->Insert(paramName.val,val);

      ParameterExpression* p = params->PushElem();
      p->name = paramName.val;
      p->expr = expr;
      p->flags = flags;
    } else if(peek.type == ')'){
      break;
    } else if(peek.type == ';'){ // To parse inside module parameters, technically wrong but harmless
      tok->NextToken();
      break;
    } else if(peek.type == ','){
      tok->NextToken();
      continue;
    }
  }

  return PushArray(out,params);
}

VExpr* VerilogParseExpression(Parser* parser,Arena* out,int bindingPower){
  VExpr* topUnary = nullptr;
  VExpr* innerMostUnary = nullptr;

  VExpr* res = nullptr;
  
  // Parse unary
  while(!parser->Done()){
    VExpr* parsed = nullptr;
    if(!parsed &&  parser->IfNextToken('-')){
      parsed = PushStruct<VExpr>(out);
      parsed->op = "-";
    }

    if(parsed){
      parsed->type = VExpr::OPERATION;
    }

    if(parsed && !topUnary){
      topUnary = parsed;
      innerMostUnary = parsed;
      continue;
    }

    if(parsed){
      innerMostUnary->expressions = PushArray<VExpr*>(out,1);
      innerMostUnary->expressions[0] = parsed;
      innerMostUnary = parsed;
      continue;
    }

    break;
  }

  // Parse atom
  Token peek = parser->PeekToken();
  if(peek.type == '('){
    parser->ExpectNext('(');

    res = VerilogParseExpression(parser,out);

    parser->ExpectNext(')');
  } else if(peek.type == TokenType_NUMBER){
    Token number = parser->ExpectNext(TokenType_NUMBER);
    res = PushStruct<VExpr>(out);

    res->type = VExpr::LITERAL;

    const char* start = number.val.data;
    const char* end = start + number.val.size;

    TEMP_REGION(temp,out);
    V_ParsedNumber num = V_ParseNumber(start,end,temp);

    if(num.anyError){
      // Report Error
      Assert(false);
    }

    i64 val = num.decimalNumber;
    if(num.type == V_NumberType_BINARY){
      val = 0;

      int size = num.asBinary.size;
      for(int i = 0; i < size; i++){
        char ch = num.asBinary[size - i - 1];

        if(ch == '1'){
          val |= (1 << i);
        } else if(ch == '0'){
          // Nothing
        } else {
          // Report error, cannot handle x or z since we only care about port sizes.
          Assert(false);
        }
      }
    } else if(num.type != V_NumberType_DECIMAL){
      Assert(false);
    }

    res->val = MakeValue(val);
  } else if(peek.type == TokenType_IDENTIFIER){
    Token id = parser->ExpectNext(TokenType_IDENTIFIER);
   
    res = PushStruct<VExpr>(out);
    res->type = VExpr::IDENTIFIER;
    res->id = id.val;
  } else if (peek.type == TokenType_C_STRING) {
    Token token = parser->ExpectNext(TokenType_C_STRING);
    
    String string = PARSE_GetStringContent(token);

    res = PushStruct<VExpr>(out);
    res->val = MakeValue(string);
    res->type = VExpr::LITERAL;
  } else if(peek.type == '$'){
    VExpr* expr = PushStruct<VExpr>(out);
    *expr = {};

    parser->NextToken();

    expr->id = parser->ExpectNext(TokenType_IDENTIFIER).val;

    Opt<MathFunctionDescription> optDescription = GetMathFunction(expr->id);
    Assert(optDescription.has_value());
    MathFunctionDescription description = optDescription.value();

    expr->type = VExpr::FUNCTION;
    expr->expressions = PushArray<VExpr*>(out,description.amountOfParameters);

    parser->ExpectNext('(');
    expr->expressions[0] = VerilogParseExpression(parser,out);
    if(description.amountOfParameters == 2){
      parser->ExpectNext(',');
      expr->expressions[1] = VerilogParseExpression(parser,out);
    }
    
    parser->ExpectNext(')');
    
  } else {
    // TODO: Better error reporting
    parser->ReportUnexpectedToken(peek,{});
  }

  if(topUnary){
    innerMostUnary->expressions = PushArray<VExpr*>(out,1);
    innerMostUnary->expressions[0] = res;

    res = topUnary;
  }

  struct OpInfo{
    TokenType type;
    int bindingPower;
    const char* op;
  };

  // TODO: This should be outside the function itself.
  TEMP_REGION(temp,out);
  auto infos = PushArray<OpInfo>(temp,7);

  // TODO: We are missing a couple of operations and need to double check 
  // TODO: Need to double check binding power
  infos[0] = {TOK_TYPE('&'),0,"&"};
  infos[1] = {TOK_TYPE('|'),0,"|"};
  infos[2] = {TOK_TYPE('^'),0,"^"};

  infos[3]  = {TOK_TYPE('*'),1,"*"};
  infos[4] = {TOK_TYPE('/'),1,"/"};

  infos[5] = {TOK_TYPE('+'),2,"+"};
  infos[6] = {TOK_TYPE('-'),2,"-"};
  
  // Parse binary ops.
  while(!parser->Done()){
    Token peek = parser->PeekToken();

    bool continueOuter = false;
    for(OpInfo info : infos){
      if(peek.type == info.type){
        if(info.bindingPower < bindingPower){
          parser->NextToken();

          VExpr* right = VerilogParseExpression(parser,out,info.bindingPower);
      
          VExpr* op = PushStruct<VExpr>(out);

          op->op = info.op;
          op->type = VExpr::OPERATION;
          op->expressions = PushArray<VExpr*>(out,2);
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

  // Parse ternary ops.
  if(parser->IfNextToken('?')){
    VExpr* first = VerilogParseExpression(parser,out);

    parser->ExpectNext(':');

    VExpr* second = VerilogParseExpression(parser,out);
    
    VExpr* ternary = PushStruct<VExpr>(out);

    ternary->op = "?";
    ternary->type = VExpr::OPERATION;
    ternary->expressions = PushArray<VExpr*>(out,3);
    ternary->expressions[0] = res;
    ternary->expressions[1] = first;
    ternary->expressions[2] = second;

    res = ternary;
  }

  return res;
}

static ExpressionRange ParseOptionalRange(Parser* tok,Arena* out){
  static VExpr zeroExpression = {};
  zeroExpression.type = VExpr::LITERAL;
  zeroExpression.val = MakeValue(0);

  Token peek = tok->PeekToken();

  if(peek.type != '['){ // No range is equal to range [0:0]. Do not known if it's worth/need to  differentiate
    ExpressionRange range = {};
    
    range.top = &zeroExpression;
    range.bottom = &zeroExpression;
    
    return range;
  }

  tok->ExpectNext('[');

  ExpressionRange res = {};
  res.top = VerilogParseExpression(tok,out);

  tok->ExpectNext(':');

  res.bottom = VerilogParseExpression(tok,out);
  tok->ExpectNext(']');

  return res;
}

static Module ParseModule(Parser* tok,Arena* out){
  TEMP_REGION(temp,out);

  Module module = {};

  TrieMap<String,Value>* values = PushTrieMap<String,Value>(temp);

  tok->ExpectNext(TokenType_VERILOG_KEYWORD_MODULE);

  module.name = tok->ExpectNext(TokenType_IDENTIFIER).val;

  //NewToken peek = C(tok->PeekToken());
  if(tok->IfNextToken('#')){
    tok->ExpectNext('(');
    module.parameters = ParseParameters(tok,values,out);
    tok->ExpectNext(')');
  }

  tok->ExpectNext('(');
  if(!tok->IfPeekToken(')')){

    ArenaList<PortDeclaration>* portList = PushList<PortDeclaration>(temp);
    // Parse ports
    while(!tok->Done()){
      Token peek = tok->PeekToken();

      PortDeclaration port;
      ArenaList<Pair<String,Value>>* attributeList = PushList<Pair<String,Value>>(temp);
    
      if(peek.type == TokenType_VERILOG_ATTRIBUTE_START){
        tok->NextToken();
        while(!tok->Done()){
          Token attributeName = tok->ExpectNext(TokenType_IDENTIFIER);

#if 1
          if(attributeName.type == TokenType_IDENTIFIER){
            if(!Contains(possibleAttributes,attributeName.val)){
              printf("ERROR: Do not know attribute named: %.*s\n",UN(attributeName.val));
              exit(-1);
            }
          }
#endif

          Token peek = tok->PeekToken();
          if(peek.type == '='){
            tok->Advance(peek);

            VExpr* expr = VerilogParseExpression(tok,out);
            Value value = Eval(expr,values);

            *attributeList->PushElem() = {attributeName.val,value};

            peek = tok->PeekToken();
          } else {
            *attributeList->PushElem() = {attributeName.val,MakeValue()};
          }

          if(peek.type == ','){
            tok->NextToken();
            continue;
          }
          if(peek.type == TokenType_VERILOG_ATTRIBUTE_END){
            tok->NextToken();
            break;
          }
        }
      }
      port.attributes = PushHashmapFromList(out,attributeList);

      Token portType = tok->NextToken();
      if(portType.type == TokenType_VERILOG_KEYWORD_INPUT){
        port.type = WireDir_INPUT;
      } else if(portType.type == TokenType_VERILOG_KEYWORD_OUTPUT){
        port.type = WireDir_OUTPUT;
      } else if(portType.type == TokenType_VERILOG_KEYWORD_INOUT){
        port.type = WireDir_INOUT;
      } else {
        UNHANDLED_ERROR("TODO: Should be a handled error");
      }

      // TODO: Add a new function to parser to "ignore" the following list of tokens (loop every time until it doesn't find one from the list), and replace this function here with reg and all the different types it can be
      while(1){
        Token peek = tok->PeekToken();
        if(peek.type == TokenType_VERILOG_KEYWORD_REG){
          tok->NextToken();
          continue;
        }
        if(peek.type == TokenType_VERILOG_KEYWORD_SIGNED){
          tok->NextToken();
          continue;
        }
        break;
      }

      ExpressionRange res = ParseOptionalRange(tok,out);
      port.range = res;
      port.name = tok->ExpectNext(TokenType_IDENTIFIER).val;

      *portList->PushElem() = port;

      peek = tok->PeekToken();
      if(peek.type == ')'){
        tok->NextToken();
        break;
      }

      tok->ExpectNext(',');
    }
    module.ports = PushArray(out,portList);
  }

  while(!tok->Done()){
    if(tok->IfNextToken(TokenType_VERILOG_KEYWORD_ENDMODULE)){
      break;
    }

    tok->NextToken();
  }

  return module;
}

Token VerilogTokenizer(void* tokenizerState,const char* start,const char* end){
  Token res = {};
  if(res.type == TokenType_INVALID) res |= ParseWhitespace(start,end);
  if(res.type == TokenType_INVALID) res |= ParseComments(start,end);
  if(res.type == TokenType_INVALID) res |= ParseVerilogPreprocess(start,end);
  if(res.type == TokenType_INVALID) res |= ParseCString(start,end);
  if(res.type == TokenType_INVALID) res |= ParseMultiSymbol(start,end,"(*",TokenType_VERILOG_ATTRIBUTE_START);
  if(res.type == TokenType_INVALID) res |= ParseMultiSymbol(start,end,"*)",TokenType_VERILOG_ATTRIBUTE_END);
  if(res.type == TokenType_INVALID) res |= ParseSymbols(start,end);

  if(res.type == TokenType_INVALID){
    V_ParsedNumber num = V_ParseNumber(start,end);

    if(num.bytesParsed > 0){
      res.type = TokenType_NUMBER;
      res.val = String(start,num.bytesParsed);
    }
  }

  //res |= ParseNumber(start,end);
  res |= ParseIdentifier(start,end);

  if(res.type == TokenType_IDENTIFIER){
  #define VKEYWORD(NAME,TYPE) if(res.val == NAME){ \
    res.type = TYPE; \
  }

  VKEYWORD("module",TokenType_VERILOG_KEYWORD_MODULE);
  VKEYWORD("endmodule",TokenType_VERILOG_KEYWORD_ENDMODULE);
  VKEYWORD("parameter",TokenType_VERILOG_KEYWORD_PARAMETER);
  VKEYWORD("signed",TokenType_VERILOG_KEYWORD_SIGNED);
  VKEYWORD("input",TokenType_VERILOG_KEYWORD_INPUT);
  VKEYWORD("output",TokenType_VERILOG_KEYWORD_OUTPUT);
  VKEYWORD("inout",TokenType_VERILOG_KEYWORD_INOUT);
  VKEYWORD("reg",TokenType_VERILOG_KEYWORD_REG);
  VKEYWORD("wire",TokenType_VERILOG_KEYWORD_WIRE);

  #undef VKEYWORD
  }

  return res;
}

Array<Module> ParseVerilogFile(String fileContent,Array<String> includeFilepaths,Arena* out){
  TEMP_REGION(temp,out);

#if 0
  Tokenizer tokenizer = Tokenizer(fileContent,"\n:',()[]{}\"+-/*=",{"#(","+:","-:","(*","*)"});
  Tokenizer* tok = &tokenizer;
#endif

  String res = PreprocessVerilogFile(fileContent,out);
  
  FREE_ARENA(tokenizer);
  FREE_ARENA(parsing);

  Parser* parser = StartParsing(VerilogTokenizer,res,parsing);

  ArenaList<Module>* modules = PushList<Module>(temp);

  bool isSource = false;
  while(!parser->Done()){
    Token peek = parser->PeekToken();
    
    if(peek.type == TokenType_VERILOG_ATTRIBUTE_START){
      parser->NextToken();

      Token attribute = parser->ExpectNext(TokenType_IDENTIFIER);

      if(attribute.type == TokenType_IDENTIFIER && attribute.val == "source"){
        isSource = true;
      } else {
        // TODO: Report unused attribute.
        //NOT_IMPLEMENTED("Should not give an error"); // Unknown attribute, error for now
      }

      parser->ExpectNext(TokenType_VERILOG_ATTRIBUTE_END);

      continue;
    }

    if(peek.type == TokenType_VERILOG_KEYWORD_MODULE){
      Module module = ParseModule(parser,out);

      module.isSource = isSource;
      *modules->PushElem() = module;
      
      isSource = false;
    }

    parser->NextToken();
  }

  for(String error : parser->errors){
    printf("%.*s\n",UN(error));
  }

  return PushArray(out,modules);
}

ModuleInfo ExtractModuleInfo(Module& module,Arena* out){
  TEMP_REGION(temp,out);

  ModuleInfo info = {};

  info.defaultParameters = module.parameters;

  auto inputs = StartGrowableArray<PortInfo>(out);
  auto outputs = StartGrowableArray<PortInfo>(out);
  auto configs = StartGrowableArray<WireExpression>(out);
  auto states = StartGrowableArray<WireExpression>(out);

  info.name = PushString(out,module.name);
  info.isSource = module.isSource;

  auto* external = PushTrieMap<ExternalMemoryID,ExternalMemoryInfo>(temp);
  
  for(PortDeclaration decl : module.ports){
    String name = decl.name;
    
    if(CompareString("signal_loop",decl.name)){
      info.singleInterfaces |= SingleInterfaces_SIGNAL_LOOP;
    } else if(CheckFormat("ext_dp_%s_%d_port_%d",decl.name)){
      Array<Value> values = ExtractValues("ext_dp_%s_%d_port_%d",decl.name,temp);

      ExternalMemoryID id = {};
      id.interface = values[1].number;
      id.type = ExternalMemoryType_DP;

      String wire = values[0].str;
      int port = values[2].number;

      Assert(port < 2);

      ExternalMemoryInfo* ext = external->GetOrInsert(id,{});
      if(CompareString(wire,"addr")){
        ext->dp[port].bitSize = decl.range; //SymbolicExpressionFromVerilog(decl.range,out); // decl.range;
      } else if(CompareString(wire,"out")){
        ext->dp[port].dataSizeOut = decl.range;
      } else if(CompareString(wire,"in")){
        ext->dp[port].dataSizeIn = decl.range;
      } else if(CompareString(wire,"write")){
        ext->dp[port].write = true;
      } else if(CompareString(wire,"enable")){
        ext->dp[port].enable = true;
      }
    } else if(CheckFormat("ext_2p_%s",decl.name)){
      ExternalMemoryID id = {};
      id.type = ExternalMemoryType_2P;

      String wire = {};
	  bool out = false;
      if(CheckFormat("ext_2p_%s_%s_%d",decl.name)){
        Array<Value> values = ExtractValues("ext_2p_%s_%s_%d",decl.name,temp);

        wire = values[0].str;
		String outOrIn = values[1].str;
		if(CompareString(outOrIn,"out")){
		  out = true;
		} else if(CompareString(outOrIn,"in")){
		  out = false;
		} else {
		  Assert(false && "Either out or in is mispelled or not present\n");
		}
        id.interface = values[2].number;
      } else if(CheckFormat("ext_2p_%s_%d",decl.name)){
        Array<Value> values = ExtractValues("ext_2p_%s_%d",decl.name,temp);

        wire = values[0].str;
        id.interface = values[1].number;
      } else {
        UNHANDLED_ERROR("TODO: Should be an handled error");
      }

      ExternalMemoryInfo* ext = external->GetOrInsert(id,{});

      if(CompareString(wire,"addr")){
		if(out){
		  ext->tp.bitSizeOut = decl.range;
		} else {
          ext->tp.bitSizeIn = decl.range; // We are using the second port to store the address despite the fact that it's only one port. It just has two addresses.
		}
      } else if(CompareString(wire,"data")){
		if(out){
          ext->tp.dataSizeOut = decl.range;
		} else {
          ext->tp.dataSizeIn = decl.range;
		}
      } else if(CompareString(wire,"write")){
        ext->tp.write = true;
      } else if(CompareString(wire,"read")){
        ext->tp.read = true;
      } else {
        UNHANDLED_ERROR("Should be an handled error");
      }
    } else if(CheckFormat("in%d",decl.name)){
      name = Offset(name,2);
      int index = ParseInt(name);
      Value* delayValue = decl.attributes->Get(VERSAT_LATENCY);

      int delay = 0;
      if(delayValue) delay = delayValue->number;

      inputs[index].delay = delay;
      inputs[index].range = decl.range;
    } else if(CheckFormat("out%d",decl.name)){
      name = Offset(name,3);
      int index = ParseInt(name);
      Value* latencyValue = decl.attributes->Get(VERSAT_LATENCY);

      int latency = 0;
      if(latencyValue) latency = latencyValue->number;

      outputs[index].delay = latency;
      outputs[index].range = decl.range;
    } else if(CheckFormat("delay%d",decl.name)){
      name = Offset(name,5);
      int delay = ParseInt(name);

      info.nDelays = MAX(info.nDelays,delay + 1);
    } else if(  CheckFormat("databus_ready_%d",decl.name)
				|| CheckFormat("databus_valid_%d",decl.name)
				|| CheckFormat("databus_addr_%d",decl.name)
				|| CheckFormat("databus_rdata_%d",decl.name)
				|| CheckFormat("databus_wdata_%d",decl.name)
				|| CheckFormat("databus_wstrb_%d",decl.name)
				|| CheckFormat("databus_len_%d",decl.name)
				|| CheckFormat("databus_last_%d",decl.name)){
      Array<Value> val = ExtractValues("databus_%s_%d",decl.name,temp);

      if(CheckFormat("databus_addr_%d",decl.name)){
        info.databusAddrSize = decl.range;
      }

      info.nIO = val[1].number;
      info.doesIO = true;
    } else if(CheckFormat("rvalid",decl.name)
		   || CheckFormat("valid",decl.name)
		   || CheckFormat("addr",decl.name)
		   || CheckFormat("rdata",decl.name)
		   || CheckFormat("wdata",decl.name)
		   || CheckFormat("wstrb",decl.name)){
      info.memoryMapped = true;

      if(CheckFormat("addr",decl.name)){
        info.memoryMappedBits = decl.range;
      }
    } else if(CheckFormat("clk",decl.name)){
      info.singleInterfaces |= SingleInterfaces_CLK;
    } else if(CheckFormat("rst",decl.name)){
      info.singleInterfaces |= SingleInterfaces_RESET;
    } else if(CheckFormat("run",decl.name)){
      info.singleInterfaces |= SingleInterfaces_RUN;
    } else if(CheckFormat("running",decl.name)){
      info.singleInterfaces |= SingleInterfaces_RUNNING;
    } else if(CheckFormat("done",decl.name)){
      info.singleInterfaces |= SingleInterfaces_DONE;
    } else if(decl.type == WireDir_INPUT){ // Config
      WireExpression* wire = configs.PushElem();

      Value* stageValue = decl.attributes->Get(VERSAT_STAGE);

      VersatStage stage = VersatStage_COMPUTE;
      
      if(stageValue && stageValue->type == ValueType_STRING){
        String val = stageValue->str;

        if(CompareString(val,"Write")){
          stage = VersatStage_WRITE;
        } else if(CompareString(val,"Read")){
          stage = VersatStage_READ;
        } else {
          Assert(false);
        }
      }
      
      wire->bitSize = decl.range;
      wire->name = decl.name;
      wire->isStatic = decl.attributes->Exists(VERSAT_STATIC);
      wire->stage = stage;
    } else if(decl.type == WireDir_OUTPUT){ // State
      WireExpression* wire = states.PushElem();

      wire->bitSize = decl.range;
      wire->name = decl.name;
    } else {
      NOT_IMPLEMENTED("Implemented as needed, so far all if cases handles all cases so we should never reach here");
    }
  }

  info.configs = configs.AsArray();
  info.states = states.AsArray();
  info.inputs = inputs.AsArray();
  info.outputs = outputs.AsArray();

  if(info.doesIO){
    info.nIO += 1;
  }

  Array<ExternalMemoryInterfaceExpression> interfaces = PushArray<ExternalMemoryInterfaceExpression>(out,external->inserted);
  int index = 0;
  for(Pair<ExternalMemoryID,ExternalMemoryInfo> pair : external){
    ExternalMemoryInterfaceExpression& inter = interfaces[index++];

    inter.interface = pair.first.interface;
    inter.type = pair.first.type;

	switch(inter.type){
	case ExternalMemoryType::ExternalMemoryType_2P:{
	  inter.tp = pair.second.tp;
	} break;
	case ExternalMemoryType::ExternalMemoryType_DP:{
	  inter.dp[0] = pair.second.dp[0];
	  inter.dp[1] = pair.second.dp[1];
	}break;
	}
  }
  info.externalInterfaces = interfaces;

  return info;
}

String PreprocessVerilogFile(String content,Arena* out){
  TEMP_REGION(temp,out);

  auto Tokenizer = [](void* tokenizerState,const char* start,const char* end) -> Token{
    DefaultTokenizerState* state = (DefaultTokenizerState*) tokenizerState;
    
    Token res = {};
    if(res.type == TokenType_INVALID) res |= ParseWhitespace(start,end,ParseWhitespaceOptions_NONE);
    if(res.type == TokenType_INVALID) res |= ParseNewline(start,end);
    if(res.type == TokenType_INVALID) res |= ParseVerilogPreprocess(start,end);
    if(res.type == TokenType_INVALID) res |= ParseComments(start,end);
    if(res.type == TokenType_INVALID) res |= ParseCString(start,end);
    if(res.type == TokenType_INVALID) res |= ParseIdentifier(start,end);
    if(res.type == TokenType_INVALID) res |= ParseSymbols(start,end);
    if(res.type == TokenType_INVALID) res |= ParseNumber(start,end);
    
    return res;
  };

  FREE_ARENA(parsing);
  Parser* p = StartParsing(Tokenizer,content,parsing,ParsingOptions_NONE);

  struct DefineInfo{
    DefineInfo* next;

    String name;
    Array<String> args;
    Array<Token> tokens;
  };

  struct Env{
    Env* parent;
    bool onceActive;
    bool active;
  };

  DefineInfo* defineHead = 0;
  DefineInfo* definePtr = 0;

  Env topEnv = {};
  topEnv.active = 1;

  Env* env = &topEnv;
  
  auto b = StartString(temp);

  struct Work{
    Work* parent;
    DefineInfo* currentDefine;
    Array<TokenNode*> paramValues;
    TokenNode* currentParam;
    int currentToken;
  };

  Work* workPtr = nullptr;

  auto PeekToken = [&](ParsingOptions opts = {}) -> Token{
    Token t = {};
    if(workPtr){
      DefineInfo* define = workPtr->currentDefine;
      TokenNode* currentParam = workPtr->currentParam;

      // Check if we just outputting define tokens ==================================
      if(!currentParam){
        if(workPtr->currentToken < define->tokens.size){
          t = define->tokens[workPtr->currentToken];
        }
      }

      // If token is a param then start replacing tokens with args ==================
      String name = t.val;
      int argIndex = -1;
      for(int i = 0; i <  define->args.size; i++){
        String str  =  define->args[i];
        if(name == str){
          argIndex = i;
          break;
        }
      }

      if(argIndex != -1){
        currentParam = workPtr->paramValues[argIndex];
      }

      if(currentParam){
        t = currentParam->val;
      } 
    } else {
      t = p->PeekToken(opts);
    }

    return t;
  };

  auto NextToken = [&](ParsingOptions opts = {}) -> Token{
    Token t = {};
    if(workPtr){
      DefineInfo* define = workPtr->currentDefine;
      TokenNode* currentParam = workPtr->currentParam;

      // Check if we just outputting define tokens ==================================
      if(!currentParam){
        if(workPtr->currentToken < define->tokens.size){
          t = define->tokens[workPtr->currentToken];
        }
        workPtr->currentToken += 1;
      }

      // If token is a param then start replacing tokens with args ==================
      String name = t.val;
      int argIndex = -1;
      for(int i = 0; i <  define->args.size; i++){
        String str  =  define->args[i];
        if(name == str){
          argIndex = i;
          break;
        }
      }

      if(argIndex != -1){
        currentParam = workPtr->paramValues[argIndex];
      }

      if(currentParam){
        t = currentParam->val;
        currentParam = currentParam->next;
      } 

      workPtr->currentParam = currentParam;

      // After processing last token delete current work ============================
      if(workPtr->currentToken >= define->tokens.size && !currentParam){
        workPtr = workPtr->parent;
      }
    } else {
      t = p->NextToken(opts);
    }

    return t;
  };

  auto Done = [&]() -> bool{
    if(!workPtr && p->Done()){
      return true;
    }
    return false;
  };

  auto ExpectNext = [&](TokenType type) -> Token{
    Token t = NextToken();
    return t;
  };

  auto IfPeekToken = [&](TokenType type) -> bool{
    Token t = PeekToken();
    if(t.type == type){
      return true;
    }
    return false;
  };

  auto IfNextToken = [&](TokenType type) -> bool{
    Token t = PeekToken();
    if(t.type == type){
      NextToken();
      return true;
    }
    return false;
  };

  while(!Done()){
    Token t = NextToken(ParsingOptions_ALLOW_ALL);
    
    // This is the code to process a token.
    // Define only stores tokens (does not process).
    // Instantiantion then processes the tokens from a define.
    bool active = 1;

    for(Env* ptr = env; ptr; ptr = ptr->parent){
      active &= ptr->active;
    }

    bool isCond = t.type == TokenType_VERILOG_IFDEF ||
                  t.type == TokenType_VERILOG_IFNDEF ||
                  t.type == TokenType_VERILOG_ELSIF ||
                  t.type == TokenType_VERILOG_ELSE ||
                  t.type == TokenType_VERILOG_ENDIF;

    if(active || (!active && isCond)){
      bool negateCond = 0;
      bool skip = 1;

      switch(t.type){
      case TokenType_VERILOG_INCLUDE:{
        Token tokenString = ExpectNext(TokenType_C_STRING);
        String filepath = PARSE_GetStringContent(tokenString);
        FileContent content = GetContentsOfFile(filepath,FilePurpose_VERILOG_INCLUDE);

        if(content.state == FileContentState_FAILED_TO_LOAD){
          p->ReportError(SF("Cannot find include file '%.*s'",UN(filepath)));
          // Better error reporting, specify which folders we looked for
        } else {
          String processed = PreprocessVerilogFile(content.content,temp);
          b->PushString(processed);
        }
      } break;

      case TokenType_VERILOG_DEFINE:{
        Token toDefine = ExpectNext(TokenType_IDENTIFIER);
        String name = toDefine.val;

        Array<String> args = {};
        Array<Token> tokens = {};

        Token peek = PeekToken(ParsingOptions_ALLOW_NEWLINE);
        if(peek.type == TOK_TYPE('(')){
          NextToken(ParsingOptions_ALLOW_NEWLINE);

          auto l = PushList<String>(temp);
          while(!Done()){
            if(IfPeekToken(TOK_TYPE(')'))){
              break;
            }

            Token arg = ExpectNext(TokenType_IDENTIFIER);
            *l->PushElem() = arg.val;

            if(IfNextToken(TOK_TYPE(','))){
              continue;
            }

            break;
          }
          ExpectNext(TOK_TYPE(')'));

          args = PushArray(temp,l);
        }

        auto t = PushList<Token>(temp);
        bool ignoreNewline = false;
        while(!Done()){
          Token token = NextToken(ParsingOptions_ALLOW_NEWLINE | ParsingOptions_ALLOW_NEWLINE);

          if(token.type == TOK_TYPE('\\')){
            ignoreNewline = true;
            continue;
          }

          if(token.type == TokenType_NEWLINE){
            if(ignoreNewline){
              ignoreNewline = false;
            } else {
              break;
            }
          }

          *t->PushElem() = token;
        }
        tokens = PushArray(temp,t);

        DefineInfo* node = 0;
        LL_Find(defineHead,next,node,it->name == name);

        bool alreadyExists = (node != nullptr);
        if(!node){
          node = PushStruct<DefineInfo>(temp);
        }

        node->args = args;
        node->name = toDefine.val;
        node->tokens = tokens;

        if(!alreadyExists){
          LL_Append(defineHead,definePtr,next,node);
        }
      } break;

      case TokenType_VERILOG_PREPROCESS:{
        Token define = t;
        String name = Offset(define.val,1);

        Array<TokenNode*> args = {};
        if(IfNextToken(TOK_TYPE('('))){
          auto l = PushList<TokenNode*>(temp);
          while(!Done()){
            if(IfPeekToken(TOK_TYPE(')'))){
              break;
            }

            TokenNode* head = 0;
            TokenNode* ptr = 0;

            while(!Done()){
              if(IfPeekToken(TOK_TYPE(')'))){
                break;
              }

              if(IfPeekToken(TOK_TYPE(','))){
                break;
              }

              Token arg = NextToken();
              TokenNode* node = PushStruct<TokenNode>(temp);
              node->val = arg;
              LL_Append(head,ptr,next,node);
            }

            *l->PushElem() = head;

            if(IfNextToken(TOK_TYPE(','))){
              continue;
            }

            break;
          }
          args = PushArray(temp,l);

          ExpectNext(TOK_TYPE(')'));
        }

        DefineInfo* node = 0;
        LL_Find(defineHead,next,node,it->name == name);

        if(!node){
          // Error, define not found
        }

        if(node){
          if(node->args.size != args.size){
            // Error, not enough args.
          }
        
          Work* newWork = PushStruct<Work>(temp);
          newWork->parent = workPtr;
          newWork->currentDefine = node;
          newWork->paramValues = args;

          workPtr = newWork;
        }
      } break;

      case TokenType_VERILOG_UNDEF:{
        Token toUndef = ExpectNext(TokenType_IDENTIFIER);
        String name = toUndef.val;

        DefineInfo* prev = 0;
        DefineInfo* node = 0;
        LL_FindPrev(defineHead,next,node,prev,it->name == name);
        LL_Remove(defineHead,definePtr,next,node,prev);
      } break;

      case TokenType_VERILOG_TIMESCALE:{
        ExpectNext(TokenType_NUMBER);
        ExpectNext(TokenType_IDENTIFIER);

        ExpectNext(TOK_TYPE('/'));

        ExpectNext(TokenType_NUMBER);
        ExpectNext(TokenType_IDENTIFIER);
      } break;

      case TokenType_VERILOG_IFNDEF: negateCond = 1; // fallthrough
      case TokenType_VERILOG_IFDEF:{
        Token def = ExpectNext(TokenType_IDENTIFIER);
        String name = def.val;

        DefineInfo* node = 0;
        LL_Find(defineHead,next,node,it->name == name);

        Env* layer = PushStruct<Env>(temp);
        layer->parent = env;
        env = layer;

        if(negateCond && !node){
          layer->active = 1;
        }
        if(!negateCond && node){
          layer->active = 1;
        }
      } break;

      case TokenType_VERILOG_ELSE:{
        env->active = !env->active;
      } break;

      case TokenType_VERILOG_ELSIF:{
        Token def = ExpectNext(TokenType_IDENTIFIER);
        String name = def.val;

        DefineInfo* node = 0;
        LL_Find(defineHead,next,node,it->name == name);

        if(node && !env->onceActive){
          env->active = 1;
        }
      } break;

      case TokenType_VERILOG_ENDIF:{
        if(env == &topEnv){
          // Error, naked endif
        }

        env = env->parent;
      } break;

      case TokenType_VERILOG_LINE:{
        ExpectNext(TokenType_NUMBER);
        ExpectNext(TokenType_C_STRING);
        ExpectNext(TokenType_NUMBER);
      } break;

      case TokenType_VERILOG_UNCONNECTED_DRIVE:{
        ExpectNext(TokenType_IDENTIFIER);
      } break;

      case TokenType_VERILOG_NOUNCONNECTED_DRIVE:// fallthrough
      case TokenType_VERILOG_CELLDEFINE:// fallthrough
      case TokenType_VERILOG_ENDCELLDEFINE:// fallthrough
      case TokenType_VERILOG_END_KEYWORDS:{
        // Nothing
      } break;

      case TokenType_VERILOG_PRAGMA:{
        // NOTE: Not proper but do not care about pragmas right now
        while(!Done()){
          Token t = NextToken();
          if(t.type == TokenType_NEWLINE){
            break;
          }
        }
      } break;
      
      case TokenType_VERILOG_BEGIN_KEYWORDS:{
        ExpectNext(TokenType_C_STRING);
      } break;

      case TokenType_VERILOG_DEFAULT_NETTYPE:{
        /* Token nettype = */ ExpectNext(TokenType_IDENTIFIER); // Consume default nettype
        // TODO: Can improve error reporting if needed by checking nettype;
      } break;

      case TokenType_VERILOG_RESETALL:{
        defineHead = 0;
        definePtr = 0;
      } break;

      default: skip = 0; break;
      }

      env->onceActive |= env->active;

      if(!skip && env->active){
        b->PushString(t.val);
      }
    }
  }

  String res = EndString(out,b);

  // TODO: Errors
  return res;
}

static b32 V_IsWhitespace(char ch){
  b32 res = (ch == '\n' || ch == '\t' || ch == '\r' || ch == ' ');
  return res;
}

static b32 V_IsSymbol(char ch){
  b32 res = false;
  res |= (ch >= 33 && ch <= 47);
  res |= (ch >= 58 && ch <= 64);
  res |= (ch >= 91 && ch <= 96);
  res |= (ch >= 123 && ch <= 126);
  return res;
}

static b32 V_IsAlpha(char ch){
  b32 res = ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_');
  return res;
}

static b32 V_IsErrorDigit(char ch){
  b32 res = (ch == 'z' || ch == 'Z' || ch == 'x' || ch == 'X' || ch == '?');
  return res;
}

static b32 V_IsBinaryDigit(char ch){
  b32 res = (ch == '0' || ch == '1');
  return res;
}

static b32 V_IsDecimalDigit(char ch){
  b32 res = (ch >= '0' && ch <= '9');
  return res;
}

static b32 V_IsHexadecimalDigit(char ch){
  b32 res = V_IsDecimalDigit(ch) || ((ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'));
  return res;
}


V_ParsedNumber V_ParseNumber(const char* start,const char* end,Arena* out){
  const char* ptr = start;

  if(start >= end){
    return {};
  }

  char ch = *start;

  if(ch >= '0' && ch <= '9' ||
     ch == '\''){
  } else {
    return {}; // Definitely not a number, return early.
  }

  // TODO: In theory every time we see a non valid character we could error report it.
  //       Currently we just ignore it and break from loops early, but we know that 
  //       something like a (Ex: 100y) is not allowed since y is never a valid character.
  //       Symbols terminate early since 100+200 is allowed. Basically only certains parts of 
  //       the alphanumeric group of characters can trigger invalid errors. Symbols and whitespaces
  //       just end the parsing immediatly.

  // Parse first section ========================================================
  const char* startOfFirstSection = ptr;

  b32 anyError = 0;

  b32 isReal = 0;
  b32 seenExp = 0;
  b32 seenDot = 0;

  b32 isSized = 0;
  b32 canBeSize = 0;

  b32 finished = 0; // Only asserted if we see a character that is not allowed.
  
  auto NotDone = [&finished,ptr,end](){return (!finished && ptr < end);};
  
  while(NotDone()){
    b32 firstChar = (ptr == startOfFirstSection);
    char ch = *ptr;
    
    if(ch == '.'){
      isReal = 1;
      seenDot = 1;
      ptr += 1;
      break;
    }
    if(ch == '\''){
      isSized = 1;
      ptr += 1;
      break;
    }
    if(ch == 'e' || ch == 'E'){
      isReal = 1;
      seenExp = 1;
      ptr += 1;
      break;
    }

    b32 validChar = 0;

    if(!firstChar && ch == '_'){
      validChar = 1;
    }
    if(V_IsDecimalDigit(ch)){
      validChar = 1;
    }
    if(V_IsWhitespace(ch) || V_IsSymbol(ch)){
      finished = 1;
      break;
    }
    
    // NOTE: Size cannot start with a zero, for some reason.
    if(firstChar && ch == '0'){
      canBeSize = 0;
    }

    if(validChar){
      ptr += 1;
      continue;
    }

    break;
  }

  b32 isUnsizedDecimal = 0;
  if(!isReal && !isSized){
    isUnsizedDecimal = 1;
  }

  V_NumberType numberType = V_NumberType_NIL;

  String firstSection = String(startOfFirstSection,ptr - startOfFirstSection);

  // Handle real numbers ========================================================
  f64 realNumber = 0.0f;
  if(isReal){
    String afterDot = {};

    if(seenDot){
      const char* afterDotStart = ptr;
      while(NotDone()){
        b32 firstChar = (ptr == afterDotStart);
        char ch = *ptr;
        
        if(ch == 'e' || ch == 'E'){
          seenExp = 1;
          ptr += 1;
          break;
        }

        b32 validChar = 0;

        if(!firstChar && ch == '_'){
          validChar = 1;
        }
        if(V_IsDecimalDigit(ch)){
          validChar = 1;
        }
        if(V_IsWhitespace(ch) || V_IsSymbol(ch)){
          finished = 1;
          break;
        }

        if(validChar){
          ptr += 1;
          continue;
        }

        break;
      }

      afterDot = String(afterDotStart,ptr - afterDotStart);
    }

    String afterExp = {};
    if(seenExp){
      b32 minus = 0;

      if(NotDone()){
        if(*ptr == '+'){
          ptr += 1;
        }
        if(*ptr == '-'){
          minus = 1;
          ptr += 1;
        }
      }

      const char* afterExpStart = ptr;
      
      while(NotDone()){
        b32 firstChar = (ptr == afterExpStart);
        char ch = *ptr;
        
        b32 validChar = 0;

        if(!firstChar && ch == '_'){
          validChar = 1;
        }
        if(V_IsDecimalDigit(ch)){
          validChar = 1;
        }
        if(V_IsWhitespace(ch) || V_IsSymbol(ch)){
          finished = 1;
          break;
        }

        if(validChar){
          ptr += 1;
          continue;
        }

        break;
      }
      
      afterExp = String(afterExpStart,ptr - afterExpStart);
    }

    if(seenDot && Empty(afterDot)){
      // TODO: Error
    }
    if(seenExp && Empty(afterExp)){
      // TODO: Error
    }

    NOT_IMPLEMENTED("TODO: firstSection + afterDot + afterExp -> f64 number");
    
    numberType = V_NumberType_REAL;
  }

  // Handle sized ===============================================================
  b32 bitsizeGiven = 0;
  b32 signedSpecificer = 0;
  String asBinary = {};
  if(isSized){
    if(NotDone()){
      if(*ptr == 's' || *ptr == 'S'){
        signedSpecificer = 1;
        ptr += 1;
      }
    }
    
    if(NotDone()){
      char ch = *ptr;

      switch(ch){
      case 'd': // fallthrough
      case 'D': {
        numberType = V_NumberType_DECIMAL;
      } break;
      
      case 'b': // fallthrough
      case 'B': {
        numberType = V_NumberType_BINARY;
      } break;

      case 'o': // fallthrough
      case 'O': {
        numberType = V_NumberType_OCTO;
      } break;

      case 'h': // fallthrough
      case 'H': {
        numberType = V_NumberType_HEXADECIMAL;
      } break;

      default: break;// TODO:Error on base
      }

      ptr += 1;
    }

    const char* afterBaseStart = ptr;
    while(NotDone()){
      b32 firstChar = (ptr == afterBaseStart);
      char ch = *ptr;
        
      b32 validChar = 0;

      if(!firstChar && ch == '_'){
        validChar = 1;
      }
      // TODO: Only parse the type specifics and report error otherwise
      if(V_IsHexadecimalDigit(ch)){
        validChar = 1;
      }
      if(V_IsWhitespace(ch) || V_IsSymbol(ch)){
        finished = 1;
        break;
      }

      if(validChar){
        ptr += 1;
        continue;
      }

      break;
    }
    
    String afterBase = String(afterBaseStart,ptr - afterBaseStart);

    firstSection.size -= 1; // NOTE: Remove the '
    for(char ch : firstSection){
      b32 isDigit = ch >= '0' && ch <= '9';
      if(!isDigit){
        anyError = 1;
        break;
      }
      
      bitsizeGiven *= 10;
      bitsizeGiven += (ch - '0');
    }
    
    if(out && !anyError){
      TEMP_REGION(temp,out);
      
      auto b = StartString(temp);

      if(numberType == V_NumberType_DECIMAL){
        u64 asNumber = 0;
        for(char ch : firstSection){
          if(ch == '_'){
            continue;
          }

          b32 isDigit = ch >= '0' && ch <= '9';
          if(!isDigit){
            anyError = 1;
            break;
          }

          asNumber *= 10;
          asNumber += (ch - '0');
        }
        
        while(!anyError && asNumber != 0){
          u32 offset = asNumber % 2;
          b->PushString(offset ? "1" : "0");
          asNumber /= 2;
        }

        String reversedBinary = EndString(temp,b);
        asBinary = Reverse(reversedBinary,out);
      } else {
        for(char ch : afterBase){
          if(ch == '_'){
            continue;
          }

          switch(numberType){
          case V_NumberType_BINARY:{
            b->PushChar(ch);
          } break;
          case V_NumberType_OCTO:{
            switch(ch){
            case '0': b->PushString("000"); break;
            case '1': b->PushString("001"); break;
            case '2': b->PushString("010"); break;
            case '3': b->PushString("011"); break;
            case '4': b->PushString("100"); break;
            case '5': b->PushString("101"); break;
            case '6': b->PushString("110"); break;
            case '7': b->PushString("111"); break;
            case 'x': 
            case 'X': b->PushString("xxx"); break;
            case 'z': 
            case 'Z': b->PushString("zzz"); break;
            case '?': b->PushString("???"); break;
            default: anyError = 1;
            }
          } break;
          case V_NumberType_HEXADECIMAL:{
            switch(ch){
            case '0': b->PushString("0000"); break;
            case '1': b->PushString("0001"); break;
            case '2': b->PushString("0010"); break;
            case '3': b->PushString("0011"); break;
            case '4': b->PushString("0100"); break;
            case '5': b->PushString("0101"); break;
            case '6': b->PushString("0110"); break;
            case '7': b->PushString("0111"); break;
            case '8': b->PushString("1000"); break;
            case '9': b->PushString("1001"); break;
            case 'a':
            case 'A': b->PushString("1010"); break;
            case 'b':
            case 'B': b->PushString("1011"); break;
            case 'c':
            case 'C': b->PushString("1100"); break;
            case 'd':
            case 'D': b->PushString("1101"); break;
            case 'e':
            case 'E': b->PushString("1110"); break;
            case 'f':
            case 'F': b->PushString("1111"); break;
            case 'x': 
            case 'X': b->PushString("xxxx"); break;
            case 'z': 
            case 'Z': b->PushString("zzzz"); break;
            case '?': b->PushString("????"); break;
            default: anyError = 1;
            }
          } break;

          default: anyError = 1;
          }
        }

        asBinary = EndString(out,b);
      }
    }
  }

  // Handle unsized decimals ====================================================
  u64 decimalNumber = 0;
  if(isUnsizedDecimal){
    for(char ch : firstSection){
      if(ch == '_'){
        continue;
      }
      b32 isDigit = ch >= '0' && ch <= '9';

      if(!isDigit){
        anyError = 1;
        break;
      }

      decimalNumber *= 10;
      decimalNumber += (ch - '0');
    }
    numberType = V_NumberType_DECIMAL;
  }  

  // Pack =======================================================================
  V_ParsedNumber res = {};
  res.type = numberType;
  res.isSized = isSized;
  res.bitsizeGiven = bitsizeGiven;
  res.isSigned = signedSpecificer;
  res.realNumber = realNumber;
  res.decimalNumber = decimalNumber;
  res.bytesParsed = ptr - start;
  res.asBinary = asBinary;
  res.anyError = anyError;

  return res;
}
