#include "parser.hpp"
#include "filesystem.hpp"
#include "utilsCore.hpp"
#include "debug.hpp"

static bool IsAlpha(char ch){
  bool res = ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'); 
  return res;
};

static bool IsNumeric(char ch){
  bool res = (ch >= '0' && ch <= '9');
  return res;
};

static bool IsCharSingleToken(char ch){
  bool res = false;

  if(ch >= TokenType_CHAR_GROUP_0_START && ch <= TokenType_CHAR_GROUP_0_LAST){
    res = true;
  }
  if(ch >= TokenType_CHAR_GROUP_1_START && ch <= TokenType_CHAR_GROUP_1_LAST){
    res = true;
  }
  if(ch >= TokenType_CHAR_GROUP_2_START && ch <= TokenType_CHAR_GROUP_2_LAST){
    res = true;
  }
  if(ch >= TokenType_CHAR_GROUP_3_START && ch <= TokenType_CHAR_GROUP_3_LAST){
    res = true;
  }
  
  return res;
}

String PushRepr(Arena* out,TokenType type){
  String res = {};
  if(IsCharSingleToken((char) type)){
    res = PushString(out,"'%c'",(char) type);
  }
  if(type == TokenType_WHITESPACE){
    res = "Whitespace";
  }
  if(type == TokenType_NEWLINE){
    res = "Newline";
  }
  if(type == TokenType_COMMENT){
    res = "Comment";
  }
  if(type == TokenType_MULTILINE_COMMENT){
    res = "Comment";
  }
  if(type == TokenType_EOF){
    res = "EOF";
  }
  if(type == TokenType_C_KEYWORD){
    res = "C Reserved Keyword";
  }
  if(type == TokenType_IDENTIFIER){
    res = "Identifier";
  }
  if(type == TokenType_NUMBER){
    res = "Number";
  }
  if(type == TokenType_FILEPATH){
    res = "Filepath";
  }

  if(type >= TokenType_START_OF_KEYWORDS && type < TokenType_END_OF_KEYWORDS){
    res = "Keyword";
  }

#define SIMPLE(TYPE,TYPENAME,RET) if(type == TYPE) res = PushString(out,"[%s] '%s'",TYPENAME,RET)
  SIMPLE(TokenType_DOUBLE_DOT,"DoubleChar","..");
  SIMPLE(TokenType_DOUBLE_HASHTAG,"DoubleChar","##");
  SIMPLE(TokenType_ARROW,"DoubleChar","->");
  SIMPLE(TokenType_SHIFT_RIGHT,"DoubleChar",">>");
  SIMPLE(TokenType_SHIFT_LEFT,"DoubleChar","<<");

  SIMPLE(TokenType_ROTATE_RIGHT,"TripleChar",">><");
  SIMPLE(TokenType_ROTATE_LEFT,"DoubleChar","><<");

  SIMPLE(TokenType_VERILOG_ATTRIBUTE_START,"DoubleChar","(*");
  SIMPLE(TokenType_VERILOG_ATTRIBUTE_END,"DoubleChar","*)");

  SIMPLE(TokenType_KEYWORD_MODULE,"Keyword","module");
  SIMPLE(TokenType_KEYWORD_MERGE,"Keyword","merge");
  SIMPLE(TokenType_KEYWORD_SHARE,"Keyword","share");
  SIMPLE(TokenType_KEYWORD_STATIC,"Keyword","static");
  SIMPLE(TokenType_KEYWORD_DEBUG,"Keyword","debug");
  SIMPLE(TokenType_KEYWORD_SIM,"Keyword","sim");
  SIMPLE(TokenType_KEYWORD_CONFIG,"Keyword","config");
  SIMPLE(TokenType_KEYWORD_STATE,"Keyword","state");
  SIMPLE(TokenType_KEYWORD_MEM,"Keyword","mem");
  SIMPLE(TokenType_KEYWORD_GEN,"Keyword","gen");
  SIMPLE(TokenType_KEYWORD_FOR,"Keyword","for");

  SIMPLE(TokenType_VERILOG_DEFINE,"Verilog","define");
  SIMPLE(TokenType_VERILOG_UNDEF,"Verilog","undef");

  SIMPLE(TokenType_VERILOG_INCLUDE,"Verilog","include");
  SIMPLE(TokenType_VERILOG_INCLUDE,"Verilog","timescale");

  SIMPLE(TokenType_VERILOG_PREPROCESS,"Verilog","preprocess");

  SIMPLE(TokenType_VERILOG_IFDEF,"Verilog","ifdef");
  SIMPLE(TokenType_VERILOG_IFNDEF,"Verilog","ifndef");
  SIMPLE(TokenType_VERILOG_ELSE,"Verilog","else");
  SIMPLE(TokenType_VERILOG_ELSIF,"Verilog","elsif");
  SIMPLE(TokenType_VERILOG_ENDIF,"Verilog","endif");

  SIMPLE(TokenType_VERILOG_KEYWORD_PARAMETER,"Verilog","parameter");

  SIMPLE(TokenType_VERILOG_KEYWORD_MODULE,"Verilog","module");
  SIMPLE(TokenType_VERILOG_KEYWORD_ENDMODULE,"Verilog","endmodule");
  SIMPLE(TokenType_VERILOG_KEYWORD_SIGNED,"Verilog","signed");
  SIMPLE(TokenType_VERILOG_KEYWORD_INPUT,"Verilog","input");
  SIMPLE(TokenType_VERILOG_KEYWORD_OUTPUT,"Verilog","output");
  SIMPLE(TokenType_VERILOG_KEYWORD_INOUT,"Verilog","inout");
  SIMPLE(TokenType_VERILOG_KEYWORD_REG,"Verilog","reg");
  SIMPLE(TokenType_VERILOG_KEYWORD_WIRE,"Verilog","wire");
#undef SIMPLE 

  return res;
}

String PARSE_PushDebugRepr(Arena* out,Token token){
  String res = {};

  if(token.type == TokenType_IDENTIFIER){
    res = PushString(out,"[Identifier] '%.*s'",UN(token.val));
  }
  if(token.type == TokenType_NUMBER){
    res = PushString(out,"[Number] '%.*s'",UN(token.val));
  }
  if(token.type == TokenType_C_STRING){
    res = PushString(out,"\"%.*s\"",UN(token.val));
  }
  if(token.type == TokenType_FILEPATH){
    res = PushString(out,"[Filepath] %.*s",UN(token.val));
  }

  if(Empty(res)){
    res = PushRepr(out,token.type);
  }

#define SIMPLE(TYPE,TYPENAME,RET) if(token.type == TYPE) res = PushString(out,"[%s] '%s'",TYPENAME,RET)
  SIMPLE(TokenType_KEYWORD_MODULE,"Keyword","module");
  SIMPLE(TokenType_KEYWORD_MERGE,"Keyword","merge");
  SIMPLE(TokenType_KEYWORD_SHARE,"Keyword","share");
  SIMPLE(TokenType_KEYWORD_STATIC,"Keyword","static");
  SIMPLE(TokenType_KEYWORD_DEBUG,"Keyword","debug");
  SIMPLE(TokenType_KEYWORD_SIM,"Keyword","sim");
  SIMPLE(TokenType_KEYWORD_CONFIG,"Keyword","config");
  SIMPLE(TokenType_KEYWORD_STATE,"Keyword","state");
  SIMPLE(TokenType_KEYWORD_MEM,"Keyword","mem");
  SIMPLE(TokenType_KEYWORD_GEN,"Keyword","gen");
  SIMPLE(TokenType_KEYWORD_FOR,"Keyword","for");

  SIMPLE(TokenType_VERILOG_DEFINE,"Verilog","define");
  SIMPLE(TokenType_VERILOG_UNDEF,"Verilog","undef");

  SIMPLE(TokenType_VERILOG_INCLUDE,"Verilog","timescale");
  SIMPLE(TokenType_VERILOG_INCLUDE,"Verilog","include");

  SIMPLE(TokenType_VERILOG_PREPROCESS,"Verilog","preprocess");

  SIMPLE(TokenType_VERILOG_IFDEF,"Verilog","ifdef");
  SIMPLE(TokenType_VERILOG_IFNDEF,"Verilog","ifndef");
  SIMPLE(TokenType_VERILOG_ELSE,"Verilog","else");
  SIMPLE(TokenType_VERILOG_ELSIF,"Verilog","elsif");
  SIMPLE(TokenType_VERILOG_ENDIF,"Verilog","endif");

  SIMPLE(TokenType_VERILOG_KEYWORD_PARAMETER,"Verilog","parameter");

  SIMPLE(TokenType_VERILOG_KEYWORD_MODULE,"Verilog","module");
  SIMPLE(TokenType_VERILOG_KEYWORD_ENDMODULE,"Verilog","endmodule");
  SIMPLE(TokenType_VERILOG_KEYWORD_SIGNED,"Verilog","signed");
  SIMPLE(TokenType_VERILOG_KEYWORD_INPUT,"Verilog","input");
  SIMPLE(TokenType_VERILOG_KEYWORD_OUTPUT,"Verilog","output");
  SIMPLE(TokenType_VERILOG_KEYWORD_INOUT,"Verilog","inout");
  SIMPLE(TokenType_VERILOG_KEYWORD_REG,"Verilog","reg");
  SIMPLE(TokenType_VERILOG_KEYWORD_WIRE,"Verilog","wire");
#undef SIMPLE 

  if(Empty(res)){
    printf("Token type: %d\n",(int) token.type);
    Assert(false);
  }  

  return res;
}

Parser* StartParsing(TokenizeFunction tokenizer,String content,Arena* freeArena,ParsingOptions options){
  Parser* res = PushStruct<Parser>(freeArena);

  DefaultTokenizerState* tokenizerState = PushStruct<DefaultTokenizerState>(freeArena);

  res->errors = PushList<String>(freeArena);

  res->tokenizerState = (void*) tokenizerState;
  res->tokenizer = tokenizer;
  res->arena = freeArena;

  res->start = content.data;
  res->ptr = content.data;
  res->end = content.data + content.size;
  
  return res;
}

Parser* StartParsing(TokenizeFunction tokenizer,void* tokenizerState,String content,Arena* freeArena,ParsingOptions options){
  Parser* res = PushStruct<Parser>(freeArena);

  res->tokenizerState = (void*) tokenizerState;
  res->tokenizer = tokenizer;
  res->arena = freeArena;
  res->errors = PushList<String>(freeArena);

  res->start = content.data;
  res->ptr = content.data;
  res->end = content.data + content.size;

  return res;
}

Token Parser::InternalConsumeToken(ParsingOptions opts){
  Token token = {};
  token.type = TokenType_EOF;

  while(this->ptr < this->end){
    Token parsed = this->tokenizer(this->tokenizerState,this->ptr,this->end);
    this->ptr += parsed.val.size;

    bool skip = 0;

    if(!(opts & ParsingOptions_ALLOW_NEWLINE) && parsed.type == TokenType_NEWLINE){
      skip = 1;
    }
    if(!(opts & ParsingOptions_ALLOW_WHITESPACE) && parsed.type == TokenType_WHITESPACE){
      skip = 1;
    }
    if(!(opts & ParsingOptions_ALLOW_COMMENTS) && PARSE_IsComment(parsed.type)){
      skip = 1;
    }
    
    if(parsed.type == TokenType_UNTERMINATED_MULTILINE_COMMENT){
      // TODO: Improve error messages, We can show user the start of the multiline comment
      ReportError("Unterminated multiline comment");
      skip = 1;
    }

    if(skip){
      continue;
    }

    if(parsed.type == TokenType_INVALID){
      if(currentFile){
        printf("Invalid token: %s\n",currentFile);
      }
      parsed.type = TokenType_EOF;
    }

    token = parsed;
    
    break;
  }

  return token;
}

void Parser::ReportError(String error){
  *errors->PushElem() = PushString(arena,error);
}

void Parser::ReportUnexpectedToken(Token token,BracketList<TokenType> expectedList){
  TEMP_REGION(temp,nullptr);

  String tokenRepr = PARSE_PushDebugRepr(temp,token);

  auto builder = StartString(temp);
  builder->PushString("Unexpected token: %.*s\n",UN(tokenRepr));
  builder->PushString("Expected one of the following:\n");
  
  // TODO: Move tokenType to meta and proper data modelling of the token type properties.
  for(TokenType type : expectedList){
    String repr = PushRepr(temp,type);
    builder->PushString("  '%.*s'\n",UN(repr));
  }

  ENTER_DEBUG();

  *errors->PushElem() = EndString(this->arena,builder);
}

Token Parser::NextToken(ParsingOptions opts){
#if 0
  EnsureTokens(1);

  Token res = this->storedTokens[0];
  for(int i = 0; i < this->amountStored - 1; i++){
    this->storedTokens[i] = this->storedTokens[i+1];
  }
  this->amountStored -= 1;
#endif

  Token res = InternalConsumeToken(opts);

  if(this->debug){
    TEMP_REGION(temp,arena);
    
    bool hasError = !Empty(this->errors);

    DEBUG_AddLocation(debugLocHead,debugLocTail,arena);
    
    // NOTE: Slow but we want to output immediatly
    Array<LocationNode*> list = DEBUG_DepthFirst(debugLocHead,temp);
    int lastLevel = 0;
    for(int i = lastDebugIndex; i < list.size; i++){
      LocationNode* node = list[i];
      if(!Contains(node->loc.functionName,"SP")){
        continue;
      }
      lastLevel = node->level;
    }

    for(int i = lastDebugIndex; i < list.size; i++){
      LocationNode* node = list[i];

      if(!Contains(node->loc.functionName,"SP")){
        continue;
      }
      
      if(hasError){
        printf("--->");
      } else {
        printf("    ");
      }

      for(int level = 0; level < node->level; level++){
        printf("  ");
      }

      printf("[%d] %.*s:%u",node->level,UN(node->loc.functionName),node->loc.line);
      
      if(node->level == lastLevel  && !Empty(res.val)){
        printf(" Token: '%.*s'",UN(res.val));
      }

      printf("\n");
    }
    lastDebugIndex = list.size;
  }

  return res;
}

Token Parser::PeekToken(int lookahead,ParsingOptions opts){
  const char* saved = this->ptr;
  
  Token res = {};
  for(int i = 0; i < lookahead + 1; i++){
    res = InternalConsumeToken(opts);
  }

  this->ptr = saved;

  return res;
}

void Parser::Advance(Token tok){
  this->ptr = tok.val.data + tok.val.size;
}

bool Parser::IfNextToken(TokenType type){
  Token tok = PeekToken();
  if(tok.type == type){
    NextToken();
    return true;
  }

  return false;
}

bool Parser::IfNextToken(char singleChar){
  Assert(IsCharSingleToken(singleChar));

  return IfNextToken(TOK_TYPE(singleChar));
}

bool Parser::IfPeekToken(TokenType type,int lookahead){
  Token tok = PeekToken(lookahead);
  if(tok.type == type){
    return true;
  }

  return false;
}

bool Parser::IfPeekToken(char singleChar,int lookahead){
  Assert(IsCharSingleToken(singleChar));

  return IfPeekToken(TOK_TYPE(singleChar),lookahead);
}

Token Parser::ExpectNext(TokenType type,ParsingOptions opts){
  Token tok = NextToken();

#if 0
  if(type == TokenType_IDENTIFIER && (opts & ParsingOptions_ERROR_ON_C_VERILOG_KEYWORDS)){
    if(tok.type == TokenType_C_KEYWORD && options & ParsingOptions_ERROR_ON_C_KEYWORDS){
      ReportError("Expected identifier but instead got a C reserved keyword.\n We cannot have C keywords since we will have to generate C code and the generated code will be malformed");
    } else if(tok.type == TokenType_VERILOG_KEYWORD && options & ParsingOptions_ERROR_ON_VERILOG_KEYWORDS){
      ReportError("Expected identifier but instead got a Verilog reserved keyword.\n We cannot have Verilog keywords since we will have to generate Verilog code and the generated code will be malformed");
    }
  } else if(tok.type != type){
#endif

  if(tok.type != type){
    //NOT_IMPLEMENTED("FileContent should just be the file currently parsing, should be optional and stored in the parser");
#if 1
    TEMP_REGION(temp,nullptr);

    FileContent content = {};

    LocInfo loc = PARSE_GetLinesAroundLocation(tok.val.data,content.content,1,1,temp);

    String typeRepr = PushRepr(temp,type);
    String repr = PARSE_PushDebugRepr(temp,tok);
    String error = PushString(temp,"Unexpected token. Expected type: %.*s , Got: %.*s",UN(typeRepr),UN(repr));
    String pointing = PushPointingString(temp,loc.column,tok.val.size);

    auto b = StartString(temp);

    b->PushString("At line %d column %d:\n",loc.line,loc.column);
    b->PushString(error);
    b->PushString("\n");
    b->PushString(loc.lineContent);
    b->PushString("\n");
    b->PushString(pointing);
    b->PushString("\n");

    Array<Location> stackTrace = CollectStackTrace(temp,1);
    stackTrace.size -= 1;

    for(Location l : stackTrace){
      b->PushString("%.*s:%d\n",UN(l.functionName),l.line);
    }

    ReportError(EndString(temp,b));
#endif
  }

  return tok;
}

Token Parser::ExpectNext(char singleChar,ParsingOptions opts){
  Assert(IsCharSingleToken(singleChar));

  return ExpectNext(TOK_TYPE(singleChar),opts);
}

Token Parser::ExpectIdentifier(String expectedContent){
  Token token = NextToken();

  if(token.type == TokenType_IDENTIFIER){
    if(token.val != expectedContent){
      ReportError(SF("Expected %.*s, got instead",UN(expectedContent)));
    }
  } else {
    ReportError("Expected identifier, got instead");
  }

  return token;
}

void Parser::Synch(BracketList<TokenType> possibleTypes){
  while(!Done()){
    Token peek = PeekToken();

    for(TokenType type : possibleTypes){
      if(peek.type == type){
        return;
      }
    }
    
    NextToken();
  }
}

bool Parser::Done(){
  if(PeekToken().type == TokenType_EOF){
    return true;
  }

  return false;
}


// ======================================
// Type

bool PARSE_IsComment(TokenType in){
  bool res = (in == TokenType_COMMENT ||
              in == TokenType_MULTILINE_COMMENT ||
              in == TokenType_UNTERMINATED_MULTILINE_COMMENT);

  return res;
}

String PARSE_GetStringContent(Token stringType){
  String res = stringType.val;
  Assert(stringType.type == TokenType_C_STRING);

  res = Offset(Cut(res,1),1);
  return res;
}

String PARSE_GetCommentContent(Token commentType){
  String res = commentType.val;
  Assert(PARSE_IsComment(commentType.type));

  switch(commentType.type){
    case TokenType_COMMENT:{
      res = Offset(res,2);
    } break;
    case TokenType_MULTILINE_COMMENT:{
      res = Cut(Offset(res,2),2);
    } break;
    case TokenType_UNTERMINATED_MULTILINE_COMMENT:{
      res = Offset(res,2);
    } break;
  }

  return res;
}

// ============================================================================
// Tokenizer function helpers

bool IsWhitespace(char ch,bool includeNewline){
  bool res = (ch == ' ' || 
              ch == '\r' || 
              ch == '\t');
 
  if(includeNewline){
    res |= (ch == '\n');
  }
 
  return res;
}

Token ParseWhitespace(const char* start,const char* end,ParseWhitespaceOptions options){
  const char* ptr = start;

  int lines = 0;
  int column = 0;
  while(ptr < end && IsWhitespace(*ptr,options & ParseWhitespaceOptions_INCLUDE_NEWLINES)){
    if(*ptr == '\n'){
      column += 1;
      lines = 0;
    } else {
      lines += 1;
    }

    ptr += 1;
  }

  Token res = {};
  res.type = (ptr > start ? TokenType_WHITESPACE : TokenType_INVALID);
  res.val = String(start,ptr - start);

  return res;
}

Token ParseNewline(const char* start,const char* end){
  const char* ptr = start;
  char ch = *ptr;

  TokenType type = TokenType_INVALID;
  if(ch == '\n'){
    type = TokenType_NEWLINE;
  }

  Token res = {};
  res.type = type;
  res.val = String(start,type == TokenType_NEWLINE ? 1 : 0);

  return res;
}

Token ParseComments(const char* start,const char* end){
  const char* ptr = start;

  bool unterminated = false;
  bool multiLine = false;
  if(ptr[0] == '/' && ptr[1] == '/'){
    while(ptr < end && (*ptr) != '\n'){
      ptr += 1;
    }
  } else if(ptr[0] == '/' && ptr[1] == '*'){
    multiLine = true;
    ptr += 1;
    int level = 0;
    while(ptr + 1 < end){
      if((*ptr) == '/' && (*(ptr + 1)) == '*'){
        level += 1;
      }
      if((*ptr) == '*' && (*(ptr + 1)) == '/'){
        if(level == 0){
          break;
        }
        level -= 1;
      }

      ptr += 1;
    }

    if(ptr + 1 >= end){
      unterminated = true;
      ptr = end;
    } else {
      ptr += 2;
    }
  }

  String comment = String(start,ptr - start);

  Token res = {};
  res.val = comment;
  
  if(ptr > start){
    if(unterminated){
      res.type = TokenType_UNTERMINATED_MULTILINE_COMMENT;
    } else if(multiLine) {
      res.type = TokenType_MULTILINE_COMMENT;
    } else {
      res.type = TokenType_COMMENT;
    }
  }

  return res;
}

Token ParseSymbols(const char* start,const char* end){
  const char* ptr = start;

  // TODO: We are only parsing single character digits and we might want to parse more characters than this one.
  //       Check how we want to progress when we start using this function more times.

  char ch = *ptr;
  
  TokenType type = TokenType_INVALID;
  if(ch >= TokenType_CHAR_GROUP_0_START && ch <= TokenType_CHAR_GROUP_0_LAST){
    type = TOK_TYPE(ch);
  }
  if(ch >= TokenType_CHAR_GROUP_1_START && ch <= TokenType_CHAR_GROUP_1_LAST){
    type = TOK_TYPE(ch);
  }
  if(ch >= TokenType_CHAR_GROUP_2_START && ch <= TokenType_CHAR_GROUP_2_LAST){
    type = TOK_TYPE(ch);
  }
  if(ch >= TokenType_CHAR_GROUP_3_START && ch <= TokenType_CHAR_GROUP_3_LAST){
    type = TOK_TYPE(ch);
  }

  Token res = {};
  res.val = String(start,(type == TokenType_INVALID ? 0 : 1));
  res.type = type;

  return res;
}

Token ParseNumber(const char* start,const char* end){
  const char* ptr = start;
  
  int number = 0;
  while(ptr < end && (*ptr) >= '0' && (*ptr) <= '9'){
    number *= 10;
    number += ((*ptr) - '0');
    ptr += 1;
  }

  Token res = {};
  res.type = (ptr > start ? TokenType_NUMBER : TokenType_INVALID);
  res.val = String(start,ptr - start);

  return res;
}

Token ParseIdentifier(const char* start,const char* end){
  const char* ptr = start;
  char ch = *ptr;

  if(!IsAlpha(ch)){
    return {};
  }

  ptr += 1;
  for(; ptr < end; ptr += 1){
    if(!IsAlpha(*ptr) && !IsNumeric(*ptr)){
      break;
    } 
  }

  Token res = {};
  res.type = (ptr > start ? TokenType_IDENTIFIER : TokenType_INVALID);
  res.val = String(start,ptr - start);

  return res;
}

Token ParseMultiSymbol(const char* start,const char* end,String format,TokenType result){
  if(start + format.size >= end){
    return {};
  }

  for(int i = 0; i < format.size; i++){
    if(start[i] != format[i]){
    return {};
    }
  }

  Token res = {};
  res.type = result;
  res.val = String(start,format.size);
  return res;
}

#if 0
TokenizeResult ParseFilepath(const char* start,const char* end){
  TokenizeResult res = {};

  const char* ptr = start;
  res.token.originalData.data = start;
  char ch = *ptr;

  if(ch != '.'){
    return {};
  }

  ptr += 1;
  for(; ptr < end; ptr += 1){
    if(!IsAlpha(*ptr) && !IsNumeric(*ptr) && (*ptr != '.') && (*ptr != '/') && (*ptr != '-')){
      break;
    } 
  }
  
  String identifier = {};
  identifier.data = start;
  identifier.size = ptr - start;

  res.bytesParsed = ptr - start;
  res.token.type = TokenType_FILEPATH;  
  res.token.identifier = identifier;

  res.token.originalData.size = res.bytesParsed;
  return res;
}
#endif

Token ParseVerilogPreprocess(const char* start,const char* end){
  const char* ptr = start;
  char ch = *ptr;

  if(ch != '`'){
    return {};
  }

  ptr += 1;
  const char* startIdentifierPart = ptr;

  for(; ptr < end; ptr += 1){
    if(!IsAlpha(*ptr) && !IsNumeric(*ptr)){
      break;
    } 
  }

  String identifier = {};
  identifier.data = startIdentifierPart;
  identifier.size = ptr - startIdentifierPart;

  TokenType type = TokenType_VERILOG_PREPROCESS; 

  if(identifier.size == 0){
    type = TokenType_INVALID;
  }

  if(identifier == "define" )             { type = TokenType_VERILOG_DEFINE; }
  if(identifier == "timescale")           { type = TokenType_VERILOG_TIMESCALE; }
  if(identifier == "undef"  )             { type = TokenType_VERILOG_UNDEF; }
  if(identifier == "include")             { type = TokenType_VERILOG_INCLUDE;}
  if(identifier == "ifdef"  )             { type = TokenType_VERILOG_IFDEF; }
  if(identifier == "ifndef" )             { type = TokenType_VERILOG_IFNDEF; }
  if(identifier == "else"   )             { type = TokenType_VERILOG_ELSE; }
  if(identifier == "elsif"  )             { type = TokenType_VERILOG_ELSIF; }
  if(identifier == "endif"  )             { type = TokenType_VERILOG_ENDIF; }
  if(identifier == "begin_keywords")      { type = TokenType_VERILOG_BEGIN_KEYWORDS; }
  if(identifier == "end_keywords")        { type = TokenType_VERILOG_END_KEYWORDS; }
  if(identifier == "celldefine")          { type = TokenType_VERILOG_CELLDEFINE; }
  if(identifier == "default_nettype")     { type = TokenType_VERILOG_DEFAULT_NETTYPE; }
  if(identifier == "endcelldefine")       { type = TokenType_VERILOG_ENDCELLDEFINE; }
  if(identifier == "line")                { type = TokenType_VERILOG_LINE; }
  if(identifier == "nounconnected_drive") { type = TokenType_VERILOG_NOUNCONNECTED_DRIVE; }
  if(identifier == "pragma")              { type = TokenType_VERILOG_PRAGMA; }
  if(identifier == "resetall")            { type = TokenType_VERILOG_RESETALL; }
  if(identifier == "unconnected_drive")   { type = TokenType_VERILOG_UNCONNECTED_DRIVE; }

  Token res = {};
  res.type = type;
  res.val = String(start,ptr - start);
  return res;
}

Token ParseCString(const char* start,const char* end){
  const char* ptr = start;
  char ch = *ptr;

  if(ch != '\"'){
    return {};
  }

  ptr += 1;

  auto IsHex = [](char ch){
    bool res = (ch >= '0' && ch <= '9');
    res |= (ch == 'a' || ch == 'A');
    res |= (ch == 'b' || ch == 'B');
    res |= (ch == 'c' || ch == 'C');
    res |= (ch == 'd' || ch == 'D');
    res |= (ch == 'e' || ch == 'E');
    res |= (ch == 'f' || ch == 'F');
    
    return res;
  };

  // TODO: Not doing anything with this but we could report as error if we define a token type for unterminated escape sequences (like we do with unterminated comments).
  IGNORE_UNUSED bool unterminatedEscapeSequence = false;

  for(; ptr < end; ){
    const char* loopStart = ptr;

    if(*ptr == '\\'){
      if(ptr + 1 >= end){
        unterminatedEscapeSequence = true;
        break;
      }

      char nextCh = *(ptr + 1);
      String allowedChars = "abefnrtv\\`\"\?";
      for(char form : allowedChars){
        if(nextCh == form){
          ptr += 2;
        }
      }
      
      String octal = "01234567";
      for(char form : octal){
        if(nextCh == form){
          ptr += 4;
        }
      }

      if(nextCh == 'x'){
        ptr += 2;

        while(ptr + 1 < end){
          if(IsHex(*ptr) && IsHex(*(ptr + 1))){
            ptr += 2;
          }
        }
      }

      if(nextCh == 'u'){
        ptr += 6;
      }

      if(nextCh == 'U'){
        ptr += 10;
      }

      if(ptr == loopStart){
        ptr += 1;
        // Found an unrecognized escape sequence, ignoring it.
      }
      
      continue;
    }

    if(*ptr == '\"'){
      ptr += 1;
      break;
    }

    ptr += 1;
  }

  String total = String(start,ptr - start);

  Token res = {};
  res.type = (ptr > start ? TokenType_C_STRING : TokenType_INVALID);
  res.val = String(start,ptr - start);
  return res;
}

bool PARSE_IsCKeyword(String str){

#define CHR(STR) if(STR == str) return true

  // TODO: We really need a fast way of checking this using size + character by character branching path.
  //       However this is something that we want to push to the meta function generation. We do not want to actually write this and potentially get it wrong.
  switch(str.size){
    case 2:{
      CHR("if"); CHR("do");	
    } break;
    case 3:{
      CHR("for"); CHR("int");	
    } break;
    case 4:{
      CHR("auto"); CHR("else"); CHR("long");
      CHR("enum"); CHR("case"); CHR("char");
      CHR("void"); CHR("goto");
    } break;
    case 5:{
      CHR("break"); CHR("union");
      CHR("float"); CHR("short");
      CHR("const"); CHR("while");
    } break;
    case 6:{
      CHR("switch"); CHR("extern");
      CHR("return"); CHR("signed");
      CHR("sizeof"); CHR("static");
      CHR("struct"); CHR("double");
    } break;
    case 7:{
      CHR("typedef"); CHR("default");	
      CHR("_Packed");
    } break;
    case 8:{
      CHR("register"); CHR("unsigned");
      CHR("continue"); CHR("volatile");
    } break;
  }

#undef CHR

   return false;
}

bool PARSE_IsVerilogKeyword(String str){
  // TODO: We really need a fast way of checking this using size + character by character branching path.
  //       However this is something that we want to push to the meta function generation. We do not want to actually write this and potentially get it wrong.
  String keywords[] = {"always","and","assign","automatic","begin","buf","bufif0","bufif1","case","casex","casez","cell","cmos","config","deassign","default","defparam","design","disable","edge","else","end","endcase","endconfig","endfunction","endgenerate","endmodule","endprimitive","endspecify","endtable","endtask","event","for","force","forever","fork","function","generate","genvar","highz0","highz1","if","ifnone","incdir","include","initial","inout","input","instance","integer","join","large","liblist","library","localparam","macromodule","medium","module","nand","negedge","nmos","nor","noshowcancelled","not","notif0","notif1","or","output","parameter","pmos","posedge","primitive","pull0","pull1","pulldown","pullup","pulsestyle_onevent","pulsestyle_ondetect","remos","real","realtime","reg","release","repeat","rnmos","rpmos","rtran","rtranif0","rtranif1","scalared","showcancelled","signed","small","specify","specparam","strong0","strong1","supply0","supply1","table","task","time","tran","tranif0","tranif1","tri","tri0","tri1","triand","trior","trireg","unsigned","use","vectored","wait","wand","weak0","weak1","while","wire","wor","xnor","xor"};
  
  for(String keyword : keywords){
    if(str == keyword){
      return true;
    }
  }

  return false;
}

TokenLocation PARSE_TokenLocation(const char* pos,String content){
  const char* ptr = content.data;
  const char* end = content.data + content.size;
  const char* toFind = pos;

  if(toFind < ptr || toFind >= end){
    return {};
  }
  
  int line = 1;
  int column = 1;
  while(ptr < toFind){
    if(*ptr == '\n'){
      column += 1;
      line = 1;
    } else {
      line += 1;
    }
    ptr += 1;
  }

  TokenLocation loc = {};
  loc.bytePos = ptr - content.data;
  loc.line = line;
  loc.column = column;

  return loc;
}

LocInfo PARSE_GetLinesAroundLocation(const char* pos,String content, int linesBefore, int linesAfter,Arena* out){
  TEMP_REGION(temp,out);

  if(content.size == 0){
    return {};
  }

  linesBefore = Abs(linesBefore);
  linesAfter = Abs(linesAfter);

  const char* contentEnd = content.data + content.size;

  auto list = PushList<String>(temp);

  // NOTE: Slow but should not matter
  int posLine = -1;
  int posColumn = -1;
  const char* ptr = content.data;
  for(int lineIndex = 0; ; lineIndex++){
    const char* start = ptr;
    const char* end = ptr;

    if(start >= contentEnd){
      break;
    }

    while(end < contentEnd){
      if(*end == '\n'){
        break;
      }
      end += 1;
    }

    if(pos >= start && pos <= end){
      posLine = lineIndex;
      posColumn = pos - start - 1;
    }

    String line = String(start,end-start);
    *list->PushElem() = line;

    ptr = end + 1;
  }

  if(posLine == -1){
    return {};
  }

  Array<String> allLines = PushArray(temp,list);
  
  int lineStart = MAX(0,posLine - linesBefore);
  int lineMiddle = posLine - lineStart;
  int lineEnd = MIN(allLines.size,posLine + 1 + linesAfter);
  int linesAmount = lineEnd - lineStart;

  Array<String> returnLines = PushArray<String>(out,linesAmount);

  int index = 0;
  for(int i = lineStart; i < lineEnd; i++){
    returnLines[index++] = allLines[i];
  }

  LocInfo res = {};

  res.linesBefore.data = returnLines.data;
  res.linesBefore.size = lineStart;
  res.lineContent = returnLines[lineMiddle];
  res.linesAfter.data = returnLines.data + lineStart + 1;
  res.linesAfter.size = lineEnd - 1;
  res.allLines = returnLines;
  res.line = posLine + 1;
  res.column = posColumn + 1;

  return res;
}
