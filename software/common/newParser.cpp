#include "newParser.hpp"

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

  if(ch >= NewTokenType_CHAR_GROUP_0_START && ch <= NewTokenType_CHAR_GROUP_0_LAST){
    res = true;
  }
  if(ch >= NewTokenType_CHAR_GROUP_1_START && ch <= NewTokenType_CHAR_GROUP_1_LAST){
    res = true;
  }
  if(ch >= NewTokenType_CHAR_GROUP_2_START && ch <= NewTokenType_CHAR_GROUP_2_LAST){
    res = true;
  }
  if(ch >= NewTokenType_CHAR_GROUP_3_START && ch <= NewTokenType_CHAR_GROUP_3_LAST){
    res = true;
  }
  
  return res;
}

String PushRepr(Arena* out,NewTokenType type){
  String res = {};
  if(IsCharSingleToken((char) type)){
    res = PushString(out,"'%c'",(char) type);
  }
  if(type == NewTokenType_WHITESPACE){
    res = "Whitespace";
  }
  if(type == NewTokenType_NEWLINE){
    res = "Newline";
  }
  if(type == NewTokenType_COMMENT){
    res = "Comment";
  }
  if(type == NewTokenType_EOF){
    res = "EOF";
  }
  if(type == NewTokenType_C_KEYWORD){
    res = "C Reserved Keyword";
  }
  if(type == NewTokenType_IDENTIFIER){
    res = "Identifier";
  }
  if(type == NewTokenType_NUMBER){
    res = "Number";
  }

  if(type >= NewTokenType_START_OF_KEYWORDS && type < NewTokenType_END_OF_KEYWORDS){
    return "Keyword";
  }

#define SIMPLE(TYPE,TYPENAME,RET) if(type == TYPE) res = PushString(out,"[%s] '%s'",TYPENAME,RET)
  SIMPLE(NewTokenType_DOUBLE_DOT,"DoubleChar","..");
  SIMPLE(NewTokenType_DOUBLE_HASHTAG,"DoubleChar","##");
  SIMPLE(NewTokenType_ARROW,"DoubleChar","->");
  SIMPLE(NewTokenType_SHIFT_RIGHT,"DoubleChar",">>");
  SIMPLE(NewTokenType_SHIFT_LEFT,"DoubleChar","<<");

  SIMPLE(NewTokenType_ROTATE_RIGHT,"TripleChar",">><");
  SIMPLE(NewTokenType_ROTATE_LEFT,"DoubleChar","><<");

  SIMPLE(NewTokenType_KEYWORD_MODULE,"Keyword","module");
  SIMPLE(NewTokenType_KEYWORD_MERGE,"Keyword","merge");
  SIMPLE(NewTokenType_KEYWORD_SHARE,"Keyword","share");
  SIMPLE(NewTokenType_KEYWORD_STATIC,"Keyword","static");
  SIMPLE(NewTokenType_KEYWORD_DEBUG,"Keyword","debug");
  SIMPLE(NewTokenType_KEYWORD_CONFIG,"Keyword","config");
  SIMPLE(NewTokenType_KEYWORD_STATE,"Keyword","state");
  SIMPLE(NewTokenType_KEYWORD_MEM,"Keyword","mem");
  SIMPLE(NewTokenType_KEYWORD_FOR,"Keyword","for");
#undef SIMPLE 

  return res;
}

String PARSE_PushDebugRepr(Arena* out,NewToken token){
  String res = {};

  if(token.type == NewTokenType_IDENTIFIER){
    res = PushString(out,"[Identifier] '%.*s'",UN(token.identifier));
  }
  if(token.type == NewTokenType_NUMBER){
    res = PushString(out,"[Number] '%ld'",token.number);
  }
  if(token.type == NewTokenType_C_STRING){
    PushString(out,"\"%.*s\"",UN(token.cString));
  }

  if(Empty(res)){
    res = PushRepr(out,token.type);
  }

#define SIMPLE(TYPE,TYPENAME,RET) if(token.type == TYPE) res = PushString(out,"[%s] '%s'",TYPENAME,RET)
  SIMPLE(NewTokenType_KEYWORD_MODULE,"Keyword","module");
  SIMPLE(NewTokenType_KEYWORD_MERGE,"Keyword","merge");
  SIMPLE(NewTokenType_KEYWORD_SHARE,"Keyword","share");
  SIMPLE(NewTokenType_KEYWORD_STATIC,"Keyword","static");
  SIMPLE(NewTokenType_KEYWORD_DEBUG,"Keyword","debug");
  SIMPLE(NewTokenType_KEYWORD_CONFIG,"Keyword","config");
  SIMPLE(NewTokenType_KEYWORD_STATE,"Keyword","state");
  SIMPLE(NewTokenType_KEYWORD_MEM,"Keyword","mem");
  SIMPLE(NewTokenType_KEYWORD_FOR,"Keyword","for");

  SIMPLE(NewTokenType_VERILOG_DEFINE,"Verilog","define");
  SIMPLE(NewTokenType_VERILOG_UNDEF,"Verilog","undef");
  SIMPLE(NewTokenType_VERILOG_INCLUDE,"Verilog","include");

  SIMPLE(NewTokenType_VERILOG_PREPROCESS,"Verilog","preprocess");

  SIMPLE(NewTokenType_VERILOG_IFDEF,"Verilog","ifdef");
  SIMPLE(NewTokenType_VERILOG_IFNDEF,"Verilog","ifndef");
  SIMPLE(NewTokenType_VERILOG_ELSE,"Verilog","else");
  SIMPLE(NewTokenType_VERILOG_ELSIF,"Verilog","elsif");
  SIMPLE(NewTokenType_VERILOG_ENDIF,"Verilog","endif");
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
  tokenizerState->line = 1;
  tokenizerState->column = 1;
  tokenizerState->start = content.data;
  tokenizerState->ptr = content.data;
  tokenizerState->end = content.data + content.size;

  res->tokenizerState = (void*) tokenizerState;
  res->tokenizer = tokenizer;
  res->arena = freeArena;
  res->options = options;
  
  return res;
}

Parser* StartParsing(TokenizeFunction tokenizer,void* tokenizerState,Arena* freeArena,ParsingOptions options){
  Parser* res = PushStruct<Parser>(freeArena);

  res->tokenizerState = (void*) tokenizerState;
  res->tokenizer = tokenizer;
  res->arena = freeArena;
  res->options = options;
  
  return res;

}

void Parser::EnsureTokens(int amount){
  while(this->amountStored < amount){
    NewToken token = this->tokenizer(this->tokenizerState);
    if(options & ParsingOptions_SKIP_WHITESPACE && token.type == NewTokenType_WHITESPACE){
      continue;
    }
    if(options & ParsingOptions_SKIP_WHITESPACE && token.type == NewTokenType_NEWLINE){
      continue;
    }
    if(options & ParsingOptions_SKIP_COMMENTS && token.type == NewTokenType_COMMENT){
      continue;
    }
    if(options & ParsingOptions_SKIP_COMMENTS && token.type == NewTokenType_UNTERMINATED_MULTILINE_COMMENT){
      // TODO: Improve error messages, We can show user the start of the multiline comment
      ReportError("Unterminated multiline comment");
      continue;
    }

    if(token.type == NewTokenType_INVALID){
      if(currentFile){
        printf("Invalid token: %s\n",currentFile);
      }
      token.type = NewTokenType_EOF;
    }

    this->storedTokens[this->amountStored++] = token;
  } 
}

void Parser::ReportError(String error){
  if(!errors){
    errors = PushList<String>(this->arena);
  }
  
  *errors->PushElem() = PushString(arena,error);
}

ParsingOptions Parser::SetOptions(ParsingOptions options){
  ParsingOptions old = this->options;
  this->options = options;
  return old;
}

void Parser::ReportUnexpectedToken(NewToken token,BracketList<NewTokenType> expectedList){
  TEMP_REGION(temp,nullptr);

  String tokenRepr = PARSE_PushDebugRepr(temp,token);

  auto builder = StartString(temp);
  builder->PushString("Unexpected token: %.*s\n",UN(tokenRepr));
  builder->PushString("Expected one of the following:\n");
  
  // TODO: Move tokenType to meta and proper data modelling of the token type properties.
  for(NewTokenType type : expectedList){
    String repr = PushRepr(temp,type);
    builder->PushString("  '%.*s'\n",UN(repr));
  }

  if(!errors){
    errors = PushList<String>(this->arena);
  }

  *errors->PushElem() = EndString(this->arena,builder);
}

NewToken Parser::NextToken(){
  EnsureTokens(1);

  NewToken res = this->storedTokens[0];
  for(int i = 0; i < this->amountStored - 1; i++){
    this->storedTokens[i] = this->storedTokens[i+1];
  }
  this->amountStored -= 1;

  return res;
}

NewToken Parser::PeekToken(int lookahead){
  EnsureTokens(lookahead + 1);

  return this->storedTokens[lookahead];
}

bool Parser::IfNextToken(NewTokenType type){
  NewToken tok = PeekToken();
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

bool Parser::IfPeekToken(NewTokenType type){
  NewToken tok = PeekToken();
  if(tok.type == type){
    return true;
  }

  return false;
}

bool Parser::IfPeekToken(char singleChar){
  Assert(IsCharSingleToken(singleChar));

  return IfPeekToken(TOK_TYPE(singleChar));
}

NewToken Parser::ExpectNext(NewTokenType type){
  NewToken tok = NextToken();

  if(type == NewTokenType_IDENTIFIER && (options & ParsingOptions_ERROR_ON_C_VERILOG_KEYWORDS)){
    if(tok.type == NewTokenType_C_KEYWORD && options & ParsingOptions_ERROR_ON_C_KEYWORDS){
      ReportError("Expected identifier but instead got a C reserved keyword.\n We cannot have C keywords since we will have to generate C code and the generated code will be malformed");
    } else if(tok.type == NewTokenType_VERILOG_KEYWORD && options & ParsingOptions_ERROR_ON_VERILOG_KEYWORDS){
      ReportError("Expected identifier but instead got a Verilog reserved keyword.\n We cannot have Verilog keywords since we will have to generate Verilog code and the generated code will be malformed");
    }
  } else if(tok.type != type){
    TEMP_REGION(temp,nullptr);

    String typeRepr = PushRepr(temp,type);
    String repr = PARSE_PushDebugRepr(temp,tok);
    String error = PushString(temp,"Unexpected token. Expected type: %.*s , Got: %.*s",UN(typeRepr),UN(repr));
    ReportError(error);
  }

  return tok;
}

NewToken Parser::ExpectNext(char singleChar){
  Assert(IsCharSingleToken(singleChar));

  return ExpectNext(TOK_TYPE(singleChar));
}

void Parser::Synch(BracketList<NewTokenType> possibleTypes){
  while(!Done()){
    NewToken peek = PeekToken();

    for(NewTokenType type : possibleTypes){
      if(peek.type == type){
        return;
      }
    }
    
    NextToken();
  }
}

bool Parser::Done(){
  if(PeekToken().type == NewTokenType_EOF){
    return true;
  }

  return false;
}

// ============================================================================
// Tokenizer function helpers

TokenizeResult ParseWhitespace(const char* start,const char* end,ParseWhitespaceOptions options){
  TokenizeResult res = {};

  const char* ptr = start;
  res.token.originalData.data = start;
  
  auto IsWhitespace = [options](char ch){
    bool res = (ch == ' ' || 
                ch == '\r' || 
                ch == '\t');

    if(options & ParseWhitespaceOptions_INCLUDE_NEWLINES){
      res |= (ch == '\n');
    }
    return res;
  };

  while(ptr < end && IsWhitespace(*ptr)){
    ptr += 1;
  }
  
  String allWhitespace = {};
  allWhitespace.data = start;
  allWhitespace.size = ptr - start;

  res.bytesParsed = allWhitespace.size;

  if(res.bytesParsed > 0){
    res.token.type = NewTokenType_WHITESPACE;
    res.token.whitespace = allWhitespace;
  }

  return res;
}

TokenizeResult ParseNewline(const char* start,const char* end){
  TokenizeResult res = {};

  const char* ptr = start;
  res.token.originalData.data = start;
  char ch = *ptr;

  NewTokenType type = NewTokenType_INVALID;
  if(ch == '\n'){
    type = NewTokenType_NEWLINE;
  }

  res.bytesParsed = (type == NewTokenType_INVALID ? 0 : 1);
  res.token.type = type;
  return res;
}

TokenizeResult ParseComments(const char* start,const char* end){
  TokenizeResult res = {};

  const char* ptr = start;
  res.token.originalData.data = start;

  if(ptr + 1 >= end){
    return res;
  }

  bool unterminated = false;
  if(ptr[0] == '/' && ptr[1] == '/'){
    while(ptr < end && (*ptr) != '\n'){
      ptr += 1;
    }
  } else if(ptr[0] == '/' && ptr[1] == '*'){
    while(ptr + 1 < end && ((*ptr) != '*' || (*(ptr + 1)) != '/')){
      ptr += 1;
    }

    if(ptr + 1 >= end){
      unterminated = true;
      ptr = end;
    } else {
      ptr += 2;
    }
  }

  String comment = {};
  comment.data = start;
  comment.size = ptr - start;

  res.bytesParsed = ptr - start;
  
  if(res.bytesParsed > 0){
    if(unterminated){
      res.token.type = NewTokenType_UNTERMINATED_MULTILINE_COMMENT;
    } else {
      res.token.type = NewTokenType_COMMENT;
    }
    
    res.token.comment = comment;
  }  

  return res;
}

TokenizeResult ParseSymbols(const char* start,const char* end){
  TokenizeResult res = {};

  const char* ptr = start;
  res.token.originalData.data = start;
  char ch = *ptr;

  // TODO: We are only parsing single character digits and we might want to parse more characters than this one.
  //       Check how we want to progress when we start using this function more times.
  
  NewTokenType type = NewTokenType_INVALID;

  if(ch >= NewTokenType_CHAR_GROUP_0_START && ch <= NewTokenType_CHAR_GROUP_0_LAST){
    type = TOK_TYPE(ch);
  }
  if(ch >= NewTokenType_CHAR_GROUP_1_START && ch <= NewTokenType_CHAR_GROUP_1_LAST){
    type = TOK_TYPE(ch);
  }
  if(ch >= NewTokenType_CHAR_GROUP_2_START && ch <= NewTokenType_CHAR_GROUP_2_LAST){
    type = TOK_TYPE(ch);
  }
  if(ch >= NewTokenType_CHAR_GROUP_3_START && ch <= NewTokenType_CHAR_GROUP_3_LAST){
    type = TOK_TYPE(ch);
  }

  res.bytesParsed = (type == NewTokenType_INVALID ? 0 : 1);
  res.token.type = type;
  
  return res;
}

TokenizeResult ParseNumber(const char* start,const char* end){
  TokenizeResult res = {};

  const char* ptr = start;
  res.token.originalData.data = start;
  
  int number = 0;
  while(ptr < end && (*ptr) >= '0' && (*ptr) <= '9'){
    number *= 10;
    number += ((*ptr) - '0');
    ptr += 1;
  }

  res.bytesParsed = ptr - start;
  
  if(res.bytesParsed > 0){
    res.token.type = NewTokenType_NUMBER;
    res.token.number = number;
  }  

  return res;
}

TokenizeResult ParseIdentifier(const char* start,const char* end){
  TokenizeResult res = {};

  const char* ptr = start;
  res.token.originalData.data = start;
  char ch = *ptr;

  if(!IsAlpha(ch)){
    return res;
  }

  ptr += 1;
  for(; ptr < end; ptr += 1){
    if(!IsAlpha(*ptr) && !IsNumeric(*ptr)){
      break;
    } 
  }

  String identifier = {};
  identifier.data = start;
  identifier.size = ptr - start;

  res.bytesParsed = ptr - start;
  res.token.type = NewTokenType_IDENTIFIER;  
  res.token.identifier = identifier;

  return res;
}

TokenizeResult ParseMultiSymbol(const char* start,const char* end,String format,NewTokenType result){
  TokenizeResult res = {};

  res.token.originalData.data = start;

  if(start + format.size >= end){
    return res;
  }

  for(int i = 0; i < format.size; i++){
    if(start[i] != format[i]){
      return res;
    }
  }

  res.bytesParsed = format.size;
  res.token.type = result;  

  return res;
}

TokenizeResult ParseVerilogPreprocess(const char* start,const char* end){
  TokenizeResult res = {};

  const char* ptr = start;
  res.token.originalData.data = start;
  char ch = *ptr;

  if(ch != '`'){
    return res;
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

  NewTokenType type = NewTokenType_VERILOG_PREPROCESS; 

  if(identifier.size == 0){
    type = NewTokenType_INVALID;
  }

  if(identifier == "define" ){ type = NewTokenType_VERILOG_DEFINE; }
  if(identifier == "undef"  ){ type = NewTokenType_VERILOG_UNDEF; }
  if(identifier == "include"){ type = NewTokenType_VERILOG_INCLUDE;}
  if(identifier == "ifdef"  ){ type = NewTokenType_VERILOG_IFDEF; }
  if(identifier == "ifndef" ){ type = NewTokenType_VERILOG_IFNDEF; }
  if(identifier == "else"   ){ type = NewTokenType_VERILOG_ELSE; }
  if(identifier == "elsif"  ){ type = NewTokenType_VERILOG_ELSIF; }
  if(identifier == "endif"  ){ type = NewTokenType_VERILOG_ENDIF; }

  res.token.type = type;
  res.bytesParsed = ptr - start;
  res.token.identifier = identifier;

  return res;
}

TokenizeResult ParseCString(const char* start,const char* end){
  TokenizeResult res = {};

  const char* ptr = start;
  res.token.originalData.data = start;
  char ch = *ptr;

  if(ch != '\"'){
    return res;
  }

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
  bool unterminatedEscapeSequence = false;
  bool foundString = false;
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
      foundString = true;
    }

    ptr += 1;
  }

  String total = String(start,ptr - start);

  // Remove the first '"' and the last '"' from the content
  String cStringContent = Offset(total,1);
  cStringContent.size -= 1;
  
  res.token.type = NewTokenType_C_STRING;
  res.bytesParsed = ptr - start;
  res.token.identifier = cStringContent;

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
