#pragma once

#include "utils.hpp"
#include "utilsCore.hpp"

#include "debug.hpp"

#include "parser_meta.hpp"

#define TokenType_START_OF_KEYWORDS   (TokenType_KEYWORD_MODULE)
#define TokenType_END_OF_KEYWORDS     (TokenType_KEYWORD_FOR + 1)

#define TokenType_START_OF_VERILOG_PREPROCESS   (TokenType_VERILOG_DEFINE)
#define TokenType_END_OF_VERILOG_PREPROCESS     (TokenType_VERILOG_PREPROCESS + 1)

#define TokenType_START_OF_VERILOG_KEYWORDS   (TokenType_VERILOG_KEYWORD_MODULE)
#define TokenType_END_OF_VERILOG_KEYWORDS   (TokenType_VERILOG_KEYWORD_WIRE + 1)

#define TOK_TYPE(IN) ((TokenType) IN)

struct TokenLocation{
  int bytePos;
  int line;
  int column;
};

struct Token{
  TokenType type;
  String val;
};

struct TokenNode{
  TokenNode* next;
  Token val;
};

String PARSE_PushDebugRepr(Arena* out,Token token);

inline Token& operator|=(Token& lhs,Token rhs){
  if(lhs.type == TokenType_INVALID){
    lhs = rhs;
  }

  return lhs;
}

struct DefaultTokenizerState{
  FileContent content;
};

typedef Token (*TokenizeFunction)(void* tokenizerState,const char* ptr,const char* end);

#define MAX_STORED_TOKENS 4

enum ParsingOptions{
  ParsingOptions_NONE = 0,

  ParsingOptions_ALLOW_WHITESPACE = (1 << 0),
  ParsingOptions_ALLOW_COMMENTS   = (1 << 1),
  ParsingOptions_ALLOW_NEWLINE    = (1 << 2),

  ParsingOptions_ALLOW_ALL = (ParsingOptions_ALLOW_WHITESPACE | ParsingOptions_ALLOW_COMMENTS | ParsingOptions_ALLOW_NEWLINE),
};
C_STYLE_ENUM(ParsingOptions);

struct Parser{
  void* tokenizerState;
  const char* start;
  const char* ptr;
  const char* end;

  Arena* arena;

  u8 amountStored;
  Token storedTokens[MAX_STORED_TOKENS];

  TokenizeFunction tokenizer;

  ArenaList<String>* errors;

  bool debug;
  int lastDebugIndex;
  LocationNode* debugLocHead;
  LocationNode* debugLocTail;

  //ParsingOptions options;
  const char* currentFile; // Optional, gives better error messages

  // Helpers
  Token InternalConsumeToken(ParsingOptions opts);
  void ReportError(String error);
  
  void ReportUnexpectedToken(Token token,BracketList<TokenType> expectedList);

  Token NextToken(ParsingOptions opts = {});
  Token PeekToken(int lookahead = 0,ParsingOptions opts = {});

  void Advance(Token tok);

  bool IfNextToken(TokenType type);
  bool IfNextToken(char singleChar);

  bool IfPeekToken(TokenType type,int lookahead = 0);
  bool IfPeekToken(char singleChar,int lookahead = 0);
  
  Token ExpectNext(TokenType type,ParsingOptions opts = {});
  Token ExpectNext(char singleChar,ParsingOptions opts = {});

  Token ExpectIdentifier(String expectedContent);

  void Synch(BracketList<TokenType> possibleTypes);

  bool Done();
};

Parser* StartParsing(TokenizeFunction tokenizer,String content,Arena* freeArena,ParsingOptions = {});
Parser* StartParsing(TokenizeFunction tokenizer,void* tokenizerState,String content,Arena* freeArena,ParsingOptions = {});

// ======================================
// Type

bool PARSE_IsComment(TokenType in);

String PARSE_GetStringContent(Token stringType);
String PARSE_GetCommentContent(Token commentType);

// ============================================================================
// Tokenizer function helpers

enum ParseWhitespaceOptions{
  ParseWhitespaceOptions_NONE = 0,
  ParseWhitespaceOptions_INCLUDE_NEWLINES = (1 << 1),

  ParseWhitespaceOptions_DEFAULT = ParseWhitespaceOptions_INCLUDE_NEWLINES
};

bool IsWhitespace(char ch,bool includeNewline);

Token ParseWhitespace(const char* start,const char* end,ParseWhitespaceOptions options = ParseWhitespaceOptions_DEFAULT);
Token ParseNewline(const char* start,const char* end);
Token ParseComments(const char* start,const char* end);
Token ParseSymbols(const char* start,const char* end);
Token ParseNumber(const char* start,const char* end);
Token ParseIdentifier(const char* start,const char* end);
Token ParseMultiSymbol(const char* start,const char* end,String format,TokenType result);

Token ParseVerilogPreprocess(const char* start,const char* end);

Token ParseCString(const char* start,const char* end);

// Since this is only for helper code, we define that all filepaths must begin with an '.'
// This separates them from other normal identifiers 
// TokenizeResult ParseFilepath(const char* start,const char* end);

//TODO: Create a parse remaining so that any other symbol does not cause problems further down the line.

// ======================================
// Check if identifier is a keyword in another language.
// The intent was to prevent Versat from generating an invalid software or hardware file 
// by using a keyword in a place that is not allowed, but so far it might be better to put this logic
// after the parsing is done since there is no guarantee that we generate a keyword from the input.

bool PARSE_IsCKeyword(String identifier);
bool PARSE_IsVerilogKeyword(String identifier);


// ======================================
// Location

struct LocInfo{
  int line;
  int column;

  Array<String> allLines;

  Array<String> linesBefore;
  String lineContent;
  Array<String> linesAfter;
};

LocInfo PARSE_GetLinesAroundLocation(const char* pos,String content, int linesBefore, int linesAfter,Arena* out);
