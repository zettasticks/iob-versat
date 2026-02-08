#pragma once

#include "utils.hpp"

enum NewTokenType : u16{
  NewTokenType_INVALID = 0,
  NewTokenType_EOF,
  NewTokenType_WHITESPACE,
  NewTokenType_COMMENT,
  NewTokenType_UNTERMINATED_MULTILNE_COMMENT,

  // Single characters are equal to their ASCII value.
  NewTokenType_CHAR_GROUP_0_START = '!',
  NewTokenType_CHAR_GROUP_0_LAST = '/',
  
  NewTokenType_CHAR_GROUP_1_START = ':',
  NewTokenType_CHAR_GROUP_1_LAST = '@',

  NewTokenType_CHAR_GROUP_2_START = '[',
  NewTokenType_CHAR_GROUP_2_LAST = '`',

  NewTokenType_CHAR_GROUP_3_START = '{',
  NewTokenType_CHAR_GROUP_3_LAST = '~',

  // Normal types commonly used
  NewTokenType_IDENTIFIER = 128,
  NewTokenType_NUMBER,
  
  // Double digit symbols
  NewTokenType_DOUBLE_DOT,     // ..
  NewTokenType_DOUBLE_HASHTAG, // ##
  NewTokenType_ARROW,          // ->
  NewTokenType_SHIFT_RIGHT,    // >>
  NewTokenType_SHIFT_LEFT,     // <<
  
  // Triple digit symbols
  NewTokenType_ROTATE_RIGHT,   // >><
  NewTokenType_ROTATE_LEFT ,   // ><<
  
  // Keywords
  NewTokenType_KEYWORD_MODULE,
  NewTokenType_KEYWORD_MERGE,
  NewTokenType_KEYWORD_ADDRESSGEN,
  NewTokenType_KEYWORD_USING,
  NewTokenType_KEYWORD_SHARE,
  NewTokenType_KEYWORD_STATIC,
  NewTokenType_KEYWORD_DEBUG,
  NewTokenType_KEYWORD_CONFIG,
  NewTokenType_KEYWORD_STATE,
  NewTokenType_KEYWORD_MEM,

  // We do not really care which keyword it is. We just want to make
  // sure that the generated C code and Verilog code is syntatically
  // correct since the user might use a C/Verilog keyword in place of
  // a name and cause problems later on (Ex: 'const' is a valid name
  // from the POV of Versat but its a keyword in C which causes
  // problems when generating the C structs and so on).
  NewTokenType_C_KEYWORD,
  NewTokenType_VERILOG_KEYWORD
};

#define NewTokenType_CHARACTER_START (NewTokenType_OPEN_PARENTHESIS)
#define NewTokenType_CHARACTER_END   (NewTokenType_DOUBLE_QUOTE + 1)

#define NewTokenType_KEYWORD_START   (NewTokenType_KEYWORD_MODULE)
#define NewTokenType_KEYWORD_END     (NewTokenType_KEYWORD_MEM + 1)

#define TOK_TYPE(IN) ((NewTokenType) IN)

struct NewToken{
  NewTokenType type;

  const char* ptr;

  union{
    String identifier;
    String whitespace;
    String comment;
    i64 number;
  };
};

String PushRepr(Arena* out,NewToken token);

struct TokenizeResult{
  NewToken token;
  u32 bytesParsed;
};

inline TokenizeResult& operator|=(TokenizeResult& lhs,TokenizeResult rhs){
  if(lhs.token.type == NewTokenType_INVALID){
    lhs = rhs;
  }

  return lhs;
}

// start < end
typedef TokenizeResult (*TokenizeFunction)(const char* start,const char* end);

#define MAX_STORED_TOKENS 4

struct ParserMark{
  const char* ptr;
};

enum ParsingOptions{
  ParsingOptions_NONE = 0,

  ParsingOptions_SKIP_WHITESPACE = (1 << 0),
  ParsingOptions_SKIP_COMMENTS   = (1 << 1),

  ParsingOptions_ERROR_ON_C_VERILOG_KEYWORDS = (1 << 2),

  ParsingOptions_DEFAULT = (ParsingOptions_SKIP_WHITESPACE | ParsingOptions_SKIP_COMMENTS)
};

inline ParsingOptions operator|(ParsingOptions lhs,ParsingOptions rhs){
  ParsingOptions res = (ParsingOptions) ((int) lhs | (int) rhs);
  return res;
}

struct Parser{
  const char* start;
  const char* ptr;
  const char* end;
  u32 line;
  u32 column;

  Arena* arena;

  u8 amountStored;
  NewToken storedTokens[MAX_STORED_TOKENS];

  TokenizeFunction tokenizer;

  ArenaList<String>* errors;

  ParsingOptions options;

  // Helpers
  void EnsureTokens(int amount);
  void ReportError(String error);

  void ReportUnexpectedToken(NewToken token,BracketList<NewTokenType> expectedList);

  NewToken NextToken();
  NewToken PeekToken(int lookahead = 0);

  bool IfNextToken(NewTokenType type);
  bool IfNextToken(char singleChar);
  
  NewToken ExpectNext(NewTokenType type);
  NewToken ExpectNext(char singleChar);

  ParserMark Mark();
  String Point(ParserMark mark);

  void Synch(BracketList<NewTokenType> possibleTypes);

  bool Done();
};

Parser* StartParsing(String content,TokenizeFunction tokenizer,Arena* freeArena,ParsingOptions = ParsingOptions_DEFAULT);

// ============================================================================
// Tokenizer function helpers

// TODO: We might add another parameter so that we can pass config parameters into the functions.
TokenizeResult ParseWhitespace(const char* start,const char* end);
TokenizeResult ParseComments(const char* start,const char* end);
TokenizeResult ParseSymbols(const char* start,const char* end);
TokenizeResult ParseNumber(const char* start,const char* end);
TokenizeResult ParseIdentifier(const char* start,const char* end);
TokenizeResult ParseMultiSymbol(const char* start,const char* end,String format,NewTokenType result);

bool Parse_IsCKeyword(String content);
bool Parse_IsVerilogKeyword(String content);

