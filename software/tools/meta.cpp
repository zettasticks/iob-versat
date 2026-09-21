#include "meta.hpp"

bool Done(Tokenizer* tok,TokenOptions opts){
  Token token = PeekToken(tok,0,opts);
  if(token.type == TokenType_EOF){
    return true;
  }
  return false;
}

Token Combine(Token left,Token right){
  if(left.type == TokenType_NIL){
    return right;
  }

  // TODO: Assuming that right is immediatly right after left.
  Token res = {};
  res.type = TokenType_MISC;
  res.id = left.id;
  res.id.size = (right.id.data - left.id.data + right.id.size);

  return res;
}

Token ConsumeToken(Tokenizer* tok){
  TokenType type = TokenType_NIL;

  const char* savedPtr = tok->ptr;

  if(tok->ptr >= tok->end || tok->ptr == nullptr){
    type = TokenType_EOF;
  }

  char ch = 0;
  char ch2 = 0;
  int sizeOffset = 0;

  if(tok->ptr < tok->end){
    ch = tok->ptr[0];
  }
  if(tok->ptr && tok->ptr + 1 < tok->end){
    ch2 = tok->ptr[1];
  }

  // Helpers ====================================================================
  auto Found = [&type](){return type != TokenType_NIL;};

  auto IsWhitespace = [](char ch){
    bool res = (ch == '\t' || ch == '\r' || ch == ' ');
    return res;
  };

  auto IsAlpha = [](char ch){
    bool res = ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_');
    return res;
  };

  auto IsDecimalDigit = [](char ch){
    bool res = (ch >= '0' && ch <= '9');
    return res;
  };

  // Start with common ==========================================================

  if(!Found() && ch == '\n'){
    type = TokenType_NEWLINE;
    tok->ptr += 1;
  }

  if(!Found() && IsWhitespace(ch)){
    while(tok->ptr < tok->end && IsWhitespace(*tok->ptr)){
      tok->ptr += 1;
    }
    
    type = TokenType_WHITESPACE;
  }

  if(!Found() && ch == '/' && ch2 == '/'){
    while(tok->ptr < tok->end && tok->ptr[0] != '\n'){
      tok->ptr += 1;
    }

    // TODO: Do we also store the newline or not?

    type = TokenType_SINGLE_LINE_COMMENT;
  }

  if(!Found() && ch == '/' && ch2 == '*'){
    i32 level = 0;
    tok->ptr += 2;

    while(tok->ptr + 1 < tok->end){
      if(tok->ptr[0] == '/' && tok->ptr[1] == '*'){
        level += 1;
      }
      if(tok->ptr[0] == '*' && tok->ptr[1] == '/'){
        if(level == 0){
          break;
        }

        level -= 1;
      }
      
      tok->ptr += 1;
    }

    if(tok->ptr + 1 < tok->end && tok->ptr[0] == '*' && tok->ptr[1] == '/' && level == 0){
      tok->ptr += 2;
      type = TokenType_MULTI_LINE_COMMENT;
    } else {
      type = TokenType_UNTERMINATED_MULTI_LINE_COMMENT;
    }
  }

  // Strings ====================================================================

  // NOTE: ' and " are passed directly. 
  if(!Found() && ch == '\''){
    tok->ptr += 1;

    while(tok->ptr < tok->end && *tok->ptr != '\''){
      tok->ptr += 1;
    }
    tok->ptr += 1;

    type = TokenType_CONTENT;
  }
  if(!Found() && ch == '"'){
    tok->ptr += 1;

    while(tok->ptr < tok->end && *tok->ptr != '"'){
      tok->ptr += 1;
    }
    tok->ptr += 1;

    type = TokenType_CONTENT;
  }

  // NOTE: A $ string acts like a proper string type and the $ are removed from consideration by the rest of the code.
  //       $ ABC $ is treated as ABC by the rest of the code, with the $ removed.
  //       Multiple $ can be used to change the start and end terminators. A $$ start only matches a $$ end.
  // NOTE: If needed add '$$$' and so on, altough two levels should be enough for anyone.
  if(!Found() && ch == '$' && ch2 == '$'){
    tok->ptr += 2;
    savedPtr = tok->ptr;

    while(tok->ptr + 1 < tok->end && *tok->ptr != '$' && *(tok->ptr + 1) != '$'){
      tok->ptr += 1;
    }
    tok->ptr += 2;

    sizeOffset = -2;
    type = TokenType_CONTENT;
  }

  if(!Found() && ch == '$'){
    tok->ptr += 1;
    savedPtr = tok->ptr;

    while(tok->ptr < tok->end && *tok->ptr != '$'){
      tok->ptr += 1;
    }
    tok->ptr += 1;

    sizeOffset = -1;
    type = TokenType_CONTENT;
  }

  if(!Found() && ch == '-' && ch2 == '>'){
    tok->ptr += 2;

    type = TokenType_ARROW;
  }

  if(!Found() && ch == '=' && ch2 == '='){
    tok->ptr += 2;

    type = TokenType_EQ;
  }
  if(!Found() && ch == '!' && ch2 == '='){
    tok->ptr += 2;

    type = TokenType_NEQ;
  }

  // Single chars symbols =======================================================
  if(!Found()){
    if(ch >= TokenType_CHAR_GROUP_0_START &&
       ch <= TokenType_CHAR_GROUP_0_LAST){
      type = TOK_TYPE(ch);
    }
    if(ch >= TokenType_CHAR_GROUP_1_START &&
       ch <= TokenType_CHAR_GROUP_1_LAST){
      type = TOK_TYPE(ch);
    }
    if(ch >= TokenType_CHAR_GROUP_2_START &&
       ch <= TokenType_CHAR_GROUP_2_LAST){
      type = TOK_TYPE(ch);
    }
    if(ch >= TokenType_CHAR_GROUP_3_START &&
       ch <= TokenType_CHAR_GROUP_3_LAST){
      type = TOK_TYPE(ch);
    }

    if(Found()){
      tok->ptr += 1;
    }
  }
  
  // NOTE: We do not parse numbers, we only do textual things currently.
  if(!Found() && (IsAlpha(ch) || IsDecimalDigit(*tok->ptr))){
    tok->ptr += 1;
    while(tok->ptr < tok->end && (IsAlpha(*tok->ptr) || IsDecimalDigit(*tok->ptr))){
      tok->ptr += 1;
    }
    
    int identifierSize = (tok->ptr - savedPtr);
    String id = String(savedPtr,identifierSize);

#define TOK_ID(NAME,TYPE) if(NAME == id){type = TokenType_KEYWORD_ ## TYPE;}

    TOK_ID("table",TABLE);
    TOK_ID("array",ARRAY);
    TOK_ID("enum",ENUM);
    TOK_ID("map",MAP);
    TOK_ID("func",FUNC);
    TOK_ID("for",FOR);
    TOK_ID("struct",STRUCT);
    TOK_ID("file",FILE);

#undef TOK_ID

    if(!Found()){
      type = TokenType_CONTENT;
    }
  }

  Token res = {};
  res.type = type;
  res.id.data = savedPtr;
  res.id.size = (tok->ptr - savedPtr);
  res.id.size += sizeOffset;
  
  return res;
}

Token ConsumeTokenInternal(Tokenizer* tok){
  TokenType type = TokenType_NIL;

  const char* savedPtr = tok->ptr;

  if(tok->ptr >= tok->end || tok->ptr == nullptr){
    type = TokenType_EOF;
  }

  char ch = 0;
  char ch2 = 0;

  if(tok->ptr < tok->end){
    ch = tok->ptr[0];
  }
  if(tok->ptr && tok->ptr + 1 < tok->end){
    ch2 = tok->ptr[1];
  }

  // Helpers ====================================================================
  auto Found = [&type](){return type != TokenType_NIL;};

  auto IsWhitespace = [](char ch){
    bool res = (ch == '\t' || ch == '\r' || ch == ' ');
    return res;
  };

  auto IsAlpha = [](char ch){
    bool res = ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_');
    return res;
  };

  auto IsDecimalDigit = [](char ch){
    bool res = (ch >= '0' && ch <= '9');
    return res;
  };

  // Symbols ====================================================================

  auto IsSymbol = [](char ch, char ch2) -> i32{
    if(ch == '('){
      return 1;
    }
    if(ch == ')'){
      return 1;
    }
    if(ch == '.'){
      return 1;
    }
    if(ch == '@'){
      return 1;
    }

    return 0;
  };

  i32 symbolOffset = IsSymbol(ch,ch2);
  if(!Found() && symbolOffset > 0){
    tok->ptr += symbolOffset;

    type = TOK_TYPE(ch);
  }

  if(!Found()){
    tok->ptr += 1;
    while(1){
      char ch = tok->ptr < tok->end ? *tok->ptr : 0;
      char ch2 = tok->ptr + 1 < tok->end ? *(tok->ptr + 1) : 0;

      if(ch == 0){
        break;
      }

      if(IsSymbol(ch,ch2)){
        break;
      }

      tok->ptr += 1;
    }
    
    int identifierSize = (tok->ptr - savedPtr);
    String id = String(savedPtr,identifierSize);
    type = TokenType_CONTENT;
  }

  Token res = {};
  res.type = type;
  res.id.data = savedPtr;
  res.id.size = (tok->ptr - savedPtr);
  
  return res;
}


Token NextToken(Tokenizer* tok,TokenOptions opt){
  Token toReturn = InternalNextToken(tok,opt);

  return toReturn;
}

Token AssertToken(Tokenizer* tok,TokenType type,TokenOptions opts){
  Token token = NextToken(tok,opts);
  Assert(token.type == type);
  return token;
}

Token InternalNextToken(Tokenizer* tok,TokenOptions opts){
  Token toReturn = tok->func(tok);

  auto Skip = [opts](TokenType t) -> b32{
    b32 skip = 0;

    if(opts.allowWhitespace == 0){
      if(t == TokenType_WHITESPACE) skip = 1;
    }

    if(opts.allowComment == 0){
      if(t == TokenType_SINGLE_LINE_COMMENT) skip = 1;
      if(t == TokenType_MULTI_LINE_COMMENT) skip = 1;
    }

    if(opts.allowNewline == 0){
      if(t == TokenType_NEWLINE) skip = 1;
    }

    return skip;
  };

  while(Skip(toReturn.type)){
    toReturn = tok->func(tok);
  }

  return toReturn;
}

Token PeekToken(Tokenizer* tok,int lookahead,TokenOptions opts){
  const char* saved = tok->ptr;

  Token toReturn = {};
  for(int i = 0; i < lookahead + 1; i++){
    toReturn = InternalNextToken(tok,opts);
  }

  tok->ptr = saved;

  return toReturn;
}

bool IfNextToken(Tokenizer* tok,TokenType type,TokenOptions opts){
  Token p = PeekToken(tok,0,opts);

  if(p.type == type){
    NextToken(tok,opts);
    return true;
  }
  
  return false;
}

bool IfPeekToken(Tokenizer* tok,TokenType type,TokenOptions opts){
  Token p = PeekToken(tok,0,opts);

  if(p.type == type){
    return true;
  }
  
  return false;
}

Node* ParseModifier(Tokenizer* tok,Arena* out){
  Node* head = 0;
  Node* ptr = 0;

  AssertToken(tok,TOK_TYPE('@'));
  AssertToken(tok,TOK_TYPE('('));
  while(!Done(tok)){
    if(IfPeekToken(tok,TOK_TYPE(')'))){
      break;
    }

    Token value = NextToken(tok);
    Node* newNode = PushStruct<Node>(out);
    newNode->type = NodeType_VALUE;
    newNode->token = value;
    
    LL_Append(head,ptr,next,newNode);
  }

  AssertToken(tok,TOK_TYPE(')'));

  Node* res = PushStruct<Node>(out);
  res->type = NodeType_MODIFIER;
  res->childs = head;

  return res;
}

Node* ParseValue(Tokenizer* tok,Arena* out){
  Node* res = 0;

  if(!res && IfNextToken(tok,TOK_TYPE('@'))){
    AssertToken(tok,TOK_TYPE('('));

    Node* modHead = 0;
    Node* modPtr = 0;

    while(!Done(tok)){
      if(IfPeekToken(tok,TOK_TYPE(')'))){
        break;
      }

      Node* val = MakeNode(out,NodeType_VALUE,NextToken(tok));
      LL_Append(modHead,modPtr,next,val);
    }
    
    AssertToken(tok,TOK_TYPE(')'));

    Node* contentInside = ParseValue(tok,out);

    res = MakeModifierNode(out,modHead,contentInside);
  }

  if(!res){
    res = MakeNode(out,NodeType_VALUE,NextToken(tok));
  }
  
  return res;
}

Node* ParseLine(Tokenizer* tok,Arena* out){
  Node* loopNode = 0;
  Node* lastLoopChild = 0;
  if(IfNextToken(tok,TOK_TYPE('@'))){
    AssertToken(tok,TOK_TYPE('('));
          
    AssertToken(tok,TokenType_KEYWORD_FOR);
    Token iter = AssertToken(tok,TokenType_CONTENT);
    Token iterating = AssertToken(tok,TokenType_CONTENT);
    AssertToken(tok,TOK_TYPE(')'));

    Node* iterName = PushStruct<Node>(out);
    iterName->type = NodeType_VALUE;
    iterName->token = iter;

    Node* iteratingName = PushStruct<Node>(out);
    iteratingName->type = NodeType_VALUE;
    iteratingName->token = iterating;

    iterName->next = iteratingName;
    lastLoopChild = iteratingName;

    loopNode = PushStruct<Node>(out);
    loopNode->type = NodeType_FOR_LOOP;
    loopNode->childs = iterName;
  }

  Token fullLine = {};
  while(!Done(tok)){
    Token token = NextToken(tok,{.allowNewline = 1,.allowWhitespace = 1});

    if(token.type == TokenType_NEWLINE){
      break;
    }

    fullLine = Combine(fullLine,token);
  }

  Node* line = PushStruct<Node>(out);
  line->type = NodeType_LINE;
  line->token = fullLine;
        
  Node* newNode = line;
  if(loopNode){
    lastLoopChild->next = line;
    newNode = loopNode;
  }

  return newNode;
}

Token ParseMetaTypeExpression(Tokenizer* tok){
  Token res = AssertToken(tok,TokenType_CONTENT);

  if(IfNextToken(tok,TOK_TYPE('['))){
    Token arraySize = NextToken(tok);

    Token end = AssertToken(tok,TOK_TYPE(']'));
    res = Combine(res,end);
  }

  return res;
}

Node* Parse(Tokenizer* tok,Arena* out){
  Node* head = nullptr;
  Node* ptr = nullptr;
  while(!Done(tok)){
    Node* node = nullptr;

    if(!node && IfNextToken(tok,TokenType_KEYWORD_TABLE)){
      Node* tableHead = nullptr;
      Node* tablePtr = nullptr;
      
      Token name = AssertToken(tok,TokenType_CONTENT);
      AssertToken(tok,TOK_TYPE('('));

      Node* paramHead = nullptr;
      Node* paramPtr = nullptr;
      while(!Done(tok)){
        if(IfPeekToken(tok,TOK_TYPE(')'))){
          break;
        }

        Token id = AssertToken(tok,TokenType_CONTENT);
        Token typeExpression = {};

        if(IfNextToken(tok,TOK_TYPE(':'))){
          typeExpression = ParseMetaTypeExpression(tok);
        }

        Node* param = PushStruct<Node>(out);
        param->token = id;
        param->type = NodeType_DECL_NAME;

        if(!IsNil(typeExpression.type)){
          Node* type = PushStruct<Node>(out);
          type->token = typeExpression;
          type->type = NodeType_DECL_TYPE;
          type->childs = param;
          param = type;
        }

        LL_Append(paramHead,paramPtr,next,param);

        IfNextToken(tok,TOK_TYPE(','));
      }
      AssertToken(tok,TOK_TYPE(')'));

      AssertToken(tok,TOK_TYPE('{'));

      Node* contentHead = nullptr;
      Node* contentPtr = nullptr;
      while(!Done(tok)){
        if(IfPeekToken(tok,TOK_TYPE('}'))){
          break;
        }        
        
        Node* value = PushStruct<Node>(out);
        value->type = NodeType_VALUE;
        value->token = NextToken(tok);

#if 1
        if(value->token.type == TokenType_STRING){
          value->token.id.data += 1;
          value->token.id.size -= 2;
          value->token.id = TrimWhitespaces(value->token.id);
        }
#endif

        LL_Append(contentHead,contentPtr,next,value);
      }
      AssertToken(tok,TOK_TYPE('}'));

      Node* parameters = PushStruct<Node>(out);
      parameters->type = NodeType_LIST;
      parameters->childs = paramHead;

      Node* content = PushStruct<Node>(out);
      content->type = NodeType_CONTENT;
      content->childs = contentHead;
      
      LL_Append(tableHead,tablePtr,next,parameters);
      LL_Append(tableHead,tablePtr,next,content);

      node = PushStruct<Node>(out);
      node->type = NodeType_TABLE;
      node->token = name;
      node->childs = tableHead;
    }

    if(!node && IfNextToken(tok,TokenType_KEYWORD_ENUM)){
      Token name = AssertToken(tok,TokenType_CONTENT);
      
      AssertToken(tok,TOK_TYPE('{'));

      Node* enumHead = nullptr;
      Node* enumPtr = nullptr;
      while(!Done(tok)){
        if(IfPeekToken(tok,TOK_TYPE('}'))){
          break;
        }

        Node* value = ParseValue(tok,out);
        LL_Append(enumHead,enumPtr,next,value);
      }

      AssertToken(tok,TOK_TYPE('}'));

      node = MakeNode(out,NodeType_ENUM,name,enumHead);
    }

    if(!node && IfNextToken(tok,TokenType_KEYWORD_FUNC)){
      Token type = AssertToken(tok,TokenType_CONTENT);
      Token name = AssertToken(tok,TokenType_CONTENT);
      AssertToken(tok,TOK_TYPE('('));

      Node* paramHead = nullptr;
      Node* paramPtr = nullptr;
      while(!Done(tok)){
        Token type = AssertToken(tok,TokenType_CONTENT);
        Token id = AssertToken(tok,TokenType_CONTENT);
        
        Node* paramType = PushStruct<Node>(out);
        paramType->token = type;
        paramType->type = NodeType_DECL_TYPE;

        Node* paramName = PushStruct<Node>(out);
        paramName->token = id;
        paramName->type = NodeType_DECL_NAME;

        paramType->next = paramName;

        Node* param = PushStruct<Node>(out);
        param->type = NodeType_PARAMETER;
        param->childs = paramType;

        LL_Append(paramHead,paramPtr,next,param);

        if(IfNextToken(tok,TOK_TYPE(','))){
          continue;
        }
        break;
      }
      AssertToken(tok,TOK_TYPE(')'));

      AssertToken(tok,TOK_TYPE('{'));
      i32 bracketsSeen = 0;

      Node* contentHead = 0;
      Node* contentPtr = 0;

      while(!Done(tok)){
        if(IfPeekToken(tok,TOK_TYPE('}'))){
          break;
        }

        Node* line = ParseValue(tok,out);
        LL_Append(contentHead,contentPtr,next,line);
      }
      
      AssertToken(tok,TOK_TYPE('}'));

      // Pack into single node ======================================================
      Node* packHead = nullptr;
      Node* packPtr = nullptr;

      Node* returnType = PushStruct<Node>(out);
      returnType->token = type;

      Node* parameters = PushStruct<Node>(out);
      parameters->type = NodeType_LIST;
      parameters->childs = paramHead;

      Node* content = PushStruct<Node>(out);
      content->type = NodeType_CONTENT;
      content->childs = contentHead;

      LL_Append(packHead,packPtr,next,returnType);
      LL_Append(packHead,packPtr,next,parameters);
      LL_Append(packHead,packPtr,next,content);

      node = MakeNode(out,NodeType_FUNC,name,packHead);
    }

    if(!node && IfNextToken(tok,TokenType_KEYWORD_ARRAY)){
      Token type = AssertToken(tok,TokenType_CONTENT);
      Token arrayName = AssertToken(tok,TokenType_CONTENT);
      AssertToken(tok,TOK_TYPE('{'));

      Node* typeNode = MakeNode(out,NodeType_DECL_TYPE,type);

      Node* head = 0;
      Node* tail = 0;

      LL_Append(head,tail,next,typeNode);

      while(!Done(tok)){
        if(IfPeekToken(tok,TOK_TYPE('}'))){
          break;
        }
        
        Node* value = ParseValue(tok,out);
        LL_Append(head,tail,next,value);
      }

      AssertToken(tok,TOK_TYPE('}'));

      node = MakeNode(out,NodeType_ARRAY,arrayName,head);
    }

    if(!node && IfNextToken(tok,TokenType_KEYWORD_MAP)){
      NodeFlags flags = {};

      if(IfNextToken(tok,TOK_TYPE('('))){
        while(!Done(tok)){
          if(IfPeekToken(tok,TOK_TYPE(')'))){
            break;
          }

          Token modifier = AssertToken(tok,TokenType_CONTENT);
          b32 found = 0;

          if(modifier.id == "shallow"){
            flags |= NodeFlags_SHALLOW;
            found = 1;
          }
          if(modifier.id == "ifChain"){
            flags |= NodeFlags_IF_CHAIN;
            found = 1;
          }

          if(!found){
            printf("USER ERROR: Did not find modifier: %.*s\n",UN(modifier.id));
          }
          Assert(found);
        }

        AssertToken(tok,TOK_TYPE(')'));
      }
      
      Token name = AssertToken(tok,TokenType_CONTENT);
      
      AssertToken(tok,TOK_TYPE('('));
      Token src = AssertToken(tok,TokenType_CONTENT);
      AssertToken(tok,TOK_TYPE(','));
      Token dst = AssertToken(tok,TokenType_CONTENT);
      AssertToken(tok,TOK_TYPE(')'));

      AssertToken(tok,TOK_TYPE('{'));

      Node* head = 0;
      Node* ptr = 0;
      while(!Done(tok)){
        if(IfPeekToken(tok,TOK_TYPE('}'))){
          break;
        }
        
        Node* value = ParseValue(tok,out);
        
        LL_Append(head,ptr,next,value);
      }

      AssertToken(tok,TOK_TYPE('}'));
      
      Node* srcNode = MakeNode(out,NodeType_DECL_NAME,src);
      Node* dstNode = MakeNode(out,NodeType_DECL_NAME,dst);

      srcNode->next = dstNode;

      Node* mapTypes = MakeNode(out,NodeType_LIST,{},srcNode);
      mapTypes->next = head;
      
      node = MakeNode(out,NodeType_MAP,name,mapTypes,flags);
    }
    
    // MARK
    if(!node && IfNextToken(tok,TokenType_KEYWORD_FILE)){
      Token name = AssertToken(tok,TokenType_CONTENT);
      
      AssertToken(tok,TOK_TYPE('{'));

      Node* head = 0;
      Node* ptr = 0;
      while(!Done(tok)){
        if(IfPeekToken(tok,TOK_TYPE('}'))){
          break;
        }

        Node* node = ParseValue(tok,out);
        LL_Append(head,ptr,next,node);
      }

      AssertToken(tok,TOK_TYPE('}'));

      node = MakeNode(out,NodeType_FILE,name,head);
    }

#if 0
    if(!node && IfNextToken(tok,TokenType_KEYWORD_STRUCT)){
      Token name = AssertToken(tok,TokenType_IDENTIFIER);
      AssertToken(tok,TOK_TYPE('{'));

      Node* memberHead = 0;
      Node* memberPtr = 0;
      while(!Done(tok)){
        if(IfPeekToken(tok,TOK_TYPE('}'))){
          break;
        }
        
        Token type = AssertToken(tok,TokenType_IDENTIFIER);
        Token name = AssertToken(tok,TokenType_IDENTIFIER);

        if(IfNextToken(tok,TOK_TYPE('['))){
          Token arraySize = NextToken(tok);

          AssertToken(tok,TOK_TYPE(']'));
        }

        
      }
      
      AssertToken(tok,TOK_TYPE('}'));
    }
#endif

    if(!node){
      Assert(Done(tok));
      break;
    }
    
    LL_Append(head,ptr,next,node);
  }

  return head;
}

Token ParseCType(Tokenizer* tok){
  Token res = {};

  b32 seenIntegerModifier = 0;
  b32 seenBaseType = 0;
  b32 seenType = 0;
  b32 templateLevel = 0;
  b32 first = true;

  while(!Done(tok,{.allowWhitespace = 1})){
    TokenOptions opts = {.allowWhitespace = 1};
    if(first){
      opts.allowWhitespace = 0;
      first = false;
    }

    Token peek = PeekToken(tok,0,opts);

    b32 modifier = 0;
    b32 integerModifier = 0;
    b32 baseType = 0;
    b32 extra = 0;

    modifier |= (peek.type == TokenType_CONTENT && peek.id == "const");
    modifier |= (peek.type == TokenType_CONTENT && peek.id == "static");

    integerModifier |= (peek.type == TokenType_CONTENT && peek.id == "long");
    integerModifier |= (peek.type == TokenType_CONTENT && peek.id == "short");
    integerModifier |= (peek.type == TokenType_CONTENT && peek.id == "unsigned");
    integerModifier |= (peek.type == TokenType_CONTENT && peek.id == "signed");

    baseType |= (peek.type == TokenType_CONTENT && peek.id == "char");
    baseType |= (peek.type == TokenType_CONTENT && peek.id == "int");
    baseType |= (peek.type == TokenType_CONTENT && peek.id == "float");
    baseType |= (peek.type == TokenType_CONTENT && peek.id == "double");

    b32 templateEnter = (peek.type == TOK_TYPE('<'));
    b32 templateExit = (peek.type == TOK_TYPE('>'));

    extra |= (peek.type == TOK_TYPE('*'));
    extra |= (peek.type == TOK_TYPE('&'));

    Assert(peek.type != TokenType_SINGLE_LINE_COMMENT);

    if(templateEnter){
      templateLevel += 1;
    }
    if(templateExit){
      templateLevel -= 1;
      if(templateLevel < 0) templateLevel = 0;
    }

    b32 keepGoing = 0;

    if(modifier){
      keepGoing = 1;
    }
    if(integerModifier){
      seenIntegerModifier = 1;
      keepGoing = 1;
    }
    if(baseType){
      seenBaseType = 1;
      seenType = 1;
      keepGoing = 1;
    }
    if(templateLevel > 0 || templateExit){
      keepGoing = 1;
    }
    if(peek.type == TokenType_WHITESPACE){
      keepGoing = 1;
    }
    if(extra){
      keepGoing = 1;
    }

    if(!keepGoing){
      if(!seenBaseType && !seenIntegerModifier && !seenType && peek.type == TokenType_CONTENT){
        keepGoing = 1;
        seenType = 1;
      }
    }

    if(keepGoing){
      res = Combine(res,peek);
      NextToken(tok,opts);
      continue;
    }
    
    break;
  }

  res.id = TrimWhitespaces(res.id);

  return res;
}

Node* MakeNode(Arena* out,NodeType type,Token token,Node* children,NodeFlags flags){
  Node* res = PushStruct<Node>(out);
  res->type = type;
  res->token = token;
  res->childs = children;
  res->flags = flags;
  return res;
}

Node* MakeModifierNode(Arena* out,Node* modifierContent,Node* insideModifier){
  Node* cons = MakeNode(out,NodeType_MODIFIER_LIST,{},modifierContent);
  cons->next = insideModifier;

  Node* res = MakeNode(out,NodeType_MODIFIER,{},cons);
  return res;
}

i32 GetTableParamIndex(Table* t,String paramName){
  for(int i = 0; i <  t->paramNames.size; i++){
    String s = t->paramNames[i];
    if(s == paramName){
      return i;
    }
  }

  Assert(false);
  return -1;
};

Table* GetTable(String name){
  for(Table* t = Meta_State.tableHead; t; t = t->next){
    if(t->name == name){
      return t;
    }
  }

  printf("\nTable named: %.*s not found\n",UN(name));
  Assert(false);
  return nullptr;
};


Type* RegisterOrGetType(TypeKind kind,String name,Type* inner,String arrayDim){
  for(TypeNode* ptr = Meta_State.typeHead; ptr; ptr = ptr->next){
    if(ptr->type.name == name && ptr->type.inner == inner && ptr->type.arrayDim == arrayDim){
      return &ptr->type;
    }
  }

  TypeNode* newType = PushStruct<TypeNode>(Meta_State.arena);
  newType->type.name = PushString(Meta_State.arena,name);
  newType->type.type = kind;
  newType->type.inner = inner;
  newType->type.arrayDim = arrayDim;

  LL_Append(Meta_State.typeHead,Meta_State.typeTail,next,newType);

  return &newType->type;
}

Type* GetType(String type,Array<String> arrayDims){
  const char* start = type.data;
  const char* ptr = type.data;
  const char* end = type.data + type.size;

  while(ptr < end){
    if(*ptr == '<' || *ptr == '*'){
      break;
    }

    ptr += 1;
  }

  String leftSideContent = String(start,ptr - start);

  String templatePortion = {};
  if(ptr < end && *ptr == '<'){
    ptr += 1;

    const char* templateStart = ptr;

    i32 templateLevel = 0;
    while(ptr < end){
      if(*ptr == '<'){
        templateLevel += 1;
      }
      if(*ptr == '>'){
        templateLevel -= 1;

        if(templateLevel < 0){
          break;
        }
      }
      ptr += 1;
    }

    templatePortion = String(templateStart,ptr - templateStart);
    ptr += 1;
  }

  TypeKind kind = TypeKind_BASE;
  Type* baseType = 0;
  Type* templatedType = 0;

  if(!Empty(templatePortion)){
    templatedType = GetType(templatePortion,{});
    kind = TypeKind_TEMPLATED;
  }
  
  baseType = RegisterOrGetType(kind,leftSideContent,templatedType,{});
  
  while(ptr < end){
    if(*ptr == '*'){
      baseType = RegisterOrGetType(TypeKind_POINTER,{},baseType);
    }

    ptr += 1;
  }

  Assert(ptr >= end);

  for(String arrayDim : arrayDims){
    baseType = RegisterOrGetType(TypeKind_ARRAY,{},baseType,arrayDim);
  }

  return baseType;
}

void GetTypeName(StringBuilder* b,Type* in){
  switch(in->type){
  case TypeKind_NIL:{
    Assert(false);
  } break;
  case TypeKind_BASE:{
    b->PushString(in->name);
  } break;
  case TypeKind_STRUCT:{
    b->PushString(in->name);
  } break;
  case TypeKind_TEMPLATED:{
    b->PushString(in->name);
    b->PushString("<");
    GetTypeName(b,in->inner);
    b->PushString(">");
  } break;
  case TypeKind_ARRAY:{
    GetTypeName(b,in->inner);
    b->PushString("[%.*s]",UN(in->arrayDim));
  } break;
  case TypeKind_POINTER:{
    GetTypeName(b,in->inner);
    b->PushString("*");
  } break;
  }
}

String GetTypeName(Type* in,Arena* out){
  TEMP_REGION(temp,out);

  StringBuilder* b = StartString(temp);
  GetTypeName(b,in);
  String res = EndString(out,b);
  return res;
}

bool IsNil(MetaType* type){
  bool res = (type == nullptr || type->kind == MetaTypeKind_NIL || type == &MetaType_Nil);
  return res;
}

Array<String> GetValue(Node* node,Arena* out){
  TEMP_REGION(temp,out);

  struct TempEnv{
    TempEnv* parent;
    String name;
    Table* table;
    Array<String> values;
  };

  auto HandleModifier = [temp,out](auto HandleModifier,Node* node,TempEnv* env) -> Array<String> {
    if(node->type == NodeType_VALUE){
      String content = node->token.id;

      TokenOptions opts = {.allowWhitespace = 1};
      
      Tokenizer tokInst = {};
      Tokenizer* tok = &tokInst;
      tok->func = ConsumeTokenInternal;
      tok->start = content.data;
      tok->end = content.data + content.size;
      tok->ptr = tok->start;

      auto b = StartString(temp);

      while(!Done(tok,opts)){
        Token token = NextToken(tok,opts);
        
        if(token.type == TOK_TYPE('@')){
          AssertToken(tok,TOK_TYPE('('));

          Token id = NextToken(tok);

          AssertToken(tok,TOK_TYPE('.'));

          Token paramName = NextToken(tok);
          
          TempEnv* value = nullptr;
          for(TempEnv* ptr = env; ptr; ptr = ptr->parent){
            if(ptr->name == id.id){
              value = ptr;
              break;
            }
          }
          Assert(value);

          i32 index = GetTableParamIndex(value->table,paramName.id);
          if(index < 0){
            printf("Bad param name: %.*s\n",UN(paramName.id));
            Assert(false);
          }
          b->PushString(value->values[index]);

          AssertToken(tok,TOK_TYPE(')'));
        } else {
          b->PushString(token.id);
        }
        
      }

      Array<String> res = PushArray<String>(temp,1);
      res[0] = EndString(out,b);
      return res;
    }

    Node* modifierList = node->childs;
    Node* firstModifier = modifierList->childs;

    Node* insideContent = node->childs->next;

    Node* modifierType = modifierList->childs;

    b32 found = 0;
    if(!found && modifierType->token.id == "for"){
      Node* iter = firstModifier->next;
      Node* iterating = iter->next;
      Table* t = GetTable(iterating->token.id);
      
      TempEnv envNode = {};
      envNode.name = iter->token.id;
      envNode.parent = env;
      envNode.table = t;

      auto stringAccum = PushList<String>(temp);
      for(Array<String> values : t->values){
        envNode.values = values;

        Array<String> results = HandleModifier(HandleModifier,insideContent,&envNode);
        for(String str : results){
          *stringAccum->PushElem() = str;
        }
      }

      return PushArray<String>(temp,stringAccum);
    }
    if(!found && modifierType->token.id == "if"){
      Node* id = firstModifier->next;
      Node* paramName = firstModifier->next->next->next;
      Node* compareValue = firstModifier->next->next->next->next->next;

      TempEnv* value = nullptr;
      for(TempEnv* ptr = env; ptr; ptr = ptr->parent){
        if(ptr->name == id->token.id){
          value = ptr;
          break;
        }
      }
      Assert(value);
      
      i32 index = GetTableParamIndex(value->table,paramName->token.id);
      if(index < 0){
        printf("Bad param name: %.*s\n",UN(paramName->token.id));
        Assert(false);
      }
      String val = value->values[index];
      
      String toCompare = compareValue->token.id;

      if(TrimWhitespaces(val) == TrimWhitespaces(toCompare)){
        return HandleModifier(HandleModifier,insideContent,env);
      } else {
        return {};
      }

      // MARK
      
      DEBUG_BREAK();
    }
  };

  Array<String> res = {};
  if(node->type == NodeType_VALUE){
    res.data = &node->token.id;
    res.size = 1;
  }

  if(node->type == NodeType_MODIFIER){
    Array<String> result = HandleModifier(HandleModifier,node,nullptr);

    res = PushArray<String>(out,result.size);
    for(int i = 0; i <  result.size; i++){
      res[i] = PushString(out,result[i]);
    }
  }

  return res;
}

int main(int argc,const char* argv[]){
  bool processHeaders = 0;
  bool skipIfOlder = 0;

  Arena* arena0 = InitArena(Megabyte(128));
  Arena* arena1 = InitArena(Megabyte(128));

  contextArenas[0] = arena0;
  contextArenas[1] = arena1;

  Arena* temp = arena0;
  Arena* persistOverLoops = arena1;

  Meta_State.arena = InitArena(Megabyte(128));
  
  printf("%s\n",argv[1]);
  
  DIR* directory = opendir(argv[1]);

  auto processed = PushList<String>(temp);

  StructDef* structHead = nullptr;
  StructDef* structPtr = nullptr;

  dirent* entry = nullptr;
  while ((entry = readdir(directory)) != NULL){
    if (entry->d_type == DT_DIR) {
      continue;
    }

    String fileName = PushString(persistOverLoops,"%s",entry->d_name);

    String fileNameWithoutDot = fileName;
    while(fileNameWithoutDot.size > 0 && fileNameWithoutDot.data[fileNameWithoutDot.size - 1] != '.'){
      fileNameWithoutDot.size -= 1;
    }
    fileNameWithoutDot.size -= 1;

    b32 alreadyProcessed = 0;
    for(SingleLink<String>* ptr = processed->head; ptr; ptr = ptr->next){
      String name = ptr->elem;

      if(name == fileNameWithoutDot){
        alreadyProcessed = 1;
        break;
      }
    }

    if(alreadyProcessed){
      continue;
    }

    *processed->PushElem() = fileNameWithoutDot;

    printf("Checking %.*s\n",UN(fileNameWithoutDot));

    String headerFilePath = PushString(temp,"%.*s.hpp",UN(fileNameWithoutDot));
    String metaFilePath = PushString(temp,"%.*s.meta",UN(fileNameWithoutDot));
    String fullMetaPath = PushString(temp,"%s/%.*s",argv[1],UN(metaFilePath));

    // Load header and meta file if exists ========================================
    String headerFileContent = {};
    if(processHeaders){
      String fullPath = PushString(temp,"%s/%.*s",argv[1],UN(headerFilePath));
      FILE* headerFile = fopen(CS(fullPath),"r");

      if(headerFile){
        headerFileContent = PushFile(temp,headerFile);
        fclose(headerFile);
      }
    }

    String metaFileContent = {};
    {
      FILE* metaFile = fopen(CS(fullMetaPath),"r");

      if(metaFile){
        metaFileContent = PushFile(temp,metaFile);
        fclose(metaFile);
      }
    }

    String metaHeaderPath = PushString(temp,"%s/%.*s_meta.hpp",argv[1],UN(fileNameWithoutDot));
    String metaSourcePath = PushString(temp,"%s/%.*s_meta.cpp",argv[1],UN(fileNameWithoutDot));

    // Check if the output is outdated ============================================
    bool metaIsNewer = true;

    if(skipIfOlder){
      u128 outputChange = OS_FileLastChange(metaHeaderPath);
      u128 inputChange = OS_FileLastChange(fullMetaPath);
      metaIsNewer = inputChange > outputChange;

      if(metaIsNewer){
        printf("Meta file is newer than meta header\n");
      }
    }

    // Header file meta stuff =====================================================
    if(processHeaders){
      Tokenizer tokInst = {};
      Tokenizer* tok = &tokInst;
      
      tok->func = ConsumeToken;
      tok->start = headerFileContent.data;
      tok->end = headerFileContent.data + headerFileContent.size;
      tok->ptr = tok->start;

      b32 waitingForStruct = 0;
      while(!Done(tok)){
        Token peek = PeekToken(tok,0,{.allowComment = 1});

        b32 advance = 1;
        b32 parseStruct = 0;
        if(waitingForStruct){
          if(peek.type == TokenType_KEYWORD_STRUCT){
            parseStruct = 1;
          } else {
            waitingForStruct = 0;
          }
        }

        if(!waitingForStruct){
          if(peek.type == TokenType_SINGLE_LINE_COMMENT){
            String comment = peek.id;

            String noCommentSymbols = Offset(comment,2);
            String normalized = TrimLeftWhitespaces(noCommentSymbols);

            if(normalized.size >= 4 &&
               normalized[0] == 'm' &&
               normalized[1] == 'e' &&
               normalized[2] == 't' &&
               normalized[3] == 'a'){
              waitingForStruct = 1;
            }
          }
        }

        if(parseStruct){
          NextToken(tok);

          Token structName = AssertToken(tok,TokenType_CONTENT);

          AssertToken(tok,TOK_TYPE('{'));
          
          auto memberList = PushList<MemberDef>(temp);
          while(!Done(tok)){
            if(IfPeekToken(tok,TOK_TYPE('}'))){
              break;
            }

            Token type = ParseCType(tok);

            Token name = AssertToken(tok,TokenType_CONTENT);

            // NOTE: Will I ever have two array dims? 
            auto list = PushList<Token>(temp);
            while(IfPeekToken(tok,TOK_TYPE('['))){
              Token accum = {};

              while(!Done(tok)){
                Token peek = PeekToken(tok);

                if(peek.type == TOK_TYPE(']')){
                  break;
                }

                accum = Combine(accum,peek);
                NextToken(tok);
              }
          
              AssertToken(tok,TOK_TYPE(']'));

              *list->PushElem() = accum;
            }
            Array<Token> arrayDims = PushArray(temp,list);

            Token bitsize = {};
            if(IfNextToken(tok,TOK_TYPE(':'))){
              bitsize = NextToken(tok);
            }

            AssertToken(tok,TOK_TYPE(';'));

            MemberDef* member = memberList->PushElem();
            member->type = type;
            member->name = name;
            member->arrayDims = arrayDims;
            member->bitsize = bitsize;
          }

          AssertToken(tok,TOK_TYPE('}'));

          StructDef* def = PushStruct<StructDef>(temp);
          def->name = structName;
          def->members = PushArray(temp,memberList);
          def->isUnion = false;

          LL_Append(structHead,structPtr,next,def);

          waitingForStruct = 0;
          advance = 0;
        }

        if(advance){
          NextToken(tok,{.allowComment = 1});
        }
      };

      if(structHead){
        printf("Parsed header: %.*s\n",UN(fileName));
        b32 process = 1;
      }
    }
    
    // Meta file processing =======================================================
    Node* metaTop = nullptr;
    {
      Tokenizer tokInst = {};
      Tokenizer* tok = &tokInst;
      tok->func = ConsumeToken;
      tok->start = metaFileContent.data;
      tok->end = metaFileContent.data + metaFileContent.size;
      tok->ptr = tok->start;

      metaTop = Parse(tok,temp);
    }

#if 1
    // Type stuff =================================================================
    MetaType* typesHead = 0;
    MetaType* typesPtr = 0;
    
    auto FindType = [typesHead](String name) -> MetaType*{
      for(MetaType* ptr = typesHead; ptr; ptr = ptr->next){
        if(ptr->name == name){
          return ptr;
        }
      }

      return &MetaType_Nil;
    };

    // First pass on enums ========================================================
    {
      Arena* out = persistOverLoops;
      for(Node* ptr = metaTop; ptr; ptr = ptr->next){
        if(ptr->type != NodeType_ENUM){
          continue;
        }

        MetaType* newType = PushStruct<MetaType>(out);
        newType->kind = MetaTypeKind_ENUM;
        newType->name = PushString(out,ptr->token.id);

        LL_Append(typesHead,typesPtr,next,newType);
      } 
    }   

    auto GetMetaType = [typesHead](String name) -> MetaType*{
      for(MetaType* ptr = typesHead; ptr; ptr = ptr->next){
        if(ptr->name == name){
          return ptr;
        }
      }

      printf("Did not find metatype: %.*s\n",UN(name));
      Assert(false);
      return nullptr;
    };
#endif

    // Parse tables ===============================================================
    {
      Arena* out = temp;

      for(Node* ptr = metaTop; ptr; ptr = ptr->next){
        if(ptr->type != NodeType_TABLE){
          continue;
        }
        
        String name = ptr->token.id;
        Node* parameters = ptr->childs->childs;
        Node* content = ptr->childs->next->childs;

        i32 paramCount = 0;
        for(Node* param = parameters; param; param = param->next){
          
          paramCount += 1;
        }
        
        Array<MetaType*> types = PushArray<MetaType*>(out,paramCount);
        Array<String> params = PushArray<String>(out,paramCount);
        i32 index = 0;
        for(Node* param = parameters; param; param = param->next,index += 1){
          Node* nameNode = param;
          
          if(param->childs){
            Assert(param->type == NodeType_DECL_TYPE);
            
            nameNode = param->childs;

            types[index] = FindType(param->token.id);
          } else {
            types[index] = &MetaType_Nil;
          }
          
          params[index] = nameNode->token.id;
        }
        
        i32 contentCount = 0;
        for(Node* con = content; con; con = con->next){
          contentCount += 1;
        }

        Assert(contentCount % paramCount == 0);
        i32 contentLineCount = contentCount / paramCount;

        Node* con = content;
        Array<Array<String>> contentArray = PushArray<Array<String>>(out,contentLineCount);
        for(i32 i = 0; i < contentLineCount; i++){
          contentArray[i] = PushArray<String>(out,paramCount);
          
          for(i32 param = 0; param < paramCount; param++){
            contentArray[i][param] = con->token.id;
            con = con->next;
          }
        }

        Table* t = PushStruct<Table>(out);
        t->name = name;
        t->paramNames = params;
        t->values = contentArray;

#if 1
        MetaType* newType = PushStruct<MetaType>(out);
        newType->kind = MetaTypeKind_TABLE;
        newType->name = PushString(out,name);

        LL_Append(typesHead,typesPtr,next,newType); 
#endif

        printf("Found table: %.*s\n",UN(name));

        LL_Append(Meta_State.tableHead,Meta_State.tableTail,next,t);
      }
    }
    
    if(metaIsNewer && metaTop){
      Arena* out = temp;

      auto h = StartString(temp);
      auto c = StartString(temp);

      h->PushString("#include \"utils.hpp\"\n\n");
      c->PushString("#include \"%.*s_meta.hpp\"\n\n",UN(fileNameWithoutDot));
      
      // Process enums ==============================================================
      for(Node* ptr = metaTop; ptr; ptr = ptr->next){
        if(ptr->type != NodeType_ENUM){
          continue;
        }

        h->PushString("enum %.*s {\n",UN(ptr->token.id));

        for(Node* line = ptr->childs; line; line = line->next){
          Array<String> contents = GetValue(line,temp);
          
          for(String str : contents){
            String fixed = TrimWhitespaces(str);
            
            h->PushString("  ");
            h->PushString(fixed);
            h->PushString(",\n");
          }
        }
        h->PushString("};\n");
      }

      for(Node* ptr = metaTop; ptr; ptr = ptr->next){
        if(ptr->type != NodeType_ARRAY){
          continue;
        }

        String arrayName = ptr->token.id;
        Node* type = ptr->childs;
        Node* content = ptr->childs->next;

        auto allStrings = PushList<String>(temp);

        for(Node* ptr = content; ptr; ptr = ptr->next){
          Array<String> values = GetValue(ptr,out);

          for(String str : values){
            *allStrings->PushElem() = str;
          }
        }
        
        Array<String> allStringsArray = PushArray(out,allStrings);

        auto b = StartString(temp);
        b->PushString("{\n");
        for(int i = 0; i <  allStringsArray.size; i++){
          String str  =  allStringsArray[i];
          String fixed = TrimWhitespaces(str);

          if(i != 0){
            b->PushString(",\n");
          }
          b->PushString("  ");
          b->PushString(fixed);
        }
        b->PushString("\n}");

        String allValues = EndString(out,b);

        h->PushString("extern Array<%.*s> %.*s;\n",UN(type->token.id),UN(arrayName));

        c->PushString("%.*s %.*s_c_temp[] = %.*s;\n",UN(type->token.id),UN(arrayName),UN(allValues));
        c->PushString("Array<%.*s> %.*s = {%.*s_c_temp,%d};\n",UN(type->token.id),UN(arrayName),UN(arrayName),allStringsArray.size);
      }

      // Process maps ===============================================================
      for(Node* ptr = metaTop; ptr; ptr = ptr->next){
        if(ptr->type != NodeType_MAP){
          continue;
        }

        b32 useIf = (ptr->flags & NodeFlags_IF_CHAIN);
        b32 shallow = (ptr->flags & NodeFlags_SHALLOW);

        Node* mapTypes = ptr->childs;
        Node* mapSrc = mapTypes->childs;
        Node* mapDst = mapSrc->next;
        Node* contentStart = mapTypes->next;

        h->PushString("%.*s %.*s(%.*s in);\n",UN(mapDst->token.id),UN(ptr->token.id),UN(mapSrc->token.id));

        c->PushString("%.*s %.*s(%.*s in){\n",UN(mapDst->token.id),UN(ptr->token.id),UN(mapSrc->token.id));
        c->PushString("  %.*s res = {};\n",UN(mapDst->token.id));
        c->PushString("  bool didIt = 0;\n");

        if(!useIf){
          c->PushString("  switch(in){\n");
        }

        String defaultExpression = {};

        for(Node* ptr = contentStart; ptr; ptr = ptr->next){
          Array<String> values = GetValue(ptr,out);
          for(String str : values){
            const char* start = str.data;
            const char* ptr = str.data;
            const char* end = str.data + str.size;

            while(ptr < end){
              if(*ptr == '-' && *(ptr + 1) == '>'){
                break;
              }

              ptr += 1;
            }
            const char* endOfFirstPart = ptr;

            ptr += 2;
            const char* startOfSecondPart = ptr;

            String firstPart = TrimWhitespaces(String(str.data,endOfFirstPart - str.data));
            String secondPart = TrimWhitespaces(String(startOfSecondPart,str.data + str.size - startOfSecondPart));

            if(firstPart == "default"){
              defaultExpression = secondPart;
              continue;
            }

            if(useIf){
              c->PushString("  if(in == %.*s){res = %.*s; didIt = 1;}\n",UN(firstPart),UN(secondPart));
            } else {
              c->PushString("  case %.*s: res = %.*s; didIt = 1; break;\n",UN(firstPart),UN(secondPart));
            }
          }
        }

        if(!Empty(defaultExpression)){
          if(useIf){
            NOT_IMPLEMENTED(); // TODO: Need 2 passes to properly implement this.
          } else {
            c->PushString("  default: res = %.*s; didIt = 1; break;\n",UN(defaultExpression));
          }
        }

        if(!useIf){
          c->PushString("  }\n");
        }

        if(!shallow){
          c->PushString("  Assert(didIt);\n");
        }
        
        c->PushString("  return res;\n");
        c->PushString("}\n");
      }

      // Process files ==============================================================
      // MARK
      for(Node* ptr = metaTop; ptr; ptr = ptr->next){
        if(ptr->type != NodeType_FILE){
          continue;
        }

        String name = ptr->token.id;
        Array<String> values = GetValue(ptr->childs,temp);

        bool isSingleFile = 0;
        String singleFilepath = {};
        auto files = PushList<FileGroupInfo>(temp);

        for(String pathWithSpaces : values){
          String path = TrimWhitespaces(pathWithSpaces);
          
          FileType type = OS_GetFileTypeFromPath(path);
          Assert(type != FileType_NIL && "Not a folder or a file, not currently handling wathever the problem is");

          if(type == FileType_FILE && values.size == 1){
            isSingleFile = 1;
            singleFilepath = path;
          }

          if(type == FileType_FOLDER){
            Array<FileGroupInfo> res = OS_GetFolderContents(path,temp);

            for(FileGroupInfo r : res){
              *files->PushElem() = r;
            }
          }
        }

        if(isSingleFile){
          String content = PushFile(temp,singleFilepath);
          Assert(content.size >= 0);

          h->PushString("extern String %.*s;\n",UN(name));
          
          c->PushString("String %.*s = \"",UN(name));
          EscapeCString(c,content);
          c->PushString("\";\n");
        } else {
          Array<FileGroupInfo> allFiles = PushArray(temp,files);

          h->PushString("extern Array<FileContent> %.*s;\n",UN(name));

          for(int i = 0; i <  allFiles.size; i++){
            FileGroupInfo file = allFiles[i];

            String fullpath = PushString(temp,"%.*s/%.*s",UN(file.originalRelativePath),UN(file.filename));

            String content = PushFile(temp,fullpath);
            Assert(content.size >= 0);

            c->PushString("static String %.*s_TEMP_%d = \"",UN(name),i);
            EscapeCString(c,content);
            c->PushString("\";\n");
          }
          
          c->PushString("static FileContent %.*s_temp_c_array[] = {\n",UN(name));
          
          for(int i = 0; i <  allFiles.size; i++){
            FileGroupInfo file = allFiles[i];

            if(i != 0){
              c->PushString(",\n");
            }
            c->PushString("  {\"%.*s\",\"%.*s\",{},%.*s_TEMP_%d}",UN(file.filename),UN(file.originalRelativePath),UN(name),i);
          }

          c->PushString("\n};\n");

          c->PushString("Array<FileContent> %.*s = {%.*s_temp_c_array,%d};\n",UN(name),UN(name),allFiles.size);
        }
      }

      // Process functions ==========================================================
      for(Node* ptr = metaTop; ptr; ptr = ptr->next){
        if(ptr->type != NodeType_FUNC){
          continue;
        }

        Node* returnType = ptr->childs;
        Node* parameters = ptr->childs->next;
        Node* content = ptr->childs->next->next;
      
        h->PushString("%.*s %.*s(",UN(returnType->token.id),UN(ptr->token.id));

        b32 first = true;
        for(Node* typedParam = parameters->childs; typedParam; typedParam = typedParam->next){
          if(!first){
            h->PushString(",");
          }
          first = false;

          Node* type = typedParam->childs;
          Node* name = typedParam->childs->next;

          h->PushString("%.*s %.*s",UN(type->token.id),UN(name->token.id));
        }

        h->PushString("){");
      
        for(Node* line = content->childs; line; line = line->next){
          if(line->type == NodeType_FOR_LOOP){
            Node* iterName = line->childs;
            Node* iterating = line->childs->next;
            Node* innerLine = line->childs->next->next;

            Table* table = GetTable(iterating->token.id);

            for(Array<String> tableLine : table->values){
              const char* ptr = innerLine->token.id.data;
              const char* end = innerLine->token.id.data + innerLine->token.id.size;

              while(ptr < end){
                if(*ptr == '@'){
                  ptr += 2;
              
                  const char* closeParan = ptr;
                  while(closeParan < end && *closeParan != ')'){
                    closeParan += 1;
                  }
              
                  Assert(closeParan < end && *closeParan == ')');

                  String name = String(ptr,closeParan - ptr);

                  i32 index = GetTableParamIndex(table,name);
                  String value = tableLine[index];

                  h->PushString("%.*s",UN(value));

                  ptr = closeParan;
                } else {
                  h->PushString("%c",*ptr);
                }

                ptr += 1;
              }
          
              h->PushString("\n");
            }
          }

          if(line->type == NodeType_LINE){
            h->PushString("%.*s\n",UN(line->token.id));
          }
        }
        h->PushString("}\n");
      }

      String headerContent = EndString(temp,h);
      String sourceContent = EndString(temp,c);

      if(headerContent.size > 0){
        FILE* f = fopen(SF("%.*s",UN(metaHeaderPath)),"w");
        if(f){
          fprintf(f,"%.*s",UN(headerContent));
          fclose(f);
        }
      }

      if(sourceContent.size > 0){
        FILE* f = fopen(SF("%.*s",UN(metaSourcePath)),"w");
        if(f){
          fprintf(f,"%.*s",UN(sourceContent));
          fclose(f);
        }
      }

      printf("Processed: %.*s\n",UN(fileNameWithoutDot));
    }
  }

  // Process header parsed data =================================================
  if(processHeaders){
    Arena* out = persistOverLoops;

    // Fill struct types with member data =========================================
    for(StructDef* ptr = structHead; ptr; ptr = ptr->next){
      Type* type = GetType(ptr->name.id,{});
      type->type = TypeKind_STRUCT;

      b32 hasNextSamePointerType = 0;
      Array<Member> members = PushArray<Member>(out,ptr->members.size);
      for(int i = 0; i <  ptr->members.size; i++){
        MemberDef def = ptr->members[i];
        
        Array<String> arrayDims = PushArray<String>(out,def.arrayDims.size);
        for(int ii = 0; ii < arrayDims.size; ii++){
          arrayDims[ii] = def.arrayDims[ii].id;
        }

        members[i].name = PushString(out,def.name.id);
        members[i].type = GetType(def.type.id,arrayDims);

        if(members[i].name == "next" && members[i].type->type == TypeKind_POINTER && members[i].type->inner == type){
          hasNextSamePointerType = true;
        }
      }

      type->members = members;
      type->flags |= TypeFlags_IS_DEFINED;

      if(hasNextSamePointerType){
        type->flags |= TypeFlags_IS_LINKED_LIST_NODE;
      }
    }
    
    GetType("u32")->flags |= TypeFlags_IS_DEFINED;
    GetType("f32")->flags |= TypeFlags_IS_DEFINED;
    GetType("f64")->flags |= TypeFlags_IS_DEFINED;
    GetType("b8")->flags |= TypeFlags_IS_DEFINED;
    GetType("b32")->flags |= TypeFlags_IS_DEFINED;
    GetType("bool")->flags |= TypeFlags_IS_DEFINED;
    GetType("i32")->flags |= TypeFlags_IS_DEFINED;
    GetType("int")->flags |= TypeFlags_IS_DEFINED;
    GetType("char")->flags |= TypeFlags_IS_DEFINED;
    GetType("float")->flags |= TypeFlags_IS_DEFINED;
    GetType("double")->flags |= TypeFlags_IS_DEFINED;
    GetType("ImVec2")->flags |= TypeFlags_IS_DEFINED;
    GetType("Vec2")->flags |= TypeFlags_IS_DEFINED;

    String outputPathHeader = PushString(temp,"%s/parsedHeader_meta.hpp",argv[1]);
    FILE* h = fopen(SF("%.*s",UN(outputPathHeader)),"w");

    auto Skip = [](Type* type) -> b32 {
      if(type->type != TypeKind_STRUCT){
        return true;
      }
      if(!(type->flags & TypeFlags_IS_DEFINED)){
        return true;
      }
      return false;
    };

    // Forward declarations first ==================================================
    for(TypeNode* ptr = Meta_State.typeHead; ptr; ptr = ptr->next){
      Type type = ptr->type;
      if(Skip(&type)){
        continue;
      }

      String name = GetTypeName(&type,temp);
      fprintf(h,"struct %.*s;\n",UN(name));
    }
    fprintf(h,"\n\n");

    // Function declarations ======================================================
    for(TypeNode* ptr = Meta_State.typeHead; ptr; ptr = ptr->next){
      Type type = ptr->type;
      if(Skip(&type)){
        continue;
      }

      String name = GetTypeName(&type,temp);
      fprintf(h,"MetaData META_Pack(%.*s* in,Arena* out);\n",UN(name));
    }
    fprintf(h,"\n\n");

    // Function definitions =======================================================
    for(TypeNode* ptr = Meta_State.typeHead; ptr; ptr = ptr->next){
      Type type = ptr->type;
      if(Skip(&type)){
        continue;
      }

      String name = GetTypeName(&type,temp);
      Array<Member> members = type.members;

      fprintf(h,"MetaData META_Pack(%.*s* in,Arena* out){\n",UN(name));
      fprintf(h,"  MetaData data = {};\n");
      fprintf(h,"  data.structName = \"%.*s\";\n",UN(name));
      fprintf(h,"  if(in == nullptr) return data;\n");
      fprintf(h,"  data.members = PushArray<MetaMember>(out,%d);\n",members.size);
        
      for(int i = 0; i <  members.size; i++){
        Member* def = &members[i];

        String memberName = def->name;
        String typeName = GetTypeName(def->type,temp);

        b32 isPointer = (def->type->type == TypeKind_POINTER);
        b32 isPointeeStruct = 0;
        b32 isPointeeDefined = 0;

        if(isPointer){
          Type* pointee = def->type->inner;
          isPointeeStruct = (pointee->type == TypeKind_STRUCT);
          isPointeeDefined = (pointee->flags & TypeFlags_IS_DEFINED);
        }

        String repr = "{}";
        if(def->type->type == TypeKind_BASE && (def->type->flags * TypeFlags_IS_DEFINED)){
          repr = PushString(temp,"META_Repr(&in->%.*s,out)",UN(memberName));
        }
        if(isPointer && !isPointeeDefined){
          repr = PushString(temp,"META_PointerRepr((void*) in->%.*s,out)",UN(memberName));
        }

        String valueStruct = "{}";
        if((def->type->flags & TypeFlags_IS_DEFINED) && def->type->type == TypeKind_STRUCT){
          valueStruct = PushString(temp,"META_Pack(&in->%.*s,out)",UN(memberName));
        }
        if(isPointer && isPointeeStruct && isPointeeDefined){
          valueStruct = PushString(temp,"META_Pack(in->%.*s,out)",UN(memberName));
        }

        fprintf(h,"  data.members[%d] = {.name = \"%.*s\",.typeName = \"%.*s\",.valueSource = &in->%.*s,.valueString = %.*s,.valueStruct = %.*s};\n",i,UN(memberName),UN(typeName),UN(memberName),UN(repr),UN(valueStruct));
      }

      fprintf(h,"  return data;\n");
      fprintf(h,"}\n");
    }
  }

        
  closedir(directory);

  return 0;
}
