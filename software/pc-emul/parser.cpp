#include "parser.hpp"

#include <cstring>

#include "utils.hpp"

static int IsWhitespace(char ch){
   if(ch == '\n' || ch == ' ' || ch == '\t'){
      return 1;
   }

   return 0;
}

void Tokenizer::ConsumeWhitespace(){
   if(keepWhitespaces){
      return;
   }

   while(1){
      if(ptr >= end)
         return;

      if(ptr[0] == '\n'){
         ptr += 1;
         lines += 1;
         continue;
      }

      if(IsWhitespace(ptr[0])){
         ptr += 1;
         continue;
      }

      if(!keepComments){
         if(ptr[0] == '/' && ptr[1] == '/'){
            while(ptr < end && *ptr != '\n') ptr += 1;
            ptr += 1;
            continue;
         }

         if(ptr[0] == '/' && ptr[1] == '*'){
            while(ptr + 1 < end &&
                  !(ptr[0] == '*' && ptr[1] == '/')) ptr += 1;
            ptr += 2;
            continue;
         }
      }

      return;
   }
}

Tokenizer::Tokenizer(SizedString content,const char* singleChars,std::vector<std::string> specialCharsList)
:start(content.str)
,ptr(content.str)
,end(content.str + content.size)
,lines(1)
,singleChars(singleChars)
,specialChars(specialCharsList)
{
   keepWhitespaces = false;
   keepComments = false;
}

int Tokenizer::SpecialChars(const char* ptr, unsigned int size){
   for(std::string special : specialChars){
      if(special.size() >= size){
         continue;
      }

      SizedString specialStr = {special.c_str(),(int) special.size()};
      SizedString ptrStr = {ptr,specialStr.size};

      if(CompareString(ptrStr,specialStr)){
         return specialStr.size;
      }
   }

   for(size_t i = 0; i < singleChars.size(); i++){
      if(singleChars[i] == ptr[0]){
         return 1;
      }
   }

   return 0;
}

Token Tokenizer::PeekToken(){
   Token token = {};

   if(ptr >= end){
      return token;
   }

   ConsumeWhitespace();

   // After this, only need to set size
   token.str = ptr;

   // Handle special tokens (custom)
   int size = SpecialChars(ptr,end - ptr);

   if(size != 0){
      token.size = size;

      return token;
   }

   for(int i = 1; &ptr[i] <= end; i++){
      token.size = i;
      if((SpecialChars(&ptr[i],end - &ptr[i])) || IsWhitespace(ptr[i])){
         return token;
      }
   }

   return token;
}

Token Tokenizer::NextToken(){
   Token res = PeekToken();
   AdvancePeek(res);
   return res;
};

Token Tokenizer::AssertNextToken(const char* format){
   Token token = NextToken();

   if(!CompareToken(token,format)){
      // Get start of the line
      const char* lineStart = token.str;
      while(lineStart > start){
         lineStart -= 1;
         if(lineStart[0] == '\n'){
            lineStart += 1;
            break;
         }
      }

      // Get end of the line
      const char* lineEnd = token.str;
      while(lineEnd < end){
         lineEnd += 1;
         if(lineEnd[0] == '\n'){
            break;
         }
      }

      SizedString fullLine = MakeSizedString(lineStart,lineEnd - lineStart);

      printf("Parser Error. Expected to find: '%s'\n",format);
      printf("%d\t | %.*s\n",lines,UNPACK_SS(fullLine));
      printf("\t | ");

      for(int i = 0; i < token.str - lineStart; i++){
         printf(" ");
      }
      printf("^\n");

      DEBUG_BREAK;
   }

   return token;
}

Token Tokenizer::PeekFindUntil(const char* str){
   Token token = {};

   SizedString ss = MakeSizedString(str);

   int i = 0;
   while(&ptr[i] + ss.size <= end){
      if(ptr[i] == str[0]){
         SizedString cmp = MakeSizedString(&ptr[i],ss.size);

         if(CompareString(ss,cmp)){
            token.size = i;
            token.str = ptr;

            return token;
         }
      }

      if(!keepComments){
         if(&ptr[i+1] + ss.size < end && ptr[i] == '/' && ptr[i+1] == '/'){
            while(ptr[i] != '\n'){
               i += 1;
            }
            continue;
         }

         if(&ptr[i+1] + ss.size < end && ptr[i] == '/' && ptr[i+1] == '*'){
            while(&ptr[i] + ss.size < end && (ptr[i-1] != '*' || ptr[i] != '/')){
               i += 1;
            }
            i += 1;
            continue;
         }
      }

      i += 1;
   }

   token.size = -1; // Didn't find anything

   return token;
}

Token Tokenizer::PeekFindIncluding(const char* str){
   Token tok = PeekFindUntil(str);

   if(tok.size == -1){
      return tok;
   }

   tok.size += strlen(str);

   return tok;
}

Token Tokenizer::FindFirst(std::initializer_list<const char*> strings){
   Token best = {};
   best.size = -1;

   const char* res = nullptr;
   bool first = true;
   for(const char* str : strings){
      Token token = PeekFindUntil(str);

      if(first){
         if(token.size != -1){
            first = false;
            best = token;
            res = str;
         }
      }

      if(token.size > 0 && token.size < best.size){
         best = token;
         res = str;
      }
   }

   if(res){
      return MakeSizedString(res);
   } else {
      Token noFound = {};
      noFound.size = -1;

      return noFound;
   }
}

Token Tokenizer::NextFindUntil(const char* str){
   Token token = PeekFindUntil(str);
   AdvancePeek(token);

   return token;
}

Token Tokenizer::PeekWhitespace(){
   Token token = {};

   token.str = ptr;

   for(int i = 0; &ptr[i] <= end; i++){
      if(!IsWhitespace(ptr[i])){
         token.size = i;
         break;
      }
   }

   return token;
}

Token Tokenizer::Finish(){
   Token token = {};

   token.str = ptr;
   token.size = end - ptr;

   ptr = end;

   return token;
}

void* Tokenizer::Mark(){
   return (void*) ptr;
}

Token Tokenizer::Point(void* mark){
   Assert(ptr >= mark);

   Token token = {};
   token.str = ((char*)mark);
   token.size = ptr - ((char*)mark);

   return token;
}

void Tokenizer::Rollback(void* mark){
   ptr = (const char*) mark;
}

void Tokenizer::SetSingleChars(const char* singleChars){
   this->singleChars = std::string(singleChars);
}

void Tokenizer::AdvancePeek(Token tok){
   for(int i = 0; i < tok.size; i++){
      if(tok.str[i] == '\n'){
         lines += 1;
      }
   }
   ptr += tok.size;
}

bool Tokenizer::Done(){
   void* mark = Mark();

   ConsumeWhitespace();

   bool res = ptr >= end;

   Rollback(mark);

   return res;
}

bool Tokenizer::IsSpecialOrSingle(SizedString toTest){
   if(toTest.size > 1){
      for(std::string& str : specialChars){
         if(CompareString(str.c_str(),toTest)){
            return true;
         }
      }
   } else {
      for(char ch : singleChars){
         if(ch == toTest.str[0]){
            return true;
         }
      }
   }

   return false;
}

SizedString Tokenizer::PeekUntilDelimiterExpression(SizedString open,SizedString close, int numberOpenSeen){
   Assert(IsSpecialOrSingle(open));
   Assert(IsSpecialOrSingle(close));

   void* pos = Mark();

   SizedString res = {};
   res.str = (const char*) pos;

   int seen = numberOpenSeen;
   while(!Done()){
      Token tok = PeekToken();

      if(CompareString(tok,open)){
         seen += 1;
      } else if(CompareString(tok,close)){
         if(seen == 0){
            break;
         }

         seen -= 1;

         if(seen == 0){
            res = Point(pos);
            break;
         }
      }

      AdvancePeek(tok);
   }

   Rollback(pos); // Act as peek

   return res;
}

bool CheckStringOnlyWhitespace(Token tok){
   for(int i = 0; i < tok.size; i++){
      char ch = tok.str[i];
      if(!IsWhitespace(ch)){
         return false;
      }
   }
   return true;
}

bool CheckFormat(const char* format,Token tok){
   int tokenIndex = 0;
   for(int formatIndex = 0; 1;){
      char formatChar = format[formatIndex];

      if(formatChar == '\0'){
         if(tok.size == tokenIndex){
            return true;
         } else {
            return false;
         }
      }

      if(tokenIndex > tok.size){
         return false;
      }

      if(formatChar == '%'){
         char type = format[formatIndex + 1];
         formatIndex += 2;

         switch(type){
         case 'd':{
            if(!IsNum(tok.str[tokenIndex])){
               return false;
            }

            for(; tokenIndex < tok.size; tokenIndex++){
               if(!IsNum(tok.str[tokenIndex])){
                  break;
               }
            }
         }break;
         case '\0':{
            NOT_POSSIBLE;
         }break;
         default:{
            NOT_IMPLEMENTED;
         }break;
         }
      } else if(formatChar != tok.str[tokenIndex]){
         return false;
      } else {
         formatIndex += 1;
         tokenIndex += 1;
      }
   }

   return true;
}

bool Contains(SizedString str,const char* toCheck){
   Tokenizer tok(str,"",{});

   Token token = tok.PeekFindUntil(toCheck);

   if(token.size > 0){
      return true;
   }

   return false;
}

Token ExtendToken(Token t1,Token t2){
   if(t1.size == -1){
      return t2;
   }

   Assert(t1.str <= t2.str);
   Assert(t2.size + t2.str >= t1.size + t1.str);

   if(t1.str == t2.str){
      return t2;
   }

   t1.size = (t2.str + t2.size) - t1.str;

   return t1;
}

int CountSubstring(SizedString str,SizedString substr){
   if(substr.size == 0){
      return 0;
   }

   int count = 0;
   for(int i = 0; i < (str.size - substr.size + 1); i++){
      const char* view = &str.str[i];

      bool possible = true;
      for(int ii = 0; ii < substr.size; ii++){
         if(view[ii] != substr.str[ii]){
            possible = false;
            break;
         }
      }

      if(possible){
         count += 1;
      }
   }

   return count;
}

void StoreToken(Token token,char* buffer){
   memcpy(buffer,token.str,token.size);
   buffer[token.size] = '\0';
}

int CompareStringNoEnd(const char* strNoEnd,const char* strWithEnd){
   for(int i = 0; strWithEnd[i] != '\0'; i++){
      if(strNoEnd[i] != strWithEnd[i]){
         return 0;
      }
   }

   return 1;
}

// The following two functions together will error if not decimal or hexadecimal
static int CharToInt(char ch){
   if(ch >= '0' && ch <= '9'){
      return ch - '0';
   } else if(ch >= 'A' && ch <= 'F'){
      return 10 + (ch - 'A');
   } else if(ch >= 'a' && ch <= 'f'){
      return 10 + (ch - 'a');
   } else {
      NOT_IMPLEMENTED;
   }
   return 0;
}

int ParseInt(SizedString ss){
   const char* str = ss.str;
   int size = ss.size;

   int sign = 1;
   if(size >= 1 && str[0] == '-'){
      sign = -1;
      size -= 1;
      str += 1;
   }

   if(size >= 2 && str[0] == '0' && (str[1] == 'x' || str[1] == 'X')){ // Hexadecimal
      int res = 0;

      for(int i = 2; i < size; i++){
         res *= 16;
         res += CharToInt(str[i]);
      }

      return (res * sign);
   } else {
      int res = 0;
      for(int i = 0; i < size; i++){
         Assert(str[i] >= '0' && str[i] <= '9');

         res *= 10;
         res += CharToInt(str[i]);
      }
      return (res * sign);
   }
}

bool IsNum(char ch){
   bool res = (ch >= '0' && ch <= '9');

   return res;
}

struct OperationList{
   const char** op;
   int nOperations;
   OperationList* next;
};

Expression* ParseOperationType_(Tokenizer* tok,OperationList* operators,ParsingFunction finalFunction,Arena* tempArena){
   void* start = tok->Mark();

   if(operators == nullptr){
      Expression* expr = finalFunction(tok);

      expr->text = tok->Point(start);
      return expr;
   }

   OperationList* nextOperators = operators->next;
   Expression* current = ParseOperationType_(tok,nextOperators,finalFunction,tempArena);

   while(1){
      Token peek = tok->PeekToken();

      bool foundOne = false;
      for(int i = 0; i < operators->nOperations; i++){
         const char* elem = operators->op[i];

         if(CompareString(peek,elem)){
            tok->AdvancePeek(peek);
            Expression* expr = PushStruct(tempArena,Expression);
            expr->expressions = PushArray(tempArena,2,Expression*);

            expr->type = Expression::OPERATION;
            expr->op = elem;
            expr->size = 2;
            expr->expressions[0] = current;
            expr->expressions[1] = ParseOperationType_(tok,nextOperators,finalFunction,tempArena);

            current = expr;
            foundOne = true;
         }
      }

      if(!foundOne){
         break;
      }
   }

   current->text = tok->Point(start);
   return current;
}

Expression* ParseOperationType(Tokenizer* tok,std::initializer_list<std::initializer_list<const char*>> operators,ParsingFunction finalFunction,Arena* tempArena){
   OperationList head = {};
   OperationList* ptr = nullptr;

   for(std::initializer_list<const char*> outerList : operators){
      if(ptr){
         ptr->next = PushStruct(tempArena,OperationList);
         ptr = ptr->next;
      } else {
         ptr = &head;
      }

      ptr->op = PushArray(tempArena,outerList.size(),const char*);

      for(const char* str : outerList){
         ptr->op[ptr->nOperations++] = str;
      }
   }

   Expression* expr = ParseOperationType_(tok,&head,finalFunction,tempArena);

   return expr;
}

