// ======================================
// Start of copy from utils

#include <cstdio>
#include <cstdint>
#include <cstddef>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include <cstdarg>

#include <dirent.h>

#define MIN(A,B) ((A) < (B) ? (A) : (B))
#define MAX(A,B) ((A) > (B) ? (A) : (B))

const char* GetFilename(const char* fullpath){
  const char* lastGood = fullpath;
  const char* ptr = fullpath;

  while(*ptr != '\0'){
    if(*ptr == '/'){
      lastGood = ptr + 1;
    }
    ptr += 1;
  }

  return lastGood;
}

#define NOT_IMPLEMENTED(...) do{ printf("%s:%d:1: error: Not implemented: %s",__FILE__,__LINE__,__PRETTY_FUNCTION__); fflush(stdout); Assert(false); } while(0) // Doesn't mean that something is necessarily future planned
#define NOT_IMPLEMENTED_IF(COND) if(COND){ printf("%s:%d:1: error: Not implemented: %s",__FILE__,__LINE__,__PRETTY_FUNCTION__); fflush(stdout); Assert(false); } 

#define LOCATION() do{ printf("%s:%d\n",GetFilename(__FILE__),__LINE__); fflush(stdout);} while(0)
#define Assert(EXPR) \
  do { \
    bool _ = !(EXPR);   \
if(_){ \
      printf("\n\n\n"); \
      fprintf(stderr,"Assertion failed: %s\n",#EXPR); \
      fprintf(stderr,"At: "); LOCATION(); \
      fflush(stderr); \
      printf("\n\n\n"); \
      __builtin_trap(); \
    } \
  } while(0)

typedef uint8_t Byte;
typedef uint8_t u8;
typedef int8_t i8;
typedef uint16_t u16;
typedef int16_t i16;
typedef uint32_t u32;
typedef int32_t i32;
typedef uint64_t u64;
typedef int64_t i64;
typedef intptr_t iptr;
typedef uintptr_t uptr;
typedef unsigned int uint;
typedef u8 b8;
typedef u32 b32;
typedef float f32;
typedef double f64;

struct Arena{
  Byte* mem;
  size_t used;
  size_t totalAllocated;
  size_t maximum;

  const char* fileCreationPlace;
  int lineCreationPlace;
}; 

static Arena* contextArenas[2];

Arena* GetArena(Arena* diff){
  if(contextArenas[0] != diff){
    return contextArenas[0];
  }

  Assert(contextArenas[1] != diff);
  return contextArenas[1];
}

struct ArenaMark{
  Arena* arena;
  Byte* mark;
}; 


ArenaMark MarkArena(Arena* arena){
  ArenaMark mark = {};
  mark.arena = arena;
  mark.mark = &arena->mem[arena->used];
  return mark;
}

void PopMark(ArenaMark mark){
  Arena* arena = mark.arena;
  int biggerUsed = arena->used;
  arena->used = mark.mark - arena->mem;
}

struct ArenaMarker{
  ArenaMark mark;
  const char* functionName;
  const char* file;
  int line;
  
  ArenaMarker(Arena* arena,const char* file,const char* functionName,int line){
    this->mark = MarkArena(arena);
    this->functionName = functionName;
    this->line = line;
    this->file = file;
  };
  ~ArenaMarker(){
    PopMark(this->mark);};
  operator bool(){return true;}; // For the region trick
};

#define __marker(LINE) marker_ ## LINE
#define _marker(LINE) __marker( LINE )
#define BLOCK_REGION(ARENA) ArenaMarker _marker(__LINE__)(ARENA,__FILE__,__FUNCTION__,__LINE__)

#define TEMP_REGION(NAME,OUT_ARENA) \
  Arena* NAME = GetArena(OUT_ARENA); \
  BLOCK_REGION(NAME)

#define TEMP_REGION2(NAME,OUT_ARENA,OUT_ARENA2) \
  Arena* NAME = GetArena2(OUT_ARENA,OUT_ARENA2); \
  BLOCK_REGION(NAME)


inline size_t Kilobyte(int val){return val * 1024;};
inline size_t Megabyte(int val){return Kilobyte(val) * 1024;};
inline size_t Gigabyte(int val){return Megabyte(val) * 1024;};

#define InitArena(SIZE) InitArena_(SIZE,__FILE__,__LINE__);
Arena* InitArena_(size_t size,const char* file,int line){
  Byte* memory = (Byte*) calloc(size,sizeof(Byte)); 

  if(memory == nullptr){
    fprintf(stderr,"Error allocating memory. Make sure enough memory is available\n");
    exit(1);
  }

  Arena* arena = (Arena*) memory;

  arena->used = 0;
  arena->totalAllocated = size;
  arena->mem = (Byte*) (arena + 1);
  arena->fileCreationPlace = file;
  arena->lineCreationPlace = line;
  
  return arena;
}

Byte* PushBytes(Arena* arena, size_t size){
  Byte* ptr = &arena->mem[arena->used];

  if(arena->used + size > arena->totalAllocated){
    printf("[%s] Used: %zd, Size: %zd, Total: %zd\n",__PRETTY_FUNCTION__,arena->used,size,arena->totalAllocated);
    NOT_IMPLEMENTED("Need to change arena to linked list approach");
    exit(0);
  }
  
  arena->used += size;
  arena->maximum = MAX(arena->maximum,arena->used);

  memset(ptr,0,size);
  
  return ptr;
}

void PushNullByte(Arena* arena){
  Byte* res = PushBytes(arena,1);
  *res = '\0';
}

void AlignArena(Arena* arena,int alignment){
  int offset = arena->used % alignment;

  if(offset == 0){
    return;
  } else {
    PushBytes(arena,alignment - offset);
  }
}

template<typename T>
class ArrayIterator{
public:
   T* ptr;

   inline bool operator!=(const ArrayIterator<T>& iter){return ptr != iter.ptr;};
   inline ArrayIterator<T>& operator++(){++ptr; return *this;};
   inline T& operator*(){return *ptr;};
};

template<typename T> struct Array;

template<typename T>
struct Array{
  T* data;
  int size;

  inline T& operator[](int index) const {Assert(index >= 0);Assert(index < size); return data[index];}
  ArrayIterator<T> begin() const{return ArrayIterator<T>{data};};
  ArrayIterator<T> end() const{return ArrayIterator<T>{data + size};};
};

static constexpr int MyStrLen(const char* str){
  int i = 0;
  while(str[i] != '\0') i++;
  return i;
}

template<typename T>
inline void Memset(Array<T> buffer,T elem){
   for(int i = 0; i < buffer.size; i++){
      buffer.data[i] = elem;
   }
}

template<typename T>
Array<T> PushArray(Arena* arena,int size){AlignArena(arena,alignof(T)); Array<T> res = {}; res.size = size; res.data = (T*) PushBytes(arena,sizeof(T) * size); Memset(res,(T){}); return res;};

inline void MemZero_(void* ptr,ssize_t size){u8* view = (u8*) ptr; for(ssize_t i = 0; i < size; i++) view[i] = 0;};
#define MemZero(PTR,TYPE) MemZero_(PTR,sizeof(TYPE));

template<typename T>
T* PushStruct(Arena* arena){AlignArena(arena,alignof(T)); T* res = (T*) PushBytes(arena,sizeof(T)); MemZero(res,T); return res;};


class StringIterator{
public:
   const char* ptr;

   inline bool operator!=(const StringIterator& iter){return ptr != iter.ptr;};
   inline StringIterator& operator++(){++ptr; return *this;};
   inline const char& operator*(){return *ptr;};
};

struct String{
  const char* data;
  int size;

  String() = default;
  constexpr String(const char* in):data(in),size(MyStrLen(in)){};
  constexpr String(const char* in,int s):data(in),size(s){};

  inline const char& operator[](int index) const {Assert(index >= 0);Assert(index < size); return data[index];}
  StringIterator begin() const{return StringIterator{data};};
  StringIterator end() const{return StringIterator{data + size};};
};

// TODO: I wonder if it would be better to have a arena backed region where we can dump strings and it acts globally, meaning that we must call functions to init it and to clear it everytime we want to use it.
// TODO: It would be better if we just have a malloc failsafe where we allocate more memory if needed.
char* StaticFormat(const char* format,...) __attribute__ ((format (printf, 1, 2)));;

char* StaticFormat(const char* format,...){
  static const int BUFFER_SIZE = 1024*4;
  static char buffer[BUFFER_SIZE];
  static char buffer2[BUFFER_SIZE];
  static char buffer3[BUFFER_SIZE];
  static char buffer4[BUFFER_SIZE];
  static int currentBuffer = 0;
  
  va_list args;
  va_start(args,format);
  int written;

  char* ptr = buffer;
  if(currentBuffer == 1){
    ptr = buffer2;
  }
  if(currentBuffer == 2){
    ptr = buffer3;
  }
  if(currentBuffer == 3){
    ptr = buffer4;
  }

  written = vsnprintf(ptr,BUFFER_SIZE,format,args);

  if(written == BUFFER_SIZE - 1){
    Assert(false); // NOTE: We are probably using StaticFormat in a situation where we shouldn't.
  }

  currentBuffer = (currentBuffer + 1) % 4;
    
  va_end(args);
  
  return ptr;
}

// Shorthand
#define SF(...) StaticFormat(__VA_ARGS__) 

// Shorthand for CString ("converts" a String to a C string using StaticFormat). Instead of having to provide two different functions to handle "const char* str" and "String" 
#define CS(STR) StaticFormat("%.*s",UN(STR))

// Shorthand for UNPACK
#define UN(STR) (STR).size,(STR).data
#define UN_REVERSE(STR) (STR).data,(STR).size

inline b32 Empty(String str){return str.size <= 0 || str.data == nullptr;};
inline String Offset(String in,i32 amount){String str = String(in.data + amount,in.size - amount); if(str.size == 0) str = {}; return str;}

inline b32 IsWhitespace(char ch){bool res = (ch == '\n' || ch == '\t' || ch == '\r' || ch == ' '); return res;};
inline String TrimLeftWhitespaces(String in){String res = in; while(res.size > 0 && IsWhitespace(res.data[0])) { res = Offset(res,1);} return res;};
inline String TrimRightWhitespaces(String in){String res = in; while(res.size > 0 && IsWhitespace(res.data[res.size - 1])) { res.size -= 1;} return res;};
inline String TrimWhitespaces(String in){String res = in; res = TrimLeftWhitespaces(res); res = TrimRightWhitespaces(res); return res;}

String PushString(Arena* arena,String ss){
  int size = ss.size;
  
  Byte* mem = PushBytes(arena,size + 1);

  String res = {};
  res.data = (const char*) mem;
  res.size = size;

  memcpy((void*) res.data,ss.data,ss.size);
  mem[size] = '\0';

  return res;
}

String vPushString(Arena* arena,const char* format,va_list args){
  int extraBuffer; // Just to make sure that vsnprintf with 0 size does not cause problems with a nullptr we pass it just a bit of memory.
  
  va_list copy;
  va_copy(copy,args);

  int stringSizeWithTerm = vsnprintf((char*) &extraBuffer,0,format,copy);
  char* buffer = (char*) PushBytes(arena,stringSizeWithTerm + 1);

  int size = vsnprintf(buffer,stringSizeWithTerm + 1,format,args);

  Assert(size == stringSizeWithTerm);

  String res = String(buffer,size);

  return res;
}

String PushString(Arena* arena,const char* format,...){
  va_list args;
  va_start(args,format);

  String res = vPushString(arena,format,args);

  va_end(args);

  return res;
}

inline bool operator==(String first,String second){
   if(first.size != second.size){
      return false;
   }

   for(int i = 0; i < first.size; i++){
      if(!(first.data[i] == second.data[i])){
         return false;
      }
   }
   return true;
}

long int GetFileSize(FILE* file){
  long int mark = ftell(file);

  fseek(file,0,SEEK_END);
  long int size = ftell(file);

  fseek(file,mark,SEEK_SET);

  return size;
}

String PushFile(Arena* out,FILE* file){
  String res = {};
  long int size = GetFileSize(file);

  AlignArena(out,alignof(void*));

  Byte* mem = PushBytes(out,size);
  int amountRead = fread(mem,sizeof(Byte),size,file);

  if(amountRead != size){
    fprintf(stderr,"Memory PushFile failed to read entire file\n");
    exit(-1);
  }

  res.size = size;
  res.data = (const char*) mem;

  return res;
}

String PushFile(Arena* out,String filepath){
  FILE* file = fopen(SF("%.*s",UN(filepath)),"r");
  
  String res = {};
  
  if(file){
    res = PushFile(out,file);
    fclose(file);
  } else {
    printf("Failed to open file: %.*s\n",UN(filepath));
  }
  
  return res;
}

//TODO: Replace return with Optional. Handle errors
String PushFile(Arena* out,const char* filepath){
  FILE* file = fopen(filepath,"r");
  
  if(!file){
    String res = {};
    printf("Failed to open file: %s\n",filepath);
    NOT_IMPLEMENTED("Need to return opt and let code handle this instead. TODO");
    res.size = -1;
    return res;
  }

  String res = PushFile(out,file);
  fclose(file);

  return res;
}

#define LL_Append(HEAD,PTR,NEXT,NODE) \
  if(HEAD == nullptr){ \
    HEAD = NODE; \
    PTR = NODE; \
  } else if(NODE) { \
    PTR->NEXT = NODE; \
    while(PTR->NEXT) PTR = PTR->NEXT; \
  }

#define LL_PopFront(HEAD,NEXT) \
  HEAD; \
  if(HEAD) HEAD = HEAD->NEXT

#define LL_Push(HEAD,NEXT,NODE) \
  NODE->NEXT = HEAD; \
  HEAD = NODE

#define C_STYLE_ENUM(NAME) \
inline NAME operator|(NAME lhs,NAME rhs){ \
  NAME res = (NAME) ((int) lhs | (int) rhs); \
  return res; \
} \
inline NAME& operator|=(NAME& lhs,NAME rhs){ \
  lhs = (NAME) ((int) lhs | (int) rhs); \
  return lhs; \
} \
inline NAME operator&(NAME lhs,NAME rhs){ \
  NAME res = (NAME) ((int) lhs & (int) rhs); \
  return res; \
} \
inline NAME& operator&=(NAME& lhs,NAME rhs){ \
  lhs = (NAME) ((int) lhs & (int) rhs); \
  return lhs; \
}


template<typename T>
struct SingleLink{
  SingleLink<T>* next;
  T elem;
};

// A list inside an arena. Normally used with a temp arena and then converted to an array in an out arena 
template<typename T>
struct List{
  SingleLink<T>* head;
  SingleLink<T>* tail;
  i32 count;
  Arena* arena; // For now store arena inside structure. 

  T* PushElem();
};

template<typename T>
T* List<T>::PushElem(){
  SingleLink<T>* s = PushStruct<SingleLink<T>>(this->arena);
  *s = {};
  
  if(!this->head){
    this->head = s;
    this->tail = s;
  } else {
    this->tail->next = s;
    this->tail = s;
  }

  this->count += 1;

  return &s->elem;
}

template<typename T>
List<T>* PushList(Arena* out){
  List<T>* res = PushStruct<List<T>>(out);
  *res = {};
  res->arena = out;
  
  return res;
}

template<typename T>
Array<T> PushArray(Arena* out,List<T>* list){
  if(list->count == 0){
    return {};
  }

  Array<T> arr = PushArray<T>(out,list->count);

  i32 index = 0;
  for(SingleLink<T>* iter = list->head; iter; iter = iter->next,index += 1){
    arr[index] = iter->elem;
  }

  return arr;
}

#define STRING_NODE_SIZE (1024 - sizeof(void*) - sizeof(i16)) 
struct StringNode{
  StringNode* next;
  i16 used;
  char buffer[STRING_NODE_SIZE];
};

struct StringBuilder{
  Arena* arena;
  StringNode* head;
  StringNode* tail;
  bool debugPrint;
    
  void Push(char ch);
  void PushString(String str);
  void PushString(const char* format,...) __attribute__ ((format (printf, 2, 3)));
  void vPushString(const char* format,va_list args);

  void PushSpaces(int amount);

  char LastCharOrElse(char orElse);
};


StringBuilder* StartString(Arena* out){
  StringBuilder* builder = PushStruct<StringBuilder>(out);
  builder->arena = out;

  // Simpler to preallocate one node, very rare for a StringBuilder to be started and not used once
  builder->head = PushStruct<StringNode>(out);
  builder->tail = builder->head;
  
  return builder;
}

void StringBuilder::PushString(String string){
  StringNode* ptr = this->tail;

  if(debugPrint){
    printf("%.*s",UN(string));
  }
  
  String str = string;
  while(1){
    i16 toCopyAmount = MIN((long unsigned) str.size,(STRING_NODE_SIZE - ptr->used));
    memcpy(&ptr->buffer[ptr->used],str.data,toCopyAmount);

    str.data += toCopyAmount;
    str.size -= toCopyAmount;
    ptr->used += toCopyAmount;
    
    if(ptr->used == STRING_NODE_SIZE){
      ptr->next = PushStruct<StringNode>(arena);
      ptr = ptr->next;
    }

    if(str.size == 0){
      break;
    }
  }

  this->tail = ptr;
}

// TODO: Performance will drop if this gets called repeatedly and in contexts where we are constantly pushing chars
//       Need to start tracking that sort of stuff in debug mode.
void StringBuilder::Push(char ch){
  String str = {};
  str.data = &ch;
  str.size = 1;
  PushString(str);
}

void StringBuilder::vPushString(const char* format,va_list args){
  TEMP_REGION(temp,arena);
  String toPush = ::vPushString(temp,format,args);
  PushString(toPush);
}

void StringBuilder::PushSpaces(int amount){
  for(int i = 0; i < amount; i++){
    this->Push(' ');
  }
}

char StringBuilder::LastCharOrElse(char orElse){
  StringNode* ptr = this->tail;

  if(ptr->used == 0){
    return orElse;
  }

  return ptr->buffer[ptr->used-1];
}

void StringBuilder::PushString(const char* format,...){
  va_list args;
  va_start(args,format);

  vPushString(format,args);

  va_end(args);
}

String EndString(Arena* out,StringBuilder* builder){
  int totalSize = 0;
  for(StringNode* ptr = builder->head; ptr != nullptr; ptr = ptr->next){
    totalSize += ptr->used;
  }

  Byte* data = PushBytes(out,totalSize + 1); // Null byte appended

  String res = {};
  res.data = (const char*) data;
  res.size = totalSize;
  
  for(StringNode* ptr = builder->head; ptr != nullptr; ptr = ptr->next){
    memcpy(data,ptr->buffer,ptr->used);
    data += ptr->used;
  }

  *data = '\0';
  
  return res;
}

// ======================================
// Meta stuff start

// Parsing ====================================================================

enum TokenType{
  TokenType_NIL,
  TokenType_WHITESPACE,
  TokenType_NEWLINE,
  TokenType_SINGLE_LINE_COMMENT,
  TokenType_UNTERMINATED_MULTI_LINE_COMMENT,
  TokenType_MULTI_LINE_COMMENT,
  TokenType_EOF,
  TokenType_IDENTIFIER,
  TokenType_STRING,
  TokenType_GROUP_DELIM, // @@
  TokenType_ARROW, // ->
  TokenType_NO_DATA,

  TokenType_CHAR_GROUP_0_START = '!',
  TokenType_CHAR_GROUP_0_LAST = '/',
  
  TokenType_CHAR_GROUP_1_START = ':',
  TokenType_CHAR_GROUP_1_LAST = '@',

  TokenType_CHAR_GROUP_2_START = '[',
  TokenType_CHAR_GROUP_2_LAST = '`',

  TokenType_CHAR_GROUP_3_START = '{',
  TokenType_CHAR_GROUP_3_LAST = '~',

  TokenType_MISC,

  TokenType_KEYWORD_TABLE,
  TokenType_KEYWORD_ARRAY,
  TokenType_KEYWORD_ENUM,
  TokenType_KEYWORD_MAP,
  TokenType_KEYWORD_FUNC,
  TokenType_KEYWORD_FOR,
  TokenType_KEYWORD_STRUCT,
};

#define TOK_TYPE(CH) ((TokenType) (CH))
b32 IsNil(TokenType type){b32 res = (type == TokenType_NIL); return res;}

struct Token{
  TokenType type;
  String id;
};

struct Tokenizer;
typedef Token (*TokenizerFunction)(Tokenizer* tok);

struct Tokenizer{
  TokenizerFunction func;
  const char* start;
  const char* ptr;
  const char* end;
};

enum NodeType{
  NodeType_TOP,
  NodeType_TABLE,
  NodeType_ENUM,
  NodeType_ARRAY,
  NodeType_FUNC,
  NodeType_MAP,
  NodeType_MAPIF,
  NodeType_LIST,
  NodeType_PARAMETER,
  NodeType_DECL_NAME,
  NodeType_DECL_TYPE,
  NodeType_CONTENT,
  NodeType_LINE,
  NodeType_VALUE,

  NodeType_MODIFIER,
  NodeType_MODIFIER_LIST,

  NodeType_FOR_LOOP,
};

enum NodeFlags{
  NodeFlags_NIL,
  NodeFlags_IF_CHAIN = (1 << 1),
  NodeFlags_SHALLOW = (1 << 2)
};
C_STYLE_ENUM(NodeFlags);

struct Node{
  Node* next;
  Node* childs;

  NodeType type;
  NodeFlags flags;
  Token token;
};

// NOTE: Default is zero
struct TokenOptions{
  b32 allowNewline : 1;
  b32 allowWhitespace : 1;
  b32 allowComment : 1;
};

bool Done(Tokenizer* tok,TokenOptions opts = {});

Token Combine(Token left,Token right);

Token ConsumeToken(Tokenizer* tok);
Token InternalNextToken(Tokenizer* tok,TokenOptions opts);

Token NextToken(Tokenizer* tok,TokenOptions opts = {});
Token AssertToken(Tokenizer* tok,TokenType type,TokenOptions opts = {});
Token PeekToken(Tokenizer* tok,int lookahead = 0,TokenOptions opts = {});
bool IfNextToken(Tokenizer* tok,TokenType type,TokenOptions opts = {});
bool IfPeekToken(Tokenizer* tok,TokenType type,TokenOptions opts = {});

Node* ParseLine(Tokenizer* tok,Arena* out);
Node* Parse(Tokenizer* tok,Arena* out);

Token ParseCType(Tokenizer* tok);

Node* MakeNode(Arena* out,NodeType type,Token token = {},Node* children = 0,NodeFlags = {});
Node* MakeModifierNode(Arena* out,Node* modifierContent,Node* insideModifier);

// Compilation ================================================================

struct Table{
  Table* next;

  String name;
  
  Array<String> paramNames;
  Array<Array<String>> values;
};

// ======================================
// C style types

struct MemberDef{
  Token name;
  Token type;
  Array<Token> arrayDims;
  Token bitsize;
};

struct StructDef{
  StructDef* next;
  
  Token name;
  Array<MemberDef> members;
  b32 isUnion;
};

enum TypeKind{
  TypeKind_NIL,
  TypeKind_BASE,
  TypeKind_STRUCT,
  TypeKind_TEMPLATED,
  TypeKind_ARRAY,
  TypeKind_POINTER
};

struct Type;

struct Member{
  String name;
  Type* type;
};

enum TypeFlags{
  TypeFlags_NONE = 0,
  TypeFlags_IS_DEFINED = (1 << 1),
  TypeFlags_IS_LINKED_LIST_NODE = (1 << 2)
};
C_STYLE_ENUM(TypeFlags);

struct Type{
  TypeKind type;
  String name;

  String arrayDim;
  Type* inner;
  Array<Member> members;
  TypeFlags flags;
};

struct TypeNode{
  TypeNode* next;
  Type type;
};

i32 GetTableParamIndex(Table* t,String paramName);
Table* GetTable(String name);

Type* RegisterOrGetType(TypeKind kind,String name,Type* inner = nullptr,String arrayDim = {});
Type* GetType(String type,Array<String> arrayDims = {});

void GetTypeName(StringBuilder* b,Type* in);
String GetTypeName(Type* in,Arena* out);

// ======================================
// Meta types

enum MetaTypeKind{
  MetaTypeKind_NIL,
  MetaTypeKind_TABLE,
  MetaTypeKind_ENUM
};

struct MetaType{
  MetaType* next;
  
  MetaTypeKind kind;
  String name;
};
static MetaType MetaType_Nil = {};

// NOTE: Also acts as environment
struct Meta_StateTag{
  Arena* arena;

  TypeNode* typeHead;
  TypeNode* typeTail;

  Table* tableHead;
  Table* tableTail;
};

static Meta_StateTag Meta_State = {};

bool IsNil(MetaType* type);

Array<String> GetValue(Node* node,Arena* out);
