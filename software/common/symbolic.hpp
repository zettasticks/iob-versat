#pragma once

#include "utils.hpp"

// TODO: The logic in relation to negative is becoming a bit tiring. If we need to keep expanding this code, might need to find some pattern to simplify things. The main problem is when the structure is changed by any of the functions, in these cases the logic for how negativity is preserved is more complicated than simply copying what we already have.

// TODO: We might have to implement a way of grouping variables, so we can group the loop variables for an address access. This might simplify things somewhat. Need to see further how the code turns out for the address access

// TODO: A lot of places are normalizing when there is no need. The more normalizations we perform on the lower levels the worse performance and memory penalties we incur. Normalization only needs to be performed if needed, otherwise try to keep whatever format we have

struct Arena;

#if 0
// ============================================================================
// Symbolic Expressions

enum SymbolicExpressionType{
  SymbolicExpressionType_LITERAL,
  SymbolicExpressionType_VARIABLE,
  SymbolicExpressionType_SUM,
  SymbolicExpressionType_MUL,
  SymbolicExpressionType_DIV,
  SymbolicExpressionType_FUNC
};

enum SymbolicReprType{
  SymbolicReprType_LITERAL,
  SymbolicReprType_VARIABLE,
  SymbolicReprType_OP
};

struct SymbolicReprAtom{
  SymbolicReprType type;
  
  union{
    String variable;
    int literal;
    char op;
  };
};

struct SymbolicExpression{
  SymbolicExpressionType type;
  bool negative;

  // All these should be inside a union. Not handling this for now
  int literal;

  union {
    String variable;
    String name;
  };
    
  // For div, imagine a fraction expression
  struct {
    SymbolicExpression* top;
    SymbolicExpression* bottom;
  };
  
  Array<SymbolicExpression*> terms; // Terms of SUM and MUL. Function arguments for FUNC
};

struct MultPartition{
  SymbolicExpression* base;
  SymbolicExpression* leftovers;
}; 

// ======================================
// Globals

extern SymbolicExpression* SYM_zero;
extern SymbolicExpression* SYM_one;
extern SymbolicExpression* SYM_two;
extern SymbolicExpression* SYM_eight;
extern SymbolicExpression* SYM_thirtyTwo;
extern SymbolicExpression* SYM_dataW;
extern SymbolicExpression* SYM_addrW;
extern SymbolicExpression* SYM_axiAddrW;
extern SymbolicExpression* SYM_axiDataW;
extern SymbolicExpression* SYM_delayW;
extern SymbolicExpression* SYM_lenW;
extern SymbolicExpression* SYM_axiStrobeW;
extern SymbolicExpression* SYM_dataStrobeW;

// ======================================
// Representation

void   Print(SymbolicExpression* expr,bool printNewLine = false);
void   Repr(StringBuilder* builder,SymbolicExpression* expr);
String PushRepr(Arena* out,SymbolicExpression* expr);
char*  DebugRepr(SymbolicExpression* expr);

// ======================================
// High level representation compilation

Array<SymbolicReprAtom> CompileRepresentation(SymbolicExpression* expr,Arena* out);

// ======================================
// Evaluation, mostly debugging purposes for now.

int Evaluate(SymbolicExpression* expr,Hashmap<String,int>* values);
Opt<int> ConstantEvaluate(SymbolicExpression* expr); // Only evaluates successfully if the expression can be evaluated directly (no variables).

// ======================================
// Low level symbolic building blocks

SymbolicExpression* PushLiteral(Arena* out,int value,bool negate = false);
SymbolicExpression* PushVariable(Arena* out,String name,bool negate = false);

SymbolicExpression* SymbolicAdd(SymbolicExpression* left,SymbolicExpression* right,Arena* out);
SymbolicExpression* SymbolicAdd(Array<SymbolicExpression*> terms,Arena* out);
SymbolicExpression* SymbolicAdd(Array<SymbolicExpression*> terms,SymbolicExpression* extra,Arena* out);
SymbolicExpression* SymbolicSub(SymbolicExpression* left,SymbolicExpression* right,Arena* out);
SymbolicExpression* SymbolicMult(SymbolicExpression* left,SymbolicExpression* right,Arena* out);
SymbolicExpression* SymbolicMult(Array<SymbolicExpression*> terms,Arena* out);
SymbolicExpression* SymbolicMult(Array<SymbolicExpression*> terms,SymbolicExpression* extra,Arena* out);
SymbolicExpression* SymbolicDiv(SymbolicExpression* top,SymbolicExpression* bottom,Arena* out);

// nocheckin: Make this proper, not the hacky stuff that we did
SymbolicExpression* SymbolicMax(ArenaList<SymbolicExpression*>* elems,Arena* out);

// For the cases where we are dealing with ranges and want to calculate size([A,B] = B - A + 1)
SymbolicExpression* SymbolicSubPlusOne(SymbolicExpression* higher,SymbolicExpression* lower,Arena* out);

SymbolicExpression* SymbolicFunc(String functionName,Array<SymbolicExpression*> terms,Arena* out);

// ======================================
// Parsing

// nocheckin: Either remove it or move it to new parser.
SymbolicExpression* ParseSymbolicExpression(String content,Arena* out);

// ======================================
// Low level manipulation

// Use this function to get the literal value of a literal type expression, takes into account negation.
int GetLiteralValue(SymbolicExpression* expr);
Array<String> GetAllSymbols(SymbolicExpression* expr,Arena* out);

bool ExpressionEqual(SymbolicExpression* left,SymbolicExpression* right);
bool IsZero(SymbolicExpression* expr);

SymbolicExpression* SymbolicDeepCopy(SymbolicExpression* expr,Arena* out);

// Must call normalizeLiteral before calling this.
// Furthermore, the negative is removed from leftovers and pushed onto the literal (base)
MultPartition CollectTermsWithLiteralMultiplier(SymbolicExpression* expr,Arena* out);

MultPartition PartitionMultExpressionOnVariable(SymbolicExpression* expr,String variableToPartition,Arena* out);

// ======================================
// High level symbolic manipulations 

SymbolicExpression* NormalizeLiterals(SymbolicExpression* expr,Arena* out);
SymbolicExpression* RemoveParenthesis(SymbolicExpression* expr,Arena* out);
// This function does not allocate new nodes unless it has to.
// Meaning that memory associated to expr must be kept valid as well as the memory for the return of this function
SymbolicExpression* CollectTerms(SymbolicExpression* expr,Arena* out);
SymbolicExpression* ApplyDistributivity(SymbolicExpression* expr,Arena* out);
SymbolicExpression* ApplySimilarTermsAddition(SymbolicExpression* expr,Arena* out);
SymbolicExpression* MoveDivToTop(SymbolicExpression* base,Arena* out);
SymbolicExpression* NormalizeLiterals(SymbolicExpression* expr,Arena* out);


SymbolicExpression* SymbolicReplace(SymbolicExpression* base,String varToReplace,SymbolicExpression* replacingExpr,Arena* out);

SymbolicExpression* ReplaceVariables(SymbolicExpression* expr,Hashmap<String,SymbolicExpression*>* values,Arena* out);
SymbolicExpression* ReplaceVariables(SymbolicExpression* expr,TrieMap<String,SymbolicExpression*>* values,Arena* out);

SymbolicExpression* Normalize(SymbolicExpression* expr,Arena* out,bool debugPrint = false);
SymbolicExpression* Derivate(SymbolicExpression* expr,String base,Arena* out);

// Are allowed to call normalize
SymbolicExpression* Group(SymbolicExpression* expr,String variableToGroupWith,Arena* out);

void TestSymbolic();

// ============================================================================
// LoopLinearSums

// Expr must be a sum of mul. Assuming that variableName only appears once. TODO: Probably would be best to create a function that first groups everything so that variableName only appears once and then call this function. Or maybe do the grouping inside here if needed.
// TODO: This function is kinda not needed. We only use it to break apart a symbolic expression into a LoopLinearSumTerm, but even then we can do it better because we know the format of the variables inside the symbolic expression and we could simplify this.
Opt<SymbolicExpression*> GetMultExpressionAssociatedTo(SymbolicExpression* expr,String variableName,Arena* out);
#endif

void SYM_Init();

enum SYM_Type{
  // Order is important, it encodes the order of the way terms should be displayed (literals first, vars second and so on)
  SYM_Type_LITERAL,
  SYM_Type_VARIABLE,
  SYM_Type_SUM,
  SYM_Type_MUL,
  SYM_Type_DIV,
  SYM_Type_FUNC // We only care about 2 args functions so no need to support more than that.
};

inline String META_Repr(SYM_Type val){
  switch(val){
    case SYM_Type_LITERAL : {
      return String("SYM_Type_LITERAL");
    } break;
    case SYM_Type_VARIABLE : {
      return String("SYM_Type_VARIABLE");
    } break;
    case SYM_Type_SUM : {
      return String("SYM_Type_SUM");
    } break;
    case SYM_Type_MUL : {
      return String("SYM_Type_MUL");
    } break;
    case SYM_Type_DIV : {
      return String("SYM_Type_DIV");
    } break;
    case SYM_Type_FUNC : {
      return String("SYM_Type_FUNC");
    } break;
  }
  Assert(false);
  return {};
}

struct SYM_Node;
struct SYM_Expr{
  SYM_Node* node;
};

extern SYM_Expr SYM_Zero;
extern SYM_Expr SYM_One;
extern SYM_Expr SYM_Two;
extern SYM_Expr SYM_Eight;

extern SYM_Expr SYM_AddrW;
extern SYM_Expr SYM_AxiAddrW;
extern SYM_Expr SYM_AxiDataW;
extern SYM_Expr SYM_AxiStrobeW;
extern SYM_Expr SYM_LenW;
extern SYM_Expr SYM_DelayW;
extern SYM_Expr SYM_DataW;
extern SYM_Expr SYM_DataStrobeW;

inline SYM_Node* Negate(SYM_Node* ptr);
inline bool IsNegative(SYM_Node* ptr);

SYM_Expr Abs(SYM_Expr in);

bool IsLiteral(SYM_Expr in);
int LiteralValue(SYM_Expr in);

inline bool operator==(SYM_Expr lhs,SYM_Expr rhs){
  // TODO: Instead of trying to "fix" negative zero, just change the negate function to 
  //       never negate if the expr points to literal zero. It is just easier.
  if(IsLiteral(lhs) && IsLiteral(rhs) && LiteralValue(lhs) == LiteralValue(rhs)){
    return true;
  }

  return lhs.node == rhs.node;
}

inline bool operator!=(SYM_Expr lhs,SYM_Expr rhs){return !(lhs == rhs);}
inline SYM_Expr Negate(SYM_Expr expr){SYM_Expr res = {Negate(expr.node)}; return res;}
inline bool IsNegative(SYM_Expr expr){return IsNegative(expr.node);}

struct SYM_Node{
  SYM_Type type;

  String name; // For functions
  union{
    int literal;
    String variable;

    struct {
      union{
        SYM_Expr top;
        SYM_Expr left;
        SYM_Expr first;
      };
      union {
        SYM_Expr bottom;
        SYM_Expr right;
        SYM_Expr second;
      };
    };
  };
  
  SYM_Node* hashNext;
};

inline SYM_Node* Negate(SYM_Node* ptr){return (SYM_Node*) (((iptr) ptr) ^ 0x1);}
inline bool IsNegative(SYM_Node* ptr){return (((iptr) ptr) & 0x1);}
inline SYM_Node* GetPointer(SYM_Node* ptr){return (SYM_Node*) (((iptr) ptr) & ~0x1);}

inline SYM_Node* GetPointer(SYM_Expr expr){return GetPointer(expr.node);}

// nocheckin (REMOVE THIS SINCE EVERY SYM_EXPR IS NOW VALID)
inline bool Valid(SYM_Expr expr){return true;}
void SYM_Print(SYM_Expr expr);

SYM_Expr operator+(SYM_Expr left,SYM_Expr right);
SYM_Expr operator-(SYM_Expr left,SYM_Expr right);
SYM_Expr operator-(SYM_Expr right);
SYM_Expr operator*(SYM_Expr left,SYM_Expr right);
SYM_Expr operator/(SYM_Expr left,SYM_Expr right);

SYM_Expr SYM_Variable(String name);
SYM_Expr SYM_Func(String name,SYM_Expr first,SYM_Expr second);
SYM_Expr SYM_Literal(int value);

SYM_Expr SYM_Replace(SYM_Expr expr,TrieMap<String,SYM_Expr>* replacements);
SYM_Expr SYM_Replace(SYM_Expr expr,TrieMap<SYM_Expr,SYM_Expr>* replacements);
SYM_Expr SYM_Replace(SYM_Expr expr,SYM_Expr toReplace,SYM_Expr replacement);

SYM_Expr SYM_Derivate(SYM_Expr expr,String var);

SYM_Expr SYM_Factor(SYM_Expr expr,SYM_Expr commonFactor);

bool SYM_IsZeroValue(SYM_Expr expr);
bool SYM_IsOneValue(SYM_Expr expr);

SYM_Expr SYM_Normalize(SYM_Expr in);

void SYM_Repr(StringBuilder* b,SYM_Expr expr);
String SYM_Repr(SYM_Expr expr,Arena* out);

Pair<SYM_Expr,SYM_Expr> SYM_BreakDiv(SYM_Expr in);

bool operator<(SYM_Expr left,SYM_Expr right);

struct SYM_EvaluateResult{
  int result;
  Array<String> errors;

  // TODO: Replace with a big flag approach
  bool divByZero;
  bool nonConstantValue;

  bool Error(){return errors.size > 0;}
};

SYM_EvaluateResult SYM_ConstantEvaluate(SYM_Expr in,Arena* out);

int Compare(SYM_Expr left,SYM_Expr right);

// For an expression of the form a*b*c*d*e, returns the members individually and the literal seperatly.
struct SYM_MultTerms{
  Array<SYM_Expr> terms;
};

struct SYM_MultPartition{
  SYM_Expr literal;
  SYM_MultTerms mults;
};

struct SYM_Partition{
  SYM_Expr leftovers;
  bool exists;
};

int64_t Hash(SYM_Expr expr);

inline u64 Hash(SYM_MultTerms terms){
  u64 res = 0;
  for(int i = 0; i < terms.terms.size; i++){
    res += Hash(terms.terms[i]);
  }

  return res;
}

inline bool operator==(SYM_MultTerms left,SYM_MultTerms right){
  if(left.terms.size != right.terms.size){
    return false;
  }

  int size = left.terms.size;
  for(int i = 0; i < size; i++){
    if(!(left.terms[i] == right.terms[i])){
      return false;
    }
  }

  return true;
}

inline bool operator<(SYM_MultTerms left,SYM_MultTerms right){
  if(left.terms.size < right.terms.size){
    return true;
  }

  if(left.terms.size > right.terms.size){
    return false;
  }

  int size = left.terms.size;
  
  for(int i = 0; i < size; i++){
    if(left.terms[i] < right.terms[i]){
      if(left.terms[i] == right.terms[i]){
        continue;
      }

      return false;
    }
  }

  return true;
}  

static inline int Compare(SYM_MultTerms left,SYM_MultTerms right){
  if(left.terms.size > right.terms.size){
    return 1;
  }

  if(left.terms.size < right.terms.size){
    return -1;
  }

  int size = left.terms.size;
  
  for(int i = 0; i < size; i++){
    int com = Compare(left.terms[i],right.terms[i]);

    if(com == 0){
      continue;
    }

    return com;
  }

  return 0;
}

void TestSym2();
char*  SYM_DebugRepr(SYM_Expr expr);

int LiteralValue(SYM_Expr in);
bool IsLiteral(SYM_Expr in);

// ============================================================================
// Loop Linear Sum

struct LoopLinearSumTerm{
  String var;
  SYM_Expr term;
  SYM_Expr loopStart;
  SYM_Expr loopEnd;
};

// A Sum of expressions in the form term * X + term * Y + term * Z + ... + constant, where X,Y and Z are loop variables that range from a start expression to a end expression.
struct LoopLinearSum{
  // 0 is the innermost and size-1 the outermost loop
  Array<LoopLinearSumTerm> terms;
  SYM_Expr freeTerm;
};

// ======================================
// Building

LoopLinearSum* PushLoopLinearSumEmpty(Arena* out);
LoopLinearSum* PushLoopLinearSumFreeTerm(SYM_Expr term,Arena* out);
LoopLinearSum* PushLoopLinearSumSimpleVar(String loopVarName,SYM_Expr term,SYM_Expr start,SYM_Expr end,Arena* out);

// ======================================
// Manipulation

LoopLinearSum* Copy(LoopLinearSum* in,Arena* out);
LoopLinearSum* AddLoopLinearSum(LoopLinearSum* inner,LoopLinearSum* outer,Arena* out);
LoopLinearSum* RemoveLoop(LoopLinearSum* in,int index,Arena* out);
SYM_Expr TransformIntoSymbolicExpression(LoopLinearSum* sum,Arena* out);

SYM_Expr GetLoopLinearSumTotalSize(LoopLinearSum* in,Arena* out);

// ======================================
// Representation

void Print(LoopLinearSum* sum,bool printNewLine = false);
void Repr(StringBuilder* builder,LoopLinearSum* sum);
String PushRepr(LoopLinearSum* sum,Arena* out);
