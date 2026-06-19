#pragma once

#include "utils.hpp"

struct Arena;

void SYM_Init();

enum SYM_Type{
  SYM_Type_NIL,

  // Order is important, it encodes the order of the way terms should be displayed (literals first then variables and so on)
  SYM_Type_LITERAL,
  SYM_Type_VARIABLE,
  SYM_Type_SUM,
  SYM_Type_MUL,
  SYM_Type_DIV,
  SYM_Type_MOD,
  SYM_Type_COMP,
  SYM_Type_FUNC // We only care about 2 args functions so no need to support more than that.
};

// Negative inverts.
enum SYM_CompType{
  SYM_CompType_NIL = 0,
  SYM_CompType_EQ,
  SYM_CompType_GT,
  SYM_CompType_GE,
  SYM_CompType_LT,
  SYM_CompType_LE
};

struct SYM_Node;
struct SYM_Expr{
  SYM_Node* node;
};

struct SYM_Node{
  SYM_Type type;
  SYM_CompType compType;

  String name;

  union{
    int literal;

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

struct SYM_EvaluateResult{
  int result;

  bool divByZero : 1;
  bool nonConstantValue : 1;
  bool nilValue : 1;

  bool Error(){return (divByZero || nonConstantValue || nilValue);}
};

struct SYM_MultTerms{
  Array<SYM_Expr> terms;
};

// For an expression of the form a*b*c*d*e, returns the members individually and the literal seperatly.
struct SYM_MultPartition{
  SYM_Expr literal;
  SYM_MultTerms mults;
};

struct SYM_Partition{
  SYM_Expr leftovers;
  bool exists;
};

// ======================================
// Couple of useful constants and variables

extern SYM_Expr SYM_Nil;
extern SYM_Expr SYM_0;
extern SYM_Expr SYM_1;
extern SYM_Expr SYM_2;
extern SYM_Expr SYM_8;

extern SYM_Expr SYM_AddrW;
extern SYM_Expr SYM_AxiAddrW;
extern SYM_Expr SYM_AxiDataW;
extern SYM_Expr SYM_AxiStrobeW;
extern SYM_Expr SYM_LenW;
extern SYM_Expr SYM_DelayW;
extern SYM_Expr SYM_DataW;
extern SYM_Expr SYM_DataStrobeW;

// ======================================
// Basic expr and node interactions. 

inline SYM_Node* Negate(SYM_Node* ptr);
inline bool IsNegative(SYM_Node* ptr);
inline SYM_Node* Negate(SYM_Node* ptr){return (SYM_Node*) (((iptr) ptr) ^ 0x1);}
inline bool IsNegative(SYM_Node* ptr){return (((iptr) ptr) & 0x1);}
inline SYM_Node* GetPointer(SYM_Node* ptr){return (SYM_Node*) (((iptr) ptr) & ~0x1);}

inline SYM_Expr Negate(SYM_Expr expr){SYM_Expr res = {Negate(expr.node)}; return res;}
inline bool IsNegative(SYM_Expr expr){return IsNegative(expr.node);}
inline SYM_Node* GetPointer(SYM_Expr expr){return GetPointer(expr.node);}

inline bool SYM_Equal(SYM_Expr lhs,SYM_Expr rhs){
  return lhs.node == rhs.node;
}
inline bool Equal(SYM_Expr lhs,SYM_Expr rhs){
  return SYM_Equal(lhs,rhs);
}

// ======================================
// Public API.

SYM_Expr SYM_Var(String name);
SYM_Expr SYM_Lit(int value);

SYM_Expr operator+(SYM_Expr left,SYM_Expr right);
SYM_Expr& operator+=(SYM_Expr& left,SYM_Expr right);
SYM_Expr operator-(SYM_Expr left,SYM_Expr right);
SYM_Expr operator-(SYM_Expr right);
SYM_Expr operator*(SYM_Expr left,SYM_Expr right);
SYM_Expr operator/(SYM_Expr left,SYM_Expr right);
SYM_Expr operator%(SYM_Expr left,SYM_Expr right);

SYM_Expr operator>(SYM_Expr left,SYM_Expr right);
SYM_Expr operator>=(SYM_Expr left,SYM_Expr right);
SYM_Expr operator<(SYM_Expr left,SYM_Expr right);
SYM_Expr operator<=(SYM_Expr left,SYM_Expr right);
SYM_Expr operator==(SYM_Expr left,SYM_Expr right);
SYM_Expr operator!=(SYM_Expr left,SYM_Expr right);

SYM_Expr SYM_Max(SYM_Expr left,SYM_Expr right);
SYM_Expr SYM_PosMax(SYM_Expr left,SYM_Expr right); // Assumes final value is positive always.
SYM_Expr SYM_Align(SYM_Expr left,SYM_Expr right);
SYM_Expr SYM_FloorDiv(SYM_Expr top,SYM_Expr bottom);

SYM_Expr SYM_Replace(SYM_Expr expr,TrieMap<String,SYM_Expr>* replacements);
SYM_Expr SYM_Replace(SYM_Expr expr,TrieMap<SYM_Expr,SYM_Expr>* replacements);
SYM_Expr SYM_Replace(SYM_Expr expr,SYM_Expr toReplace,SYM_Expr replacement);

SYM_Expr SYM_Derivate(SYM_Expr expr,String var);
SYM_Expr SYM_Normalize(SYM_Expr in);

// ======================================
// Manipulation and info retrieval

bool SYM_IsNil(SYM_Expr expr);
bool SYM_IsZeroValue(SYM_Expr expr);
bool SYM_IsOneValue(SYM_Expr expr);

Pair<SYM_Expr,SYM_Expr> SYM_BreakDiv(SYM_Expr in);
SYM_Expr SYM_Factor(SYM_Expr expr,SYM_Expr commonFactor);
Array<String> SYM_GetAllVariables(SYM_Expr top,Arena* out);

// ======================================
// Repr

void SYM_Print(SYM_Expr expr);
void SYM_Repr(StringBuilder* b,SYM_Expr expr);
String SYM_ReprHier(SYM_Expr expr,Arena* out);
String SYM_Repr(SYM_Expr expr,Arena* out);

// ======================================
// Evaluation

SYM_EvaluateResult SYM_ConstantEvaluate(SYM_Expr in);

// ======================================
// Implementation helpers

SYM_Expr GetOrAllocateOp(SYM_Type type,SYM_Expr topIn,SYM_Expr bottomIn);
SYM_Expr GetOrAllocateFunc(String name,SYM_Expr first,SYM_Expr second);
SYM_Expr GetOrAllocateVariable(String name);
SYM_Expr GetOrAllocateLiteral(int input);

int LiteralValue(SYM_Expr in);
bool IsLiteral(SYM_Expr in);

SYM_Expr Abs(SYM_Expr in);
int Compare(SYM_Expr left,SYM_Expr right);
bool LessThan(SYM_Expr left,SYM_Expr right);
bool GreaterThan(SYM_Expr left,SYM_Expr right);

u64 Hash(SYM_Expr expr);
u64 Hash(SYM_MultTerms terms);
bool Equal(SYM_MultTerms left,SYM_MultTerms right);
bool operator<(SYM_MultTerms left,SYM_MultTerms right);
int Compare(SYM_MultTerms left,SYM_MultTerms right);

void SYM_Test();
char* SYM_DebugRepr(SYM_Expr expr);

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

LoopLinearSum* ReplaceVariables(LoopLinearSum* in,TrieMap<String,SYM_Expr>* varReplace,Arena* out);

// ======================================
// Representation

void Print(LoopLinearSum* sum,bool printNewLine = false);
void Repr(StringBuilder* builder,LoopLinearSum* sum);
String PushRepr(LoopLinearSum* sum,Arena* out);
