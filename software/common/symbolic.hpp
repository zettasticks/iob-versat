#pragma once

#include "utils.hpp"

#if 0
A SYM_Expr is always normalized.
#endif

#if 0
- LEFT HERE - I'm thinking if there is a way of 
avoiding performing normalization all the time 
and having to call the function everytime we want to
do something. 

If we are already reusing nodes and such, I think that 
one thing that we could try is to add a flag to each node
that tells if the node has been normalized or not.

Every time we call a GetOrAllocate node we check the flag
and if false then we Normalize the node.

We also change the normalization from being a set of 
different step functions into a single step function.

The only thing that we actually care about is the "top"
level since that is the only point that can change and that 
can affect the lower levels.

ALSO, VERY IMPORTANT:

- When you "normalize" a node by this method you can technically
make it so the node becomes something else entirely. If we have the node
'b + a' and then we add a new node '-a' then we obviously are gonna
"normalize" into 'b'. In this case, what we could do, since we are gonna 
allocate the 'b + a + -a' node anyway is to store inside the node a 
bit indicating that the node was normalized as well as the pointer to the 
normalized node.

So the node 'b + a + -a' will contain a pointer to the node 'b' as well as
a bit (probably stored inside the pointer) that tells that the node was normalized.

If in the future the same operation is repeated, the code can just check the 
normalized bit, see that it is set and follow the pointer to the 'b' node.

That means that we only ever "normalize" once. And the entire point of normalization
is to handle things that the individual operators cannot handle alone (Adding a 
minus term to a sum is not guaranteed to cancel out with another term simply for the fact
that the terms might be below on the tree. "Ex: (+,-a,(+,b,(+,c,(+,d,a))))" the first -a 
is not detected by the operator+ function since the positive a is way below the 
sum tree.

THE OBJECTIVE IS SO THAT USER CODE NEVER HAS TO CALL A NORMALIZATION FUNCTION EVER.
We already know that we want to move divisions to the top and only have one total.
We already know that we want to apply distributivity so that at the very end we can
separate everything into groups cleanly.
No point forcing user to call Normalize everywhere. We already know what we want.
#endif

struct Arena;

void SYM_Init();

enum SYM_Type{
  // Order is important, it encodes the order of the way terms should be displayed (literals first then variables and so on)
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

#if 1
SYM_Expr operator+(SYM_Expr left,SYM_Expr right);
SYM_Expr operator-(SYM_Expr left,SYM_Expr right);
SYM_Expr operator-(SYM_Expr right);
SYM_Expr operator*(SYM_Expr left,SYM_Expr right);
SYM_Expr operator/(SYM_Expr left,SYM_Expr right);
#endif

SYM_Expr SYM_PosMax(SYM_Expr left,SYM_Expr right);
SYM_Expr SYM_Align(SYM_Expr left,SYM_Expr right);

SYM_Expr SYM_Variable(String name);
SYM_Expr SYM_Literal(int value);

SYM_Expr SYM_Replace(SYM_Expr expr,TrieMap<String,SYM_Expr>* replacements);
SYM_Expr SYM_Replace(SYM_Expr expr,TrieMap<SYM_Expr,SYM_Expr>* replacements);
SYM_Expr SYM_Replace(SYM_Expr expr,SYM_Expr toReplace,SYM_Expr replacement);

SYM_Expr SYM_Derivate(SYM_Expr expr,String var);

SYM_Expr SYM_Factor(SYM_Expr expr,SYM_Expr commonFactor);

bool SYM_IsZeroValue(SYM_Expr expr);
bool SYM_IsOneValue(SYM_Expr expr);

SYM_Expr SYM_Normalize(SYM_Expr in);
SYM_Expr SYM_Normalize2(SYM_Expr in);

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

SYM_Expr GetOrAllocateOp(SYM_Type type,SYM_Expr topIn,SYM_Expr bottomIn);
SYM_Expr GetOrAllocateFunc(String name,SYM_Expr first,SYM_Expr second);
SYM_Expr GetOrAllocateVariable(String name);
SYM_Expr GetOrAllocateLiteral(int input);

bool operator>(SYM_Expr left,SYM_Expr right);

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
