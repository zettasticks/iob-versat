#include "addressGen.hpp"

#include "embeddedData.hpp"
#include "globals.hpp"
#include "memory.hpp"
#include "symbolic.hpp"
#include "utils.hpp"
#include "utilsCore.hpp"
#include "versatSpecificationParser.hpp"
#include "CEmitter.hpp"

AddressAccess* Copy(AddressAccess* in,Arena* out){
  AddressAccess* res = PushStruct<AddressAccess>(out);

  res->external = Copy(in->external,out);
  res->internal = Copy(in->internal,out);

  return res;
}

void Repr(StringBuilder* builder,AddressAccess* access){
  TEMP_REGION(temp,builder->arena);

  builder->PushString("External:\n");
  Repr(builder,access->external);
  builder->PushString("\n\nInternal:\n");
  Repr(builder,access->internal);
  builder->PushString("\n");
}

String PushRepr(Arena* out,AddressAccess* access){
  TEMP_REGION(temp,out);
  
  // TODO: Performance
  auto b = StartString(temp);
  Repr(b,access);
  return EndString(out,b);
}

void Print(AddressAccess* access){
  TEMP_REGION(temp,nullptr);

  printf("External:\n");
  Print(access->external);
  printf("\nInternal:\n");
  Print(access->internal);
  printf("\n");
  if(!SYM_IsOneValue(access->dutyDivExpr)){
    printf("Duty:\n");
    SYM_Print(access->dutyDivExpr);
    printf("\n");
  }
}


SYM_Expr LoopMaximumValue(LoopLinearSumTerm term){
  SYM_Expr maxVal = term.loopEnd - SYM_1;

  return maxVal;
}

SYM_Expr EvaluateMaxLinearSumValue(LoopLinearSum* sum){
  SYM_Expr val = sum->freeTerm;

  for(int i = 0; i <  sum->terms.size; i++){
    LoopLinearSumTerm term  =  sum->terms[i];

    SYM_Expr maxLoopValue = LoopMaximumValue(term);
    SYM_Expr maxTermValue = term.term * maxLoopValue;
    
    val += maxTermValue;
  }

  return val;
}

AddressAccess* ConvertAccessTo1External(AddressAccess* access,Arena* out){
  TEMP_REGION(temp,out);

  SYM_Expr freeTerm = access->external->freeTerm;
  
  AddressAccess* result = Copy(access,out);
  result->external->freeTerm = SYM_0; // Pretend that the free term does not exist
  
  result->internal = Copy(result->external,out);
  result->external = PushLoopLinearSumEmpty(out);
  
  SYM_Expr maxLoopValue = EvaluateMaxLinearSumValue(result->internal);
  maxLoopValue += SYM_1;

  result->external = PushLoopLinearSumSimpleVar("x",SYM_1,SYM_0,maxLoopValue,out);
  result->external->freeTerm = freeTerm;
  result->dutyDivExpr = access->dutyDivExpr;
  
  return result;
}

AddressAccess* ConvertAccessTo2External(AddressAccess* access,int biggestLoopIndex,Arena* out){
  AddressAccess* result = Copy(access,out);

  LoopLinearSum* external = result->external;
  
  // We pretent that the free term does not exist and then shift the initial address of the final external expression by the free term.
  SYM_Expr freeTerm = external->freeTerm;
  external->freeTerm = SYM_0;
  
  int highestConstantIndex = biggestLoopIndex;
  SYM_Expr highestConstant = access->external->terms[biggestLoopIndex].term;

  LoopLinearSum oneSort = {};
  oneSort.terms = RemoveElement(external->terms,highestConstantIndex,out);
  oneSort.freeTerm = external->freeTerm;

  // We perform a loop "evaluation" here. 
  SYM_Expr val = EvaluateMaxLinearSumValue(&oneSort);
  SYM_Expr maxLoopValueExpr = val + SYM_1;

  // NOTE: The reason we need to align is because the increase in AXI_DATA_W forces data to be aligned inside the VUnits memories. The single loop does not care because we only need to access values individually, but the double loop cannot function because it assumes that the data is read and stored in a linear matter while in reality the data is stored in multiples of (AXI_DATA_W/DATA_W).
  // TODO: The easist solution for this would be to add logic to the hardware unit that allows it to 
  //       adjust the final offset. Its easier to do this in software for now.

  //maxLoopValueExpr = SYM_Align(maxLoopValueExpr,SYM_Var("VERSAT_DIFF_W"));
  
  result->internal = Copy(external,out);
  //result->internal->terms[highestConstantIndex].term = maxLoopValueExpr; //PushLiteral(out,maxLoopValue);
  
  LoopLinearSum* innermostExternal = PushLoopLinearSumSimpleVar("x",SYM_1,SYM_0,maxLoopValueExpr,out);

  // NOTE: Not sure about the end of this loop. We carry it directly but need to do further tests to make sure that this works fine.
  LoopLinearSum* outerMostExternal = PushLoopLinearSumSimpleVar("y",highestConstant,SYM_0,external->terms[highestConstantIndex].loopEnd,out);
  
  result->external = AddLoopLinearSum(innermostExternal,outerMostExternal,out);
  result->external->freeTerm = freeTerm;
  result->dutyDivExpr = access->dutyDivExpr;
  
  return result;
}

AddressAccess* ReplaceVariables(AddressAccess* in,TrieMap<String,SYM_Expr>* varReplace,Array<String> newInputVariableNames,Arena* out){
  AddressAccess* res = PushStruct<AddressAccess>(out);
  
  res->name = PushString(out,in->name);
  res->internal = ReplaceVariables(in->internal,varReplace,out);
  res->external = ReplaceVariables(in->external,varReplace,out);
  res->dutyDivExpr = SYM_Replace(in->dutyDivExpr,varReplace);
  res->loopVars = CopyArray(in->loopVars,out);
  
  return res;
}

SYM_Expr GetLoopHighestDecider(LoopLinearSumTerm* term){
  return term->term;
}

SYM_Expr GetLoopSize(LoopLinearSumTerm def,bool removeOne){
  SYM_Expr diff = def.loopEnd - def.loopStart;
    
  if(removeOne){
    diff = diff - SYM_1;
  }
    
  return diff;
};

String GetLoopSizeRepr(LoopLinearSumTerm def,Arena* out,bool removeOne = false){
  return SYM_Repr(GetLoopSize(def,removeOne),out);
};

CompiledAccess CompileAccess(LoopLinearSum* access,SYM_Expr dutyDiv,Arena* out){
  auto GenerateLoopExpressionPairSymbolic = [dutyDiv](Array<LoopLinearSumTerm> loops,
                                                      SYM_Expr expr,Arena* out) -> CompiledAccess{
    TEMP_REGION(temp,out);

    // TODO: Because we are adding an extra loop, there is a possibility of failure since the unit might not support enough loops to implement this. For the SingleLoop VS DoubleLoop the problem does not occur because we know that Singleloop is possible and DoubleLoop is easier on the address gen of the internal loop which is the limitting factor.
    //       Overall, we need to push this stuff upwards, so that we can simplify the code. It is easier to check and handle address gens that are to big before starting to emit stuff and writing to files.
    // NOTE: The only thing that we need to do is to add +1 to the amount of loops if a unit contains a duty expression. The failure is exactly the same, "address gen contains more loops than the unit is capable of handling"
    if(!SYM_IsOneValue(dutyDiv)){
      // In order to solve duty, we want to use two loops for the innermost loop. The first loop will have a period equal to the duty division expression and a duty of 1.
      // The second loop will have the expression of the original loop.
      Array<LoopLinearSumTerm> newLoops = PushArray<LoopLinearSumTerm>(temp,loops.size + 1);

      for(int i = 0; i < loops.size; i++){
        newLoops[i+1] = loops[i];
      }

      newLoops[0].var = "NONE";
      newLoops[0].term = SYM_1;
      newLoops[0].loopStart = SYM_0;
      newLoops[0].loopEnd = dutyDiv;
      
      loops = newLoops;
    }
    
    int loopSize = (loops.size + 1) / 2;
    Array<InternalMemoryAccess> result = PushArray<InternalMemoryAccess>(out,loopSize);
    
    for(int i = 0; i < loopSize; i++){
      LoopLinearSumTerm l0 = loops[i*2];

      InternalMemoryAccess& res = result[i];
      res = {};

      SYM_Expr derived = SYM_Derivate(expr,l0.var);
      SYM_Expr firstDerived = derived;
        
      res.periodExpression = GetLoopSize(l0);
      res.incrementExpression = firstDerived;
      
      SYM_Expr firstEndSym = l0.loopEnd;

      res.shiftWithoutRemovingIncrement = SYM_0; // By default
      if(i * 2 + 1 < loops.size){
        LoopLinearSumTerm l1 = loops[i*2 + 1];

        res.iterationExpression = GetLoopSize(l1);
        SYM_Expr derived = SYM_Derivate(expr,l1.var);

        res.shiftWithoutRemovingIncrement = derived;
        
        // We handle shifts very easily. We just remove the effects of all the previous period increments and then apply the shift.
        // That way we just have to calculate the derivative in relation to the shift, instead of calculating the change from a period term to a iteration term.
        // We need to subtract 1 because the period increment is only applied (period - 1) times.

        res.shiftExpression = -(firstDerived * (firstEndSym - SYM_1)) + derived;
      } else {
        res.iterationExpression = SYM_0;
        res.shiftExpression = SYM_0;
      }

      if(i == 0){
        if(SYM_IsOneValue(dutyDiv)){
          result[0].dutyExpression = result[0].periodExpression;
        } else {
          result[0].dutyExpression = result[0].periodExpression / dutyDiv; 
        }
      }
    }

    CompiledAccess com = {};
    com.internalAccess = result;
    com.dutyDivExpression = dutyDiv;
    
    return com;
  };

  SYM_Expr fullExpression = TransformIntoSymbolicExpression(access);
  CompiledAccess res = GenerateLoopExpressionPairSymbolic(access->terms,fullExpression,out);
  
  return res;
}

Array<Pair<String,SYM_Expr>> InstantiateIndividualAssignments(AddressAccess* access,int maxLoops,InstantiateOptions options,Arena* out){
  TEMP_REGION(temp,out);

  // Unpack options =============================================================
  AddressGenType type = options.type;
  int port = options.memPort;
  bool input = (options.dir == Direction_INPUT);

  // External access values =====================================================
  SYM_Expr totalTransferSize = SYM_Nil;
  SYM_Expr length = SYM_Nil;
  SYM_Expr amountMinusOne = SYM_Nil;
  SYM_Expr addrShift = SYM_Nil;
  
  CompiledAccess compiled = {};
  switch(type){
  case AddressGenType_READ:{
    // NOTE: Read type logic is mostly internal except the memory access part
    //       that is calculated differently.
    compiled = CompileAccess(access->internal,access->dutyDivExpr,temp);
    
    LoopLinearSum* external = access->external;
    int size = external->terms.size;

    Assert(size <= 2);

    LoopLinearSumTerm inner = external->terms[0];
    LoopLinearSumTerm outer = external->terms[external->terms.size - 1];

    SYM_Expr fullExpression = TransformIntoSymbolicExpression(external);
  
    length = GetLoopSize(inner);
    if(size == 1){
      totalTransferSize = length;
      amountMinusOne = SYM_0;
      addrShift = SYM_0;
    } else {
      addrShift = SYM_Derivate(fullExpression,outer.var);
    
      SYM_Expr outerLoopSize = GetLoopSize(outer);
      SYM_Expr all = GetLoopSize(inner) * outerLoopSize;

      totalTransferSize = all;
      amountMinusOne = GetLoopSize(outer,true);
    }
  } break;
  case AddressGenType_MEM:
  case AddressGenType_GEN:{
    compiled = CompileAccess(access->external,access->dutyDivExpr,temp);
  } break;
  }

  auto internal = compiled.internalAccess;
  int loops = internal.size;
  
  // Calculate free term 
  SYM_Expr freeTerm = access->external->freeTerm;
  if(SYM_Equal(freeTerm,SYM_0)){
    freeTerm = access->internal->freeTerm;
  } else {
    // NOTE: I do not think it is possible for both external and internal to have free terms.
    Assert(SYM_Equal(access->internal->freeTerm,SYM_0));
  }
  
  ArenaList<Pair<String,SYM_Expr>>* list = PushList<Pair<String,SYM_Expr>>(temp);

  if(type == AddressGenType_GEN || type == AddressGenType_READ){
    *list->PushElem() = {"start",freeTerm};
  }
  if(type == AddressGenType_MEM){
    if(port == 0){
      *list->PushElem() = {"startA",freeTerm};
    } else {
      *list->PushElem() = {"startB",freeTerm};
    }

    if(input){
      if(port == 0){
        *list->PushElem() = {"in0_wr",SYM_1};
      } else {
        *list->PushElem() = {"in1_wr",SYM_1};
      }
    } else {
      if(port == 0){
        *list->PushElem() = {"in0_wr",SYM_0};
      } else {
        *list->PushElem() = {"in1_wr",SYM_0};
      }
    }
  }

  if(type == AddressGenType_READ){
    *list->PushElem() = {"extra_delay",compiled.dutyDivExpression - SYM_1};
    *list->PushElem() = {"ext_addr",SYM_Var(options.extVarName)};
    *list->PushElem() = {"length",length};

    *list->PushElem() = {"amount_minus_one",amountMinusOne};
    *list->PushElem() = {"addr_shift",addrShift};

    *list->PushElem() = {"enabled",SYM_1};
    *list->PushElem() = {"pingPong",SYM_1};
  }

  if(type == AddressGenType_GEN || type == AddressGenType_READ){
    if(loops > 0){
      InternalMemoryAccess l = internal[0];
      *list->PushElem() = {"duty",l.dutyExpression};
      *list->PushElem() = {"per",l.periodExpression};
      *list->PushElem() = {"incr",l.incrementExpression};
      *list->PushElem() = {"iter",l.iterationExpression};
      *list->PushElem() = {"shift",l.shiftExpression};
    } else {
      *list->PushElem() = {"duty",SYM_0};
      *list->PushElem() = {"per",SYM_0};
      *list->PushElem() = {"incr",SYM_0};
      *list->PushElem() = {"iter",SYM_0};
      *list->PushElem() = {"shift",SYM_0};
    }
  }
  if(type == AddressGenType_MEM){
    if(loops > 0){
      InternalMemoryAccess l = internal[0];
      
      if(port == 0){
        *list->PushElem() = {"dutyA",l.dutyExpression};
        *list->PushElem() = {"perA",l.periodExpression};
        *list->PushElem() = {"incrA",l.incrementExpression};
        *list->PushElem() = {"iterA",l.iterationExpression};
        *list->PushElem() = {"shiftA",l.shiftExpression};
      } else {
        *list->PushElem() = {"dutyB",l.dutyExpression};
        *list->PushElem() = {"perB",l.periodExpression};
        *list->PushElem() = {"incrB",l.incrementExpression};
        *list->PushElem() = {"iterB",l.iterationExpression};
        *list->PushElem() = {"shiftB",l.shiftExpression};
      }    
    } else {
      // NOTE: Assume that the an empty loop is the same as a one iteration loop
      // TODO: This might not be the place to put this logic. We probably should transform a single element
      //       expression into a single loop before calling this function.
      if(port == 0){
        *list->PushElem() = {"dutyA",SYM_1};
        *list->PushElem() = {"perA",SYM_1};
        *list->PushElem() = {"incrA",SYM_1};
        *list->PushElem() = {"iterA",SYM_0};
        *list->PushElem() = {"shiftA",SYM_0};
      } else {
        *list->PushElem() = {"dutyB",SYM_1};
        *list->PushElem() = {"perB",SYM_1};
        *list->PushElem() = {"incrB",SYM_1};
        *list->PushElem() = {"iterB",SYM_0};
        *list->PushElem() = {"shiftB",SYM_0};
      }
    }
  }
    
  if(type == AddressGenType_GEN || type == AddressGenType_READ){
    if(loops > 1){
      InternalMemoryAccess l = internal[1]; 
      *list->PushElem() = {"per2",l.periodExpression};
      *list->PushElem() = {"incr2",l.incrementExpression};
      *list->PushElem() = {"iter2",l.iterationExpression};
      *list->PushElem() = {"shift2",l.shiftExpression};
    } else if(maxLoops > 1){
      *list->PushElem() = {"per2",SYM_0};
      *list->PushElem() = {"incr2",SYM_0};
      *list->PushElem() = {"iter2",SYM_0};
      *list->PushElem() = {"shift2",SYM_0};
    }
  }
  if(type == AddressGenType_MEM){
    if(loops > 1){
      InternalMemoryAccess l = internal[1]; 
      if(port == 0){
        *list->PushElem() = {"per2A",l.periodExpression};
        *list->PushElem() = {"incr2A",l.incrementExpression};
        *list->PushElem() = {"iter2A",l.iterationExpression};
        *list->PushElem() = {"shift2A",l.shiftExpression};
      } else {
        *list->PushElem() = {"per2B",l.periodExpression};
        *list->PushElem() = {"incr2B",l.incrementExpression};
        *list->PushElem() = {"iter2B",l.iterationExpression};
        *list->PushElem() = {"shift2B",l.shiftExpression};
      }
    } else if(maxLoops > 1){
      if(port == 0){
        *list->PushElem() = {"per2A",SYM_0};
        *list->PushElem() = {"incr2A",SYM_0};
        *list->PushElem() = {"iter2A",SYM_0};
        *list->PushElem() = {"shift2A",SYM_0};
      } else {
        *list->PushElem() = {"per2B",SYM_0};
        *list->PushElem() = {"incr2B",SYM_0};
        *list->PushElem() = {"iter2B",SYM_0};
        *list->PushElem() = {"shift2B",SYM_0};
      }
    }
  }

  // TODO: This is stupid. We can just put some loop logic in here. Do this when tests are stable and quick changes are easy to do.
  if(type == AddressGenType_GEN || type == AddressGenType_READ){
    if(loops > 2){
      InternalMemoryAccess l = internal[2]; 
      
      *list->PushElem() = {"per3",l.periodExpression};
      *list->PushElem() = {"incr3",l.incrementExpression};
      *list->PushElem() = {"iter3",l.iterationExpression};
      *list->PushElem() = {"shift3",l.shiftExpression};
    } else if(maxLoops > 2){
      *list->PushElem() = {"per3",SYM_0};
      *list->PushElem() = {"incr3",SYM_0};
      *list->PushElem() = {"iter3",SYM_0};
      *list->PushElem() = {"shift3",SYM_0};
    }
  }

  if(loops > maxLoops){
    // TODO: Proper error reporting requires us to lift the data up.
    printf("[ERROR] Address gen contains more loops than the unit is capable of handling\n");
    exit(-1);
  }

  return PushArray(out,list);
}

AddressAccess* CompileAddressGen(Env* env,Array<Token> inputs,Array<AddressGenForDef> loops,SYM_Expr addr,String content){
  Arena* out = globalPermanent;
  
  TEMP_REGION(temp,out);

  Array<String> asString = PushArray<String>(out,inputs.size);

  for(int i = 0; i <  inputs.size; i++){
    Token t = inputs[i];
    asString[i] = t.identifier;
  }

  // TODO: Issue a warning if a variable is declared but not used.
  // TODO: Better error reporting by allowing code to call the ReportError from the spec parser 
  bool anyError = false;
#if 0
  for(AddressGenForDef loop : loops){
    Opt<String> sameNameAsInput = Find(asString,loop.loopVariable.identifier);

    if(sameNameAsInput.has_value()){
      //ReportErrorGoodTokenExists(content,loop.loopVariable,sameNameAsInput.value(),"Loop variable","Overshadows input variable");
      anyError = true;
    }

#if 0
    {
      auto tokens = AccumTokens(loop.startSym,temp);
      for(NewToken tok : tokens){
        if(!Contains(inputs,tok)){
          printf("\t[Error] Loop expression variable '%.*s' does not appear inside input list (did you forget to declare it as input?)\n",UN(tok.identifier));
          anyError = true;
        }
      }
    }

    {
      auto tokens = AccumTokens(loop.endSym,temp);
      for(NewToken tok : tokens){
        if(!Contains(inputs,tok)){
          printf("\t[Error] Loop expression variable '%.*s' does not appear inside input list (did you forget to declare it as input?)\n",UN(tok.identifier));
          anyError = true;
        }
      }
    }
#endif
  }
#endif

  if(anyError){
    return nullptr;
  }
  
  auto loopVarBuilder = PushList<String>(temp);
  for(int i = 0; i < loops.size; i++){
    AddressGenForDef loop = loops[i];

    *loopVarBuilder->PushElem() = PushString(temp,loop.loopVariable.identifier);
  }
  Array<String> loopVars = PushArray(out,loopVarBuilder);

  Array<SYM_Expr> loopEnd = PushArray<SYM_Expr>(temp,loops.size);
  
  SYM_Expr symbolicExpr = addr;

  // Builds expression for the internal address which is basically just a multiplication of all the loops sizes
  SYM_Expr loopExpression = SYM_1;
  for(int i = 0; i <  loops.size; i++){
    AddressGenForDef loop = loops[i];
    // TODO: Handle parsing errors
    SYM_Expr start = env->SymbolicFromMathExpression(loop.startSym);
    loopEnd[i] = env->SymbolicFromMathExpression(loop.endSym);

    // NOTE: We transform loops so that they always start at zero:
    //       - for x a..b {x}  <=>  for x 0..b-a {(x+a)} 
    if(!Equal(start,SYM_0)){
      loopEnd[i] = loopEnd[i] - start;

      SYM_Expr loopVar = SYM_Var(loop.loopVariable.identifier);
      symbolicExpr = SYM_Replace(symbolicExpr,loopVar,loopVar + start);
    }

    SYM_Expr diff = loopEnd[i];
    loopExpression = loopExpression * diff;

  }
  SYM_Expr finalExpression = loopExpression;

  // Building expression for the external address
  // TODO: Handle parsing errors
  SYM_Expr normalized = symbolicExpr;

  Pair<SYM_Expr,SYM_Expr> pair = SYM_BreakDuty(normalized);

  SYM_Expr fullExpr = pair.first;
  SYM_Expr dutyDiv = pair.second;

  LoopLinearSum* expr = PushLoopLinearSumEmpty(temp);
  for(int i = 0; i < loopVars.size; i++){
    String var = loopVars[i];

    SYM_Expr term = SYM_Factor(fullExpr,SYM_Var(var));

    AddressGenForDef loop = loops[i];
    
    LoopLinearSum* sum = PushLoopLinearSumSimpleVar(loop.loopVariable.identifier,term,SYM_0,loopEnd[i],temp);
    expr = AddLoopLinearSum(sum,expr,temp);
  }
  
  // Extracts the constant term
  SYM_Expr toCalcConst = fullExpr;
  for(String str : loopVars){
    toCalcConst = SYM_Replace(toCalcConst,SYM_Var(str),SYM_0);
  }

  LoopLinearSum* freeTerm = PushLoopLinearSumFreeTerm(toCalcConst,temp);
      
  AddressAccess* result = PushStruct<AddressAccess>(out);
  result->internal = PushLoopLinearSumSimpleVar("x",SYM_1,SYM_0,finalExpression,out);
  result->external = AddLoopLinearSum(expr,freeTerm,out);
  result->dutyDivExpr = dutyDiv;
  result->loopVars = loopVars;
  
  return result;
};

static void EmitDebugAddressGenInfo(AddressAccess* access,CEmitter* c){
  TEMP_REGION(temp,c->castArena);

  auto builder = StartString(temp);
  Repr(builder,access->internal);
  String internalStr = EndString(temp,builder);

  builder = StartString(temp);
  Repr(builder,access->external);
  String externalStr = EndString(temp,builder);

  c->Comment("[DEBUG] Internal address");
  c->Comment(internalStr);
  c->Comment("[DEBUG] External address");
  c->Comment(externalStr);
}

CodeNode* EmitStatements(AccessAndType access,Arena* out,InstantiateOptions options){
  TEMP_REGION(temp,out);

  AddressAccess* initial = access.access;
  int maxLoops = access.inst.loopsSupported;

  auto EmitDoubleOrSingleLoopCode = [maxLoops,options,out](int loopIndex,AddressAccess* access) -> CodeNode*{
    TEMP_REGION(temp,out);

    AddressAccess* doubleLoop = ConvertAccessTo2External(access,loopIndex,temp);
    AddressAccess* singleLoop = ConvertAccessTo1External(access,temp);

    SYM_Expr doubleSize = GetLoopLinearSumTotalSize(doubleLoop->external);
    SYM_Expr singleSize = GetLoopLinearSumTotalSize(singleLoop->external);

    CodeNode* ifTrue = PushStruct<CodeNode>(out);
    CodeNode* ifFalse = PushStruct<CodeNode>(out);

    ifTrue->type = CodeNodeType_IF;
    ifTrue->expr = doubleSize > singleSize;
    
    ifFalse->type = CodeNodeType_IF;
    ifFalse->expr = singleSize >= doubleSize;

    ifTrue->next = ifFalse;

    Array<Pair<String,SYM_Expr>> paramsDouble = InstantiateIndividualAssignments(doubleLoop,maxLoops,options,temp);
    Array<Pair<String,SYM_Expr>> paramsSingle = InstantiateIndividualAssignments(singleLoop,maxLoops,options,temp);

    {
      CodeNode* chainStart = nullptr;
      CodeNode* ptr = nullptr;

      for(Pair<String,SYM_Expr> p : paramsDouble){
        CodeNode* newNode = PushStruct<CodeNode>(out);

        newNode->type = CodeNodeType_ASSIGN;
        newNode->name = PushString(out,p.first);
        newNode->expr = p.second;

        LL_Append(chainStart,ptr,next,newNode);
      }
      
      ifTrue->child = chainStart;
    }

    {
      CodeNode* chainStart = nullptr;
      CodeNode* ptr = nullptr;

      for(Pair<String,SYM_Expr> p : paramsSingle){
        CodeNode* newNode = PushStruct<CodeNode>(out);

        newNode->type = CodeNodeType_ASSIGN;
        newNode->name = PushString(out,p.first);
        newNode->expr = p.second;

        LL_Append(chainStart,ptr,next,newNode);
      }
      
      ifFalse->child = chainStart;
    }

    return ifTrue;
  };

  CodeNode* head = nullptr;
  CodeNode* ptr = nullptr;

  bool isExtMemType = (options.type == AddressGenType_READ);

  // Non reads are easier since we do not have to worry about memory access =====
  if(!isExtMemType){
    Array<Pair<String,SYM_Expr>> params = InstantiateIndividualAssignments(initial,maxLoops,options,temp);

    CodeNode* chainStart = nullptr;
    CodeNode* ptr = nullptr;

    for(Pair<String,SYM_Expr> p : params){
      CodeNode* newNode = PushStruct<CodeNode>(out);

      newNode->type = CodeNodeType_ASSIGN;
      newNode->name = PushString(out,p.first);
      newNode->expr = p.second;

      LL_Append(chainStart,ptr,next,newNode);
    }
      
    head = chainStart;
  }

  // NOTE: For reads we need to generate runtime code that decides on how many loops to read data from
  //       No point in reading thousands of bytes when we only care about the first and the last bytes.
  //       Might as well divide a single read into multiple reads for this case. The problem is that 
  //       we can only divide based on runtime info, meaning that we need to generate code for every
  //       single case and then generate a runtime if that selects the best.
  if(isExtMemType){
    int totalSize = initial->external->terms.size;

    // Generate top level if chains ===============================================
    Array<SYM_Expr> ifDecider = PushArray<SYM_Expr>(temp,totalSize);
    for(int i = 0; i < totalSize; i++){
      LoopLinearSumTerm term  =  initial->external->terms[i];
      ifDecider[i] = GetLoopHighestDecider(&term);
    }

    for(int i = 0; i < totalSize; i++){
      int topIndex = i;
    
      SYM_Expr topVar = ifDecider[topIndex];
      SYM_Expr ifCond = SYM_1;
      for(int ii = 0; ii < totalSize; ii++){
        if(ii == topIndex){
          continue;
        }
      
        SYM_Expr var = ifDecider[i];

        SYM_Expr cond = SYM_Nil;
        if(ii <= topIndex){
          cond = (topVar >= var);
        } else {
          cond = (topVar > var);
        }

        ifCond = ifCond && cond;
      }

      // TODO: If we add more information we could generate better if chains. Something like
      //       the range of certain values, if some values are always bigger than others and stuff like that.
      //       Do not know how much this would improve runtime. Regardless we always want to generate as little 
      //       code as possible otherwise runtime will suffer.

      //SYM_Print(ifCond);
      //printf("\n\n");
      SYM_Expr reduced = SYM_Reduce(ifCond);
      //SYM_Print(reduced);
      //printf("\n\n");
      
      SYM_EvaluateResult eval = SYM_ConstantEvaluate(reduced);
      if(!eval.Error() && eval.result == 0){
        // Skip, if(0)
      } else {
        CodeNode* emitted = EmitDoubleOrSingleLoopCode(topIndex,initial);
      
        CodeNode* expr = nullptr;
        if(!eval.Error() && eval.result){
          expr = emitted;
        } else {
          expr = PushStruct<CodeNode>(out);
          expr->type = CodeNodeType_IF;
          expr->expr = reduced;
          expr->child = emitted;
        }

        LL_Append(head,ptr,next,expr);
      }
    }

    auto GetAssignByName = [](CodeNode* top,String name) -> CodeNode*{
      for(CodeNode* ptr = top; ptr; ptr = ptr->next){
        if(ptr->type == CodeNodeType_ASSIGN && ptr->name == name){
          return ptr;
        }
      }
      return nullptr;
    };

    // Returns the head of the list after removing the node.
    auto RemoveNode = [](CodeNode* head,CodeNode* toRemove) -> CodeNode*{
      for(CodeNode *ptr = head,*previous = nullptr; ptr; previous = ptr,ptr = ptr->next){
        if(ptr == toRemove){
          if(!previous){
            CodeNode* newHead = toRemove->next;
            toRemove->next = nullptr;

            return newHead;
          } else {
            previous->next = toRemove->next;
            toRemove->next = nullptr;

            return head;
          }
        }
      }

      Assert(false && "ToRemove was not found inside the list");
      return nullptr;
    };

    auto AddNode = [](CodeNode* list,CodeNode* toAdd){
      for(CodeNode* ptr = list; ptr; ptr = ptr->next){
        if(!ptr->next){
          ptr->next = toAdd;
          break;
        }
      }
    };

    // Check if we can pull up any expression thats equal in all ifs ==============
    auto PullUp = [GetAssignByName,RemoveNode,AddNode](auto PullUp,CodeNode* top) -> void {
      TEMP_REGION(temp,nullptr);

      if(!top->child){
        return;
      }

      // Recurse first. 
      for(CodeNode* ptr = top->child; ptr; ptr = ptr->next){
        if(ptr->type == CodeNodeType_IF){
          PullUp(PullUp,ptr);
        }
      }

      auto allIfs = PushList<CodeNode*>(temp);

      for(CodeNode* ptr = top; ptr; ptr = ptr->next){
        if(ptr->type == CodeNodeType_IF){
          *allIfs->PushElem() = ptr;
        }
      }

      Array<CodeNode*> ifArray = PushArray(temp,allIfs);
      int size = ifArray.size;

      // Only makes sense to export when having more than 1 if statement
      if(ifArray.size < 2){
        return;
      }

      CodeNode* singleBranch = ifArray[0];

      Array<CodeNode*> sameAssignNodeBuffer = PushArray<CodeNode*>(temp,size);
      for(CodeNode* ptr = singleBranch->child; ptr; ){
        sameAssignNodeBuffer[0] = ptr;
      
        if(ptr->type == CodeNodeType_IF){
          ptr = ptr->next;
          continue;
        }

        String assignName = ptr->name;
        SYM_Expr assignExpr = ptr->expr;

        bool allEqual = true;
        for(int i = 1; i < size; i++){
          CodeNode* otherIf = ifArray[i];
          CodeNode* sameAssign = GetAssignByName(otherIf->child,assignName);

          if(sameAssign == nullptr){
            allEqual = false;
            break;
          }

          if(!SYM_Equal(assignExpr,sameAssign->expr)){
            allEqual = false;
            break;
          }

          sameAssignNodeBuffer[i] = sameAssign;
        }

        if(!allEqual){
          ptr = ptr->next;
          continue;
        }
      
        CodeNode* nextIterNode = ptr->next;

        // Remove all the nodes from the inner Ifs.
        for(int i = 0; i < size; i++){
          CodeNode* otherIf = ifArray[i];
          CodeNode* sameAssign = sameAssignNodeBuffer[i];

          otherIf->child = RemoveNode(otherIf->child,sameAssign);
        }

        // Add node to the outer if
        AddNode(singleBranch,ptr);

        ptr = nextIterNode;
      }
    };
  
    PullUp(PullUp,head);
  }
  
  return head;
}





















// nocheckin: Reorganize
AddressAccess* CompileAddressGen2(Array<AddressGenForDef2> loops,SYM_Expr addr){
  Arena* out = globalPermanent;
  TEMP_REGION(temp,out);
  
  auto loopVarBuilder = PushList<String>(temp);
  for(int i = 0; i < loops.size; i++){
    AddressGenForDef2 loop = loops[i];

    *loopVarBuilder->PushElem() = PushString(temp,loop.loopVariable);
  }
  Array<String> loopVars = PushArray(out,loopVarBuilder);

  Array<SYM_Expr> loopEnd = PushArray<SYM_Expr>(temp,loops.size);
  
  SYM_Expr symbolicExpr = addr;

  // Builds expression for the internal address which is basically just a multiplication of all the loops sizes
  SYM_Expr loopExpression = SYM_1;
  for(int i = 0; i < loops.size; i++){
    AddressGenForDef2 loop = loops[i];
    // TODO: Handle parsing errors
    SYM_Expr start = loop.startSym;
    loopEnd[i] = loop.endSym;

    // NOTE: We transform loops so that they always start at zero:
    //       - for x a..b {x}  <=>  for x 0..b-a {(x+a)} 
    if(!Equal(start,SYM_0)){
      loopEnd[i] = loopEnd[i] - start;

      SYM_Expr loopVar = SYM_Var(loop.loopVariable);
      symbolicExpr = SYM_Replace(symbolicExpr,loopVar,loopVar + start);
    }

    SYM_Expr diff = loopEnd[i];
    loopExpression = loopExpression * diff;

  }
  SYM_Expr finalExpression = loopExpression;

  // Building expression for the external address
  // TODO: Handle parsing errors
  SYM_Expr normalized = symbolicExpr;

  Pair<SYM_Expr,SYM_Expr> pair = SYM_BreakDuty(normalized);

  SYM_Expr fullExpr = pair.first;
  SYM_Expr dutyDiv = pair.second;

  LoopLinearSum* expr = PushLoopLinearSumEmpty(temp);
  for(int i = 0; i < loopVars.size; i++){
    String var = loopVars[i];

    SYM_Expr term = SYM_Factor(fullExpr,SYM_Var(var));

    AddressGenForDef2 loop = loops[i];
    
    LoopLinearSum* sum = PushLoopLinearSumSimpleVar(loop.loopVariable,term,SYM_0,loopEnd[i],temp);
    expr = AddLoopLinearSum(sum,expr,temp);
  }
  
  // Extracts the constant term
  SYM_Expr toCalcConst = fullExpr;
  for(String str : loopVars){
    toCalcConst = SYM_Replace(toCalcConst,SYM_Var(str),SYM_0);
  }

  LoopLinearSum* freeTerm = PushLoopLinearSumFreeTerm(toCalcConst,temp);
      
  AddressAccess* result = PushStruct<AddressAccess>(out);
  result->internal = PushLoopLinearSumSimpleVar("x",SYM_1,SYM_0,finalExpression,out);
  result->external = AddLoopLinearSum(expr,freeTerm,out);
  result->dutyDivExpr = dutyDiv;
  result->loopVars = loopVars;
  
  return result;
};

