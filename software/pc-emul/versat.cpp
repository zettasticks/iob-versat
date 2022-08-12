#include "versatPrivate.hpp"

#include <new>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include <array>
#include <functional>
#include <queue>
#include <unordered_map>

#include <printf.h>

#include <set>
#include <utility>

#include "templateEngine.hpp"

#define IMPLEMENT_VERILOG_UNITS
#include "verilogWrapper.inc"

#define DELAY_BIT_SIZE 8

static int versat_base;

#define TAG_TEMPORARY_MARK 1
#define TAG_PERMANENT_MARK 2

// Implementations of units that versat needs to know about explicitly

#define MAX_DELAY 128

typedef std::function<void(FUInstance*)> AcceleratorInstancesVisitor;
static void VisitAcceleratorInstances(Accelerator* accel,AcceleratorInstancesVisitor func);
static void AcceleratorRunStart(Accelerator* accel);

bool operator<(const StaticInfo& left,const StaticInfo& right){
   bool res = std::tie(left.module,left.name,left.wires,left.ptr) < std::tie(right.module,right.name,right.wires,right.ptr);

   return res;
}

static int zeros[100] = {};

static int* DefaultInitFunction(FUInstance* inst){
   inst->done = true;
   return nullptr;
}

static FUDeclaration* RegisterCircuitInput(Versat* versat){
   FUDeclaration decl = {};

   decl.name = MakeSizedString("CircuitInput");
   decl.nOutputs = 99;
   decl.nInputs = 1;  // Used for templating circuit
   decl.latencies = zeros;
   decl.inputDelays = zeros;
   decl.initializeFunction = DefaultInitFunction;
   decl.delayType = DelayType::DELAY_TYPE_SOURCE_DELAY;
   decl.type = FUDeclaration::SPECIAL;

   return RegisterFU(versat,decl);
}

static FUDeclaration* RegisterCircuitOutput(Versat* versat){
   FUDeclaration decl = {};

   decl.name = MakeSizedString("CircuitOutput");
   decl.nInputs = 99;
   decl.nOutputs = 99; // Used for templating circuit
   decl.latencies = zeros;
   decl.inputDelays = zeros;
   decl.initializeFunction = DefaultInitFunction;
   decl.delayType = DelayType::DELAY_TYPE_SINK_DELAY;
   decl.type = FUDeclaration::SPECIAL;

   return RegisterFU(versat,decl);
}

static int Visit(FUInstance*** ordering,FUInstance* inst){
   if(inst->tag == TAG_PERMANENT_MARK){
      return 0;
   }
   if(inst->tag == TAG_TEMPORARY_MARK){
      Assert(0);
   }

   if(inst->tempData->nodeType == GraphComputedData::TAG_SINK ||
     (inst->tempData->nodeType == GraphComputedData::TAG_SOURCE_AND_SINK && CHECK_DELAY(inst,DelayType::DELAY_TYPE_SINK_DELAY))){
      return 0;
   }

   inst->tag = TAG_TEMPORARY_MARK;

   int count = 0;
   if(inst->tempData->nodeType == GraphComputedData::TAG_COMPUTE){
      for(int i = 0; i < inst->tempData->numberInputs; i++){
         count += Visit(ordering,inst->tempData->inputs[i].inst.inst);
      }
   }

   inst->tag = TAG_PERMANENT_MARK;

   if(inst->tempData->nodeType == GraphComputedData::TAG_COMPUTE){
      *(*ordering) = inst;
      (*ordering) += 1;
      count += 1;
   }

   return count;
}

static void CalculateDAGOrdering(Accelerator* accel){
   Assert(accel->locked == Accelerator::Locked::GRAPH);

   ZeroOutAlloc(&accel->order.instances,accel->instances.Size());

   accel->order.numberComputeUnits = 0;
   accel->order.numberSinks = 0;
   accel->order.numberSources = 0;
   for(FUInstance* inst : accel->instances){
      inst->tag = 0;
   }

   FUInstance** sourceUnits = accel->order.instances.ptr;
   accel->order.sources = sourceUnits;
   // Add source units, guaranteed to come first
   for(FUInstance* inst : accel->instances){
      if(inst->tempData->nodeType == GraphComputedData::TAG_SOURCE || (inst->tempData->nodeType == GraphComputedData::TAG_SOURCE_AND_SINK && CHECK_DELAY(inst,DelayType::DELAY_TYPE_SOURCE_DELAY))){
         *(sourceUnits++) = inst;
         accel->order.numberSources += 1;
         inst->tag = TAG_PERMANENT_MARK;
      }
   }

   // Add compute units
   FUInstance** computeUnits = sourceUnits;
   accel->order.computeUnits = computeUnits;
   for(FUInstance* inst : accel->instances){
      if(inst->tempData->nodeType == GraphComputedData::TAG_UNCONNECTED){
         *(computeUnits++) = inst;
         accel->order.numberComputeUnits += 1;
         inst->tag = TAG_PERMANENT_MARK;
      } else if(inst->tag == 0 && inst->tempData->nodeType == GraphComputedData::TAG_COMPUTE){
         accel->order.numberComputeUnits += Visit(&computeUnits,inst);
      }
   }

   // Add sink units
   FUInstance** sinkUnits = computeUnits;
   accel->order.sinks = sinkUnits;
   for(FUInstance* inst : accel->instances){
      if(inst->tempData->nodeType == GraphComputedData::TAG_SINK || (inst->tempData->nodeType == GraphComputedData::TAG_SOURCE_AND_SINK && CHECK_DELAY(inst,DelayType::DELAY_TYPE_SINK_DELAY))){
         *(sinkUnits++) = inst;
         accel->order.numberSinks += 1;
         Assert(inst->tag == 0);
         inst->tag = TAG_PERMANENT_MARK;
      }
   }

   for(FUInstance* inst : accel->instances){
      Assert(inst->tag == TAG_PERMANENT_MARK);
   }

   Assert(accel->order.numberSources + accel->order.numberComputeUnits + accel->order.numberSinks == accel->instances.Size());
}

void LockAccelerator(Accelerator* accel,Accelerator::Locked level,bool freeMemory){
   if(level == Accelerator::Locked::FREE){
      if(freeMemory){
         switch(accel->locked){ // Fallthrough switch
            case Accelerator::Locked::FIXED:{
               Free(&accel->versatData);
            }
            case Accelerator::Locked::ORDERED:{
               Free(&accel->order.instances);
            }
            case Accelerator::Locked::GRAPH:{
               Free(&accel->graphData);
            }
            case Accelerator::Locked::FREE:{
            }
         }
      }

      accel->locked = level;
      return;
   }

   if(accel->locked >= level){
      return;
   }

   if(level >= Accelerator::Locked::GRAPH){
      CalculateGraphData(accel);
      accel->locked = Accelerator::Locked::GRAPH;
   }
   if(level >= Accelerator::Locked::ORDERED){
      CalculateDAGOrdering(accel);
      accel->locked = Accelerator::Locked::ORDERED;
   }
   if(level >= Accelerator::Locked::FIXED){
      CalculateVersatData(accel);
      accel->locked = Accelerator::Locked::FIXED;
   }
}

typedef std::unordered_map<FUInstance*,FUInstance*> InstanceMap;

int* UnaryNot(FUInstance* inst){
    static uint out;
    out = ~GetInputValue(inst,0);
    inst->done = true;
    return (int*) &out;
}

int* BinaryXOR(FUInstance* inst){
    static uint out;
    out = GetInputValue(inst,0) ^ GetInputValue(inst,1);
    inst->done = true;
    return (int*) &out;
}

int* BinaryADD(FUInstance* inst){
    static uint out;
    out = GetInputValue(inst,0) + GetInputValue(inst,1);
    inst->done = true;
    return (int*) &out;
}
int* BinaryAND(FUInstance* inst){
    static uint out;
    out = GetInputValue(inst,0) & GetInputValue(inst,1);
    inst->done = true;
    return (int*) &out;
}
int* BinaryOR(FUInstance* inst){
    static uint out;
    out = GetInputValue(inst,0) | GetInputValue(inst,1);
    inst->done = true;
    return (int*) &out;
}
int* BinaryRHR(FUInstance* inst){
    static uint out;
    uint value = GetInputValue(inst,0);
    uint shift = GetInputValue(inst,1);
    out = (value >> shift) | (value << (32 - shift));
    inst->done = true;
    return (int*) &out;
}
int* BinaryRHL(FUInstance* inst){
    static uint out;
    uint value = GetInputValue(inst,0);
    uint shift = GetInputValue(inst,1);
    out = (value << shift) | (value >> (32 - shift));
    inst->done = true;
    return (int*) &out;
}
int* BinarySHR(FUInstance* inst){
    static uint out;
    uint value = GetInputValue(inst,0);
    uint shift = GetInputValue(inst,1);
    out = (value >> shift);
    inst->done = true;
    return (int*) &out;
}
int* BinarySHL(FUInstance* inst){
    static uint out;
    uint value = GetInputValue(inst,0);
    uint shift = GetInputValue(inst,1);
    out = (value << shift);
    inst->done = true;
    return (int*) &out;
}

void RegisterOperators(Versat* versat){
   const char* unary[] = {"NOT"};
   FUFunction unaryF[] = {UnaryNot};
   const char* binary[] = {"XOR","ADD","AND","OR","RHR","SHR","RHL","SHL"};
   FUFunction binaryF[] = {BinaryXOR,BinaryADD,BinaryAND,BinaryOR,BinaryRHR,BinarySHR,BinaryRHL,BinarySHL};
   const char* binaryOperation[] = {"^","+","&","|","<<",">>","<<",">>"};

   FUDeclaration decl = {};
   decl.nOutputs = 1;
   decl.nInputs = 1;
   decl.inputDelays = zeros;
   decl.latencies = zeros;
   decl.isOperation = true;
   for(unsigned int i = 0; i < ARRAY_SIZE(unary); i++){
      decl.name = MakeSizedString(unary[i]);
      decl.updateFunction = unaryF[i];
      decl.operation = "~";
      RegisterFU(versat,decl);
   }

   decl.nInputs = 2;
   for(unsigned int i = 0; i < ARRAY_SIZE(binary); i++){
      decl.name = MakeSizedString(binary[i]);
      decl.updateFunction = binaryF[i];
      decl.operation = binaryOperation[i];
      RegisterFU(versat,decl);
   }
}

Versat* InitVersat(int base,int numberConfigurations){
   static Versat versatInst = {};
   static bool doneOnce = false;

   Assert(!doneOnce); // For now, only allow one Versat instance
   doneOnce = true;

   Versat* versat = &versatInst;

   RegisterTypes();

   versat->numberConfigurations = numberConfigurations;
   versat->base = base;
   versat_base = base;

   InitArena(&versat->temp,Megabyte(16));
   InitArena(&versat->permanent,Megabyte(64));

   FUDeclaration nullDeclaration = {};
   nullDeclaration.latencies = zeros;
   nullDeclaration.inputDelays = zeros;
   RegisterFU(versat,nullDeclaration);

   RegisterAllVerilogUnits(versat);

   versat->delay = GetTypeByName(versat,MakeSizedString("Delay"));
   versat->pipelineRegister = GetTypeByName(versat,MakeSizedString("PipelineRegister"));

   versat->input = RegisterCircuitInput(versat);
   versat->output = RegisterCircuitOutput(versat);

   RegisterOperators(versat);

   return versat;
}

void Free(Versat* versat){
   for(Accelerator* accel : versat->accelerators){
      LockAccelerator(accel,Accelerator::FREE,true);

      Free(&accel->configAlloc);
      Free(&accel->stateAlloc);
      Free(&accel->delayAlloc);
      Free(&accel->staticAlloc);

      accel->instances.Clear(true);
      accel->edges.Clear(true);
      accel->inputInstancePointers.Clear(true);

      accel->staticInfo.~vector();
   }

   // Missing clear FUInstances
   versat->accelerators.Clear(true);
   versat->declarations.Clear(true);

   Free(&versat->temp);
   Free(&versat->permanent);

   FreeTypes();
}

void ParseCommandLineOptions(Versat* versat,int argc,const char** argv){
   #if 0
   for(int i = 0; i < argc; i++){
      printf("Arg %d: %s\n",i,argv[i]);
   }
   #endif

   for(int i = 1; i < argc; i++){
      versat->includeDirs.push_back(argv[i]);
   }
}

static int AccessMemory(FUInstance* instance,int address, int value, int write){
   int res = instance->declaration->memAccessFunction(instance,address,value,write);

   return res;
}

void VersatUnitWrite(FUInstance* instance,int address, int value){
   if(instance->declaration->type == FUDeclaration::COMPOSITE){
      int offset = 0;

      for(FUInstance* inst : instance->compositeAccel->instances){
         if(!inst->declaration->isMemoryMapped){
            continue;
         }

         int mappedWords = 1 << inst->declaration->memoryMapBits;
         if(mappedWords){
            if(address >= offset && address <= offset + mappedWords){
               VersatUnitWrite(inst,address - offset,value);
            } else {
               offset += mappedWords;
            }
         }
      }
   } else {
      AccessMemory(instance,address,value,1);
   }
}

int VersatUnitRead(FUInstance* instance,int address){
   int res = AccessMemory(instance,address,0,0);
   return res;
}

FUDeclaration* GetTypeByName(Versat* versat,SizedString name){
   for(FUDeclaration* decl : versat->declarations){
      if(CompareString(decl->name,name)){
         return decl;
      }
   }

   Assert(0);
   return nullptr;
}

FUInstance* GetInstanceByName_(Accelerator* circuit,int argc, ...){
   char buffer[256];

   va_list args;
   va_start(args,argc);

   Accelerator* ptr = circuit;
   FUInstance* res = nullptr;

   for (int i = 0; i < argc; i++){
      char* str = va_arg(args, char*);
      int found = 0;

      int arguments = parse_printf_format(str,0,nullptr);

      if(arguments){
         vsprintf(buffer,str,args);
         i += arguments;
         for(int ii = 0; ii < arguments; ii++){
            va_arg(args, int); // Need to consume something
         }
      } else {
         strcpy(buffer,str);
      }

      for(FUInstance* inst : ptr->instances){
         if(strcmp(inst->name.str,buffer) == 0){
            res = inst;
            found = 1;

            break;
         }
      }

      Assert(found);

      if(res->declaration->type == FUDeclaration::COMPOSITE){
         ptr = res->compositeAccel;
      }
   }

   va_end(args);

   res->namedAccess = true;

   return res;
}

FUDeclaration* RegisterFU(Versat* versat,FUDeclaration decl){
   FUDeclaration* type = versat->declarations.Alloc();
   *type = decl;

   if(decl.nInputs){
      Assert(decl.inputDelays);
   }

   Assert(decl.latencies);

   return type;
}

// Uses static allocated memory. Intended for use by OutputGraphDotFile
static char* FormatNameToOutput(FUInstance* inst){
   #define PRINT_ID 0
   #define HIERARCHICAL_NAME 0
   #define PRINT_DELAY 1

   static char buffer[1024];

   #if HIERARCHICAL_NAME == 1
      char* name = GetHierarchyNameRepr(inst->name);
   #else
      char* name = inst->name.str;
   #endif

   char* ptr = buffer;
   ptr += sprintf(ptr,"%s",name);

   #if PRINT_ID == 1
   ptr += sprintf(ptr,"_%d",inst->id);
   #endif

   #if PRINT_DELAY == 1
   if(CompareString(inst->declaration->name,"Delay") && inst->config){
      ptr += sprintf(ptr,"_%d",inst->config[0]);
   } else {
      ptr += sprintf(ptr,"_%d",inst->baseDelay);
   }
   #endif

   #undef PRINT_ID
   #undef HIERARCHICAL_NAME
   #undef PRINT_DELAY

   return buffer;
}

static void OutputGraphDotFile_(Accelerator* accel,bool collapseSameEdges,FILE* outputFile){
   LockAccelerator(accel,Accelerator::Locked::GRAPH);

   fprintf(outputFile,"digraph accel {\n\tnode [fontcolor=white,style=filled,color=\"160,60,176\"];\n");
   for(FUInstance* inst : accel->instances){
      char* name = FormatNameToOutput(inst);

      fprintf(outputFile,"\t%s;\n",name);
   }

   std::set<std::pair<FUInstance*,FUInstance*>> sameEdgeCounter;

   for(Edge* edge : accel->edges){
      if(collapseSameEdges){
         std::pair<FUInstance*,FUInstance*> key{edge->units[0].inst,edge->units[1].inst};

         if(sameEdgeCounter.count(key) == 1){
            continue;
         }

         sameEdgeCounter.insert(key);
      }

      fprintf(outputFile,"\t%s -> ",FormatNameToOutput(edge->units[0].inst));
      fprintf(outputFile,"%s",FormatNameToOutput(edge->units[1].inst));

      #if 1
      FUInstance* outputInst = edge->units[0].inst;
      int delay = 0;
      for(int i = 0; i < outputInst->tempData->numberOutputs; i++){
         if(edge->units[1].inst == outputInst->tempData->outputs[i].inst.inst && edge->units[1].port == outputInst->tempData->outputs[i].inst.port){
            delay = outputInst->tempData->outputs[i].delay;
            break;
         }
      }

      fprintf(outputFile,"[label=\"%d\"]",delay);
      //fprintf(outputFile,"[label=\"[%d:%d;%d:%d]\"]",outputInst->declaration->latencies[0],delay,edge->units[1].inst->declaration->inputDelays[edge->units[1].port],edge->delay);
      #endif

      fprintf(outputFile,";\n");
   }

   fprintf(outputFile,"}\n");
}

void OutputGraphDotFile(Accelerator* accel,bool collapseSameEdges,const char* filenameFormat,...){
   char buffer[1024];

   va_list args;
   va_start(args,filenameFormat);

   vsprintf(buffer,filenameFormat,args);

   FILE* file = fopen(buffer,"w");
   OutputGraphDotFile_(accel,collapseSameEdges,file);
   fclose(file);

   va_end(args);
}

SubgraphData SubGraphAroundInstance(Versat* versat,Accelerator* accel,FUInstance* instance,int layers){
   Accelerator* newAccel = CreateAccelerator(versat);
   newAccel->type = Accelerator::CIRCUIT;

   std::set<FUInstance*> subgraphUnits;
   std::set<FUInstance*> tempSubgraphUnits;
   subgraphUnits.insert(instance);
   for(int i = 0; i < layers; i++){
      for(FUInstance* inst : subgraphUnits){
         for(Edge* edge : accel->edges){
            for(int i = 0; i < 2; i++){
               if(edge->units[i].inst == inst){
                  tempSubgraphUnits.insert(edge->units[1 - i].inst);
                  break;
               }
            }
         }
      }

      subgraphUnits.insert(tempSubgraphUnits.begin(),tempSubgraphUnits.end());
      tempSubgraphUnits.clear();
   }

   InstanceMap map;
   for(FUInstance* nonMapped : subgraphUnits){
      FUInstance* mapped = CreateFUInstance(newAccel,nonMapped->declaration,MakeSizedString(nonMapped->name.str));
      map.insert({nonMapped,mapped});
   }

   for(Edge* edge : accel->edges){
      auto iter0 = map.find(edge->units[0].inst);
      auto iter1 = map.find(edge->units[1].inst);

      if(iter0 == map.end() || iter1 == map.end()){
         continue;
      }

      Edge* newEdge = newAccel->edges.Alloc();

      *newEdge = *edge;
      newEdge->units[0].inst = iter0->second;
      newEdge->units[1].inst = iter1->second;
   }

   SubgraphData data = {};

   data.accel = newAccel;
   data.instanceMapped = map.at(instance);

   return data;
}

Accelerator* CreateAccelerator(Versat* versat){
   Accelerator* accel = versat->accelerators.Alloc();
   accel->versat = versat;

   ZeroOutAlloc(&accel->staticAlloc,1024); // Do not want to deal with reallocations for static data, for now

   return accel;
}

static Accelerator* InstantiateSubAccelerator(Versat* versat,Accelerator* topLevel,FUDeclaration* subaccelType,Accelerator* accel,HierarchyName* parent,int* config,int* state,int* memMapped,int* delay);

static FUInstance* CreateSubFUInstance(Accelerator* accel,Accelerator* topLevel,FUDeclaration* declaration,int* config,int* state,int* memMapped,int* delay){
   FUInstance* ptr = accel->instances.Alloc();

   ptr->id = accel->entityId++;
   ptr->accel = accel;
   ptr->declaration = declaration;

   sprintf(ptr->name.str,"%.*s",UNPACK_SS(declaration->name));

   if(declaration->nConfigs){
      ptr->config = config;
   }
   if(declaration->nStates){
      ptr->state = state;
   }
   if(declaration->isMemoryMapped){
      ptr->memMapped = memMapped;
   }
   if(declaration->nDelays){
      ptr->delay = delay;
   }

   if(declaration->nOutputs){
      ptr->outputs = (int*) calloc(declaration->nOutputs,sizeof(int));
      ptr->storedOutputs = (int*) calloc(declaration->nOutputs,sizeof(int));
   }

   if(declaration->extraDataSize){
      ptr->extraData = calloc(declaration->extraDataSize,sizeof(char));
   }

   if(declaration->type == FUDeclaration::COMPOSITE){
      ptr->compositeAccel = InstantiateSubAccelerator(accel->versat,topLevel,declaration,declaration->circuit,&ptr->name,config,state,memMapped,delay);
   }

   return ptr;
}

static Accelerator* InstantiateSubAccelerator(Versat* versat,Accelerator* topLevel,FUDeclaration* subaccelType,Accelerator* accel,HierarchyName* parent,int* config,int* state,int* memMapped,int* delay){
   InstanceMap map = {};
   Accelerator* newAccel = CreateAccelerator(versat);

   // Flat copy of instances
   for(FUInstance* inst : accel->instances){

      // Check if static
      // Check if static unit already exists
      // If exists, simply pass the static unit config pointer as the config pointer (so that the lower units can be set to the correct position)
      int* configPtr = config;

      #if 1
      StaticInfo* saved = nullptr;
      if(inst->isStatic){
         int* foundConfig = nullptr;
         for(StaticInfo& info : topLevel->staticInfo){
            if(info.module == subaccelType && CompareString(info.name,inst->name.str)){
               foundConfig = info.ptr;
               saved = &info;
            }
         }

         if(foundConfig){
            configPtr = foundConfig;
         } else { // Have to allocate and reallocate static configuration
            configPtr = &topLevel->staticAlloc.ptr[topLevel->staticAlloc.size];

            topLevel->staticAlloc.size += inst->declaration->nConfigs;
            Assert(topLevel->staticAlloc.size < topLevel->staticAlloc.reserved);

            StaticInfo info = {};
            info.module = subaccelType;
            info.name = MakeSizedString(inst->name.str);
            info.ptr = configPtr;

            topLevel->staticInfo.push_back(info);

            saved = &topLevel->staticInfo.back();
         }
      }
      #endif

      FUInstance* newInst = CreateSubFUInstance(newAccel,topLevel,inst->declaration,configPtr,state,memMapped,delay);
      newInst->name.parent = parent;
      FixedStringCpy(newInst->name.str,MakeSizedString(inst->name.str));

      state += inst->declaration->nStates;

      if(inst->isStatic){
         saved->wires = newInst->declaration->configWires;
         saved->nConfigs = newInst->declaration->nConfigs;
      } else {
         config += inst->declaration->nConfigs;
      }

      delay += inst->declaration->nDelays;

      if(inst->declaration->isMemoryMapped){
         memMapped = (int*) (((char*) memMapped) + (1 << inst->declaration->memoryMapBits) * 4);
      }

      newInst->baseDelay = inst->baseDelay;

      if(newInst->declaration->initializeFunction){
         newInst->declaration->initializeFunction(newInst);
      }

      if(inst->config){
         memcpy(newInst->config,inst->config,inst->declaration->nConfigs * sizeof(int));
      }

      if(inst->declaration->isMemoryMapped && inst->memMapped){
         for(int i = 0; i < (1 << inst->declaration->memoryMapBits); i++){
            VersatUnitWrite(newInst,i,inst->memMapped[i]);
         }
      }

      map.insert({inst,newInst});
   }

   // Flat copy of edges
   for(Edge* edge : accel->edges){
      Edge* newEdge = newAccel->edges.Alloc();

      *newEdge = *edge;
      newEdge->units[0].inst = map.at(edge->units[0].inst);
      newEdge->units[1].inst = map.at(edge->units[1].inst);
   }

   // Flat copy of input instance pointers
   for(FUInstance** instPtr : accel->inputInstancePointers){
      FUInstance** newInstPtr = newAccel->inputInstancePointers.Alloc();

      *newInstPtr = map.at(*instPtr);
   }

   if(accel->outputInstance){
      newAccel->outputInstance = map.at(accel->outputInstance);
   }

   return newAccel;
}

static void ReallocInstances(Accelerator* accel,int* ptr,int* FUInstance::*arrayPtr,int FUDeclaration::*sizePtr){
   for(FUInstance* inst : accel->instances){
      int size = inst->declaration->*sizePtr;
      inst->*arrayPtr = ptr;

      if(inst->declaration->type == FUDeclaration::COMPOSITE && inst->compositeAccel){
         ReallocInstances(inst->compositeAccel,ptr,arrayPtr,sizePtr);
      }
      ptr += size;
   }
}

static void PrintStuff(Accelerator* accel,int* FUInstance::*arrayPtr){
   for(FUInstance* inst : accel->instances){
      printf("%p ",inst->*arrayPtr);

      if(inst->declaration->type == FUDeclaration::COMPOSITE && inst->compositeAccel){
         PrintStuff(inst->compositeAccel,arrayPtr);
      }
   };
}

static void CheckReallocation(Allocation<int>* accelInfo,FUInstance* inst,Accelerator* accel,int* FUInstance::*arrayPtr,int FUDeclaration::*sizePtr, int powerOf2Size){
   int size = inst->declaration->*sizePtr;

   if(powerOf2Size){
      size = (1 << size);
   }

   if(size){
      if(ZeroOutRealloc(accelInfo,accelInfo->size + size)){
         ReallocInstances(accel,accelInfo->ptr,arrayPtr,sizePtr);
         accelInfo->size = accelInfo->size + size;
      }
   }
}

FUInstance* CreateFUInstance(Accelerator* accel,FUDeclaration* type,SizedString name){
   LockAccelerator(accel,Accelerator::Locked::FREE);

   FUInstance* ptr = accel->instances.Alloc();

   ptr->id = accel->entityId++;
   ptr->accel = accel;
   ptr->declaration = type;

   FixedStringCpy(ptr->name.str,name);
   ptr->namedAccess = true;

   if(accel->type == Accelerator::CIRCUIT){
      return ptr;
   }

   CheckReallocation(&accel->configAlloc   ,ptr,accel,&FUInstance::config   ,&FUDeclaration::nConfigs, false);
   CheckReallocation(&accel->stateAlloc    ,ptr,accel,&FUInstance::state    ,&FUDeclaration::nStates, false);
   CheckReallocation(&accel->delayAlloc    ,ptr,accel,&FUInstance::delay    ,&FUDeclaration::nDelays, false);

   if(type->nOutputs){
      ptr->outputs = (int*) calloc(type->nOutputs,sizeof(int));
      ptr->storedOutputs = (int*) calloc(type->nOutputs,sizeof(int));
   }

   if(type->extraDataSize){
      ptr->extraData = calloc(type->extraDataSize,sizeof(char));
   }

   if(type->type == FUDeclaration::COMPOSITE){
      ptr->compositeAccel = InstantiateSubAccelerator(accel->versat,accel,type,type->circuit,&ptr->name,ptr->config,ptr->state,ptr->memMapped,ptr->delay);
   }

   if(!ptr->config && accel->staticAlloc.ptr){
      ptr->config = accel->staticAlloc.ptr;
   }

   if(type->initializeFunction)
      type->initializeFunction(ptr);

   return ptr;
}

void RemoveFUInstance(Accelerator* accel,FUInstance* inst){
   LockAccelerator(accel,Accelerator::Locked::FREE);

   //TODO: Remove instance doesn't update the config / state / memMapped / delay pointers

   for(Edge* edge : accel->edges){
      if(edge->units[0].inst == inst){
         accel->edges.Remove(edge);
      } else if(edge->units[1].inst == inst){
         accel->edges.Remove(edge);
      }
   }

   accel->instances.Remove(inst);
}

static StaticInfo* SetLikeInsert(Pool<StaticInfo>& vec,StaticInfo& info){
   for(StaticInfo* iter : vec){
      if(info.module == iter->module && CompareString(info.name,iter->name)){
         return iter;
      }
   }

   StaticInfo* res = vec.Alloc();
   *res = info;

   return res;
}

FUDeclaration* RegisterSubUnit(Versat* versat,SizedString name,Accelerator* circuit){
   FUDeclaration decl = {};

   decl.type = FUDeclaration::COMPOSITE;
   decl.circuit = circuit;
   decl.name = PushString(&versat->permanent,name);

   // HACK, for now
   circuit->outputInstance = nullptr;

   for(FUInstance* inst : circuit->instances){
      if(inst->declaration == versat->input){
         decl.nInputs += 1;
      }
      if(inst->declaration == versat->output){
         Assert(!circuit->outputInstance);
         circuit->outputInstance = inst;
      }
   }

   if(circuit->outputInstance){
      for(Edge* edge : circuit->edges){
         if(edge->units[0].inst == circuit->outputInstance){
            decl.nOutputs = maxi(decl.nOutputs - 1,edge->units[0].port) + 1;
         }
         if(edge->units[1].inst == circuit->outputInstance){
            decl.nOutputs = maxi(decl.nOutputs - 1,edge->units[1].port) + 1;
         }
      }
   }

   CalculateDelay(versat,circuit);

   decl.inputDelays = (int*) calloc(decl.nInputs,sizeof(int));

   int i = 0;
   int minimum = (1 << 30);
   for(FUInstance** input : circuit->inputInstancePointers){
      decl.inputDelays[i++] = (*input)->baseDelay;
      minimum = mini(minimum,(*input)->baseDelay);
   }

   decl.latencies = (int*) calloc(decl.nOutputs,sizeof(int));

   if(circuit->outputInstance){
      for(int i = 0; i < decl.nOutputs; i++){
         decl.latencies[i] = circuit->outputInstance->tempData->inputDelay;
      }
   }

   OutputGraphDotFile(circuit,1,"circuit.dot");

   int memoryMapBits[32];
   memset(memoryMapBits,0,sizeof(int) * 32);
   for(FUInstance* inst : circuit->instances){
      FUDeclaration* d = inst->declaration;

      if(d->isMemoryMapped){
         memoryMapBits[d->memoryMapBits] += 1;
      }

      if(inst->isStatic){
         decl.nStaticConfigs += d->nConfigs;
      } else {
         decl.nConfigs += d->nConfigs;
      }

      decl.nStates += d->nStates;
      decl.nDelays += d->nDelays;
      decl.extraDataSize += d->extraDataSize;
      decl.nIOs += d->nIOs;
   }

   // Huffman encoding calculation for max bits
   int last = -1;
   while(1){
      for(int i = 0; i < 32; i++){
         if(memoryMapBits[i]){
            memoryMapBits[i+1] += (memoryMapBits[i] / 2);
            memoryMapBits[i] = memoryMapBits[i] % 2;
            last = i;
         }
      }

      int first = -1;
      int second = -1;
      for(int i = 0; i < 32; i++){
         if(first == -1 && memoryMapBits[i] == 1){
            first = i;
         } else if(second == -1 && memoryMapBits[i] == 1){
            second = i;
            break;
         }
      }

      if(second == -1){
         break;
      }

      memoryMapBits[first] = 0;
      memoryMapBits[second] = 0;
      memoryMapBits[maxi(first,second) + 1] += 1;
   }

   if(last != -1){
      decl.isMemoryMapped = true;
      decl.memoryMapBits = last;
   }

   decl.configWires = PushArray(&versat->permanent,decl.nConfigs,Wire);
   decl.stateWires = PushArray(&versat->permanent,decl.nStates,Wire);

   int configIndex = 0;
   int stateIndex = 0;
   for(FUInstance* inst : circuit->instances){
      FUDeclaration* d = inst->declaration;

      if(!inst->isStatic){
         for(int i = 0; i < d->nConfigs; i++){
            int newSize = d->configWires[i].name.size + 16;

            Byte* newName = PushBytes(&versat->permanent,newSize);
            int size = sprintf(newName,"%.*s_%.2d",UNPACK_SS(d->configWires[i].name),configIndex);

            Assert(size < newSize);

            decl.configWires[configIndex].name = MakeSizedString(newName,size);
            decl.configWires[configIndex++].bitsize = d->configWires[i].bitsize;
         }
      }

      for(int i = 0; i < d->nStates; i++){
         int newSize = d->stateWires[i].name.size + 16;

         Byte* newName = PushBytes(&versat->permanent,newSize);
         int size = sprintf(newName,"%.*s_%.2d",UNPACK_SS(d->stateWires[i].name),stateIndex);

         Assert(size < newSize);

         decl.stateWires[stateIndex].name = MakeSizedString(newName,size);
         decl.stateWires[stateIndex++].bitsize = d->stateWires[i].bitsize;
      }
   }

   // TODO: Change unit delay type inference. Only care about delay type to upper levels.
   // Type source only if a source unit is connected to out. Type sink only if there is a input to sink connection
   #if 1
   bool hasSourceDelay = false;
   bool hasSinkDelay = false;
   #endif
   bool implementsDone = false;

   LockAccelerator(circuit,Accelerator::Locked::GRAPH);
   for(FUInstance* inst : circuit->instances){
      if(inst->declaration->type == FUDeclaration::SPECIAL){
         continue;
      }

      if(inst->declaration->implementsDone){
         implementsDone = true;
      }
      #if 1
      if(inst->tempData->nodeType == GraphComputedData::TAG_SINK){
         hasSinkDelay = CHECK_DELAY(inst,DELAY_TYPE_SINK_DELAY);
      }
      if(inst->tempData->nodeType == GraphComputedData::TAG_SOURCE){
         hasSourceDelay = CHECK_DELAY(inst,DELAY_TYPE_SOURCE_DELAY);
      }
      if(inst->tempData->nodeType == GraphComputedData::TAG_SOURCE_AND_SINK){
         hasSinkDelay = CHECK_DELAY(inst,DELAY_TYPE_SINK_DELAY);
         hasSourceDelay = CHECK_DELAY(inst,DELAY_TYPE_SOURCE_DELAY);
      }
      #endif
   }

   #if 1
   if(hasSourceDelay){
      decl.delayType = (DelayType) ((int)decl.delayType | (int) DelayType::DELAY_TYPE_SOURCE_DELAY);
   }
   if (hasSinkDelay){
      decl.delayType = (DelayType) ((int)decl.delayType | (int) DelayType::DELAY_TYPE_SINK_DELAY);
   }
   #endif

   decl.implementsDone = implementsDone;

   FUDeclaration* res = RegisterFU(versat,decl);

   // TODO: Hackish
   #if 1
   for(FUInstance* inst : circuit->instances){
      if(inst->isStatic){
         StaticInfo unit = {};
         unit.module = res;
         unit.name = MakeSizedString(inst->name.str);
         unit.nConfigs = inst->declaration->nConfigs;
         unit.wires = inst->declaration->configWires;

         Assert(unit.wires);

         SetLikeInsert(res->staticUnits,unit);
      } else if(inst->declaration->type == FUDeclaration::COMPOSITE){
         for(StaticInfo* unit : inst->declaration->staticUnits){
            SetLikeInsert(res->staticUnits,*unit);
         }
      }
   }
   #endif

   #if 1
   {
   char buffer[256];
   sprintf(buffer,"src/%.*s.v",UNPACK_SS(decl.name));
   FILE* sourceCode = fopen(buffer,"w");
   OutputCircuitSource(versat,res,circuit,sourceCode);
   fclose(sourceCode);
   }
   #endif

   return res;
}

static bool IsGraphValid(Accelerator* accel){
   InstanceMap map;

   for(FUInstance* inst : accel->instances){
      inst->tag = 0;

      map.insert({inst,inst});
   }

   for(Edge* edge : accel->edges){
      for(int i = 0; i < 2; i++){
         auto res = map.find(edge->units[i].inst);

         if(res == map.end()){
            return 0;
         }

         res->first->tag = 1;
      }
   }

   for(FUInstance* inst : accel->instances){
      if(inst->tag != 1){
         return 0;
      }
   }

   return 1;
}

static Accelerator* CopyAccelerator(Versat* versat,Accelerator* accel,InstanceMap* map){
   Accelerator* newAccel = CreateAccelerator(versat);

   // Flat copy of instances
   for(FUInstance* inst : accel->instances){
      FUInstance* newInst = CreateFUInstance(newAccel,inst->declaration,MakeSizedString(inst->name.str));

      newInst->name = inst->name;
      newInst->baseDelay = inst->baseDelay;

      map->insert({inst,newInst});
   }

   // Flat copy of edges
   for(Edge* edge : accel->edges){
      Edge* newEdge = newAccel->edges.Alloc();

      *newEdge = *edge;
      newEdge->units[0].inst = map->at(edge->units[0].inst);
      newEdge->units[1].inst = map->at(edge->units[1].inst);
   }

   // Flat copy of input instance pointers
   for(FUInstance** instPtr : accel->inputInstancePointers){
      FUInstance** newInstPtr = newAccel->inputInstancePointers.Alloc();

      *newInstPtr = map->at(*instPtr);
   }

   if(accel->outputInstance){
      newAccel->outputInstance = map->at(accel->outputInstance);
   }

   return newAccel;
}

Accelerator* Flatten(Versat* versat,Accelerator* accel,int times){
   InstanceMap map;
   Accelerator* newAccel = CopyAccelerator(versat,accel,&map);
   map.clear();

   PortInstance outputs[60];
   for(int i = 0; i < 60; i++){
      outputs[i] = (PortInstance){};
   }

   // TODO: figure out a way to have true hierarchircal naming schemes even when flattening
   // Use it to debug further. No point wasting time right now
   Pool<FUInstance*> compositeInstances = {};
   for(int i = 0; i < times; i++){
      for(FUInstance* inst : newAccel->instances){
         if(inst->declaration->type == FUDeclaration::COMPOSITE){
            FUInstance** ptr = compositeInstances.Alloc();
            *ptr = inst;
         }
      }

      int count = 0;
      for(FUInstance** instPtr : compositeInstances){
         FUInstance* inst = *instPtr;
         count += 1;
         Accelerator* circuit = inst->declaration->circuit;

         // Create new instance and map then
         #if 1
         for(FUInstance* circuitInst : circuit->instances){
            if(circuitInst->declaration->type == FUDeclaration::SPECIAL){
               continue;
            }

            FUInstance* newInst = CreateFUInstance(newAccel,circuitInst->declaration,MakeSizedString(circuitInst->name.str));
            newInst->name.parent = circuitInst->name.parent;
            newInst->baseDelay = circuitInst->baseDelay;

            map.insert({circuitInst,newInst});
         }
         #endif

         #if 1
         // Add accel edges to output instances
         for(Edge* edge : newAccel->edges){
            if(edge->units[0].inst == inst){
               for(Edge* circuitEdge: circuit->edges){
                  if(circuitEdge->units[1].inst == circuit->outputInstance && circuitEdge->units[1].port == edge->units[0].port){
                     Edge* newEdge = newAccel->edges.Alloc();

                     FUInstance* mappedInst = map.at(circuitEdge->units[0].inst);

                     *newEdge = *edge;
                     newEdge->units[0].inst = mappedInst;
                     newEdge->units[0].port = edge->units[0].port;
                  }
               }
            }
         }
         #endif

         #if 1
         // Add accel edges to input instances
         for(Edge* edge : newAccel->edges){
            if(edge->units[1].inst == inst){
               FUInstance* circuitInst = *circuit->inputInstancePointers.Get(edge->units[1].port);

               for(Edge* circuitEdge : circuit->edges){
                  if(circuitEdge->units[0].inst == circuitInst){
                     Edge* newEdge = newAccel->edges.Alloc();

                     FUInstance* mappedInst = map.at(circuitEdge->units[1].inst);

                     *newEdge = *edge;
                     newEdge->units[1].inst = mappedInst;
                     newEdge->units[1].port = circuitEdge->units[1].port;
                  }
               }
            }
         }
         #endif

         #if 1
         // Add circuit specific edges
         for(Edge* circuitEdge : circuit->edges){
            auto input = map.find(circuitEdge->units[0].inst);
            auto output = map.find(circuitEdge->units[1].inst);

            if(input == map.end() || output == map.end()){
               continue;
            }

            Edge* newEdge = newAccel->edges.Alloc();

            *newEdge = *circuitEdge;
            newEdge->units[0].inst = input->second;
            newEdge->units[1].inst = output->second;
         }
         #endif

         #if 0
         SubgraphData subgraph = SubGraphAroundInstance(versat,newAccel,inst,6);
         {
            char buffer[128];
            sprintf(buffer,"debug/%d_%d_0.dot",i,count);
            FILE* dotFile = fopen(buffer,"w");
            OutputGraphDotFile(subgraph.accel,dotFile,1);
            fclose(dotFile);
         }
         #endif

         RemoveFUInstance(newAccel,inst);
         map.clear();

         #if 0
         RemoveFUInstance(subgraph.accel,subgraph.instanceMapped);
         {
            char buffer[128];
            sprintf(buffer,"debug/%d_%d_1.dot",i,count);
            FILE* dotFile = fopen(buffer,"w");
            OutputGraphDotFile(subgraph.accel,dotFile,1);
            fclose(dotFile);
         }
         #endif

         Assert(IsGraphValid(newAccel));
      }

      compositeInstances.Clear();
   }

   return newAccel;
}

// Debug output file

#define DUMP_VCD 1

static FILE* accelOutputFile = nullptr;
static std::array<char,4> currentMapping = {'a','a','a','a'};
static void ResetMapping(){
   currentMapping[0] = 'a';
   currentMapping[1] = 'a';
   currentMapping[2] = 'a';
   currentMapping[3] = 'a';
}

static void IncrementMapping(){
   for(int i = 3; i >= 0; i--){
      currentMapping[i] += 1;
      if(currentMapping[i] == 'z' + 1){
         currentMapping[i] = 'a';
      } else {
         return;
      }
   }
   Assert(false && "Increase mapping space");
}

static void PrintVCDDefinitions_(Accelerator* accel){
   for(FUInstance* inst : accel->instances){
      fprintf(accelOutputFile,"$scope module %s_%d $end\n",inst->name.str,inst->id);

      for(int i = 0; i < inst->tempData->inputPortsUsed; i++){
         fprintf(accelOutputFile,"$var wire  32 %c%c%c%c %s_in%d $end\n",currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3],GetHierarchyNameRepr(inst->name),i);
         IncrementMapping();
      }

      for(int i = 0; i < inst->tempData->outputPortsUsed; i++){
         fprintf(accelOutputFile,"$var wire  32 %c%c%c%c %s_out%d $end\n",currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3],GetHierarchyNameRepr(inst->name),i);
         IncrementMapping();
      }

      for(int i = 0; i < inst->declaration->nConfigs; i++){
         Wire* wire = &inst->declaration->configWires[i];
         fprintf(accelOutputFile,"$var wire  %d %c%c%c%c %.*s $end\n",wire->bitsize,currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3],UNPACK_SS(wire->name));
         IncrementMapping();
      }

      for(int i = 0; i < inst->declaration->nStates; i++){
         Wire* wire = &inst->declaration->stateWires[i];
         fprintf(accelOutputFile,"$var wire  %d %c%c%c%c %.*s $end\n",wire->bitsize,currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3],UNPACK_SS(wire->name));
         IncrementMapping();
      }

      for(StaticInfo info : accel->staticInfo){
         for(int i = 0; i < info.nConfigs; i++){
            Wire* wire = &info.wires[i];
            fprintf(accelOutputFile,"$var wire  %d %c%c%c%c %.*s_%.*s $end\n",wire->bitsize,currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3],UNPACK_SS(info.name),UNPACK_SS(wire->name));
            IncrementMapping();
         }
      }

      for(int i = 0; i < accel->delayAlloc.size; i++){
         fprintf(accelOutputFile,"$var wire 32 %c%c%c%c delay%d $end\n",currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3],i);
         IncrementMapping();
      }

      if(inst->declaration->implementsDone){
         fprintf(accelOutputFile,"$var wire  1 %c%c%c%c done $end\n",currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3]);
         IncrementMapping();
      }

      if(inst->declaration->type == FUDeclaration::COMPOSITE){
         PrintVCDDefinitions_(inst->compositeAccel);
      }

      fprintf(accelOutputFile,"$upscope $end\n");
   }
}

static void PrintVCDDefinitions(Accelerator* accel){
   #if DUMP_VCD == 0
      return;
   #endif

   ResetMapping();

   fprintf(accelOutputFile,"$timescale   1ns $end\n");
   fprintf(accelOutputFile,"$scope module TOP $end\n");
   fprintf(accelOutputFile,"$var wire  1 a clk $end\n");
   PrintVCDDefinitions_(accel);
   fprintf(accelOutputFile,"$upscope $end\n");
   fprintf(accelOutputFile,"$enddefinitions $end\n");
}

static char* Bin(unsigned int val){
   static char buffer[33];
   buffer[32] = '\0';

   for(int i = 0; i < 32; i++){
      if(val - (1 << (31 - i)) < val){
         val = val - (1 << (31 - i));
         buffer[i] = '1';
      } else {
         buffer[i] = '0';
      }
   }
   return buffer;
}

static void PrintVCD_(Accelerator* accel,int time){
   for(FUInstance* inst : accel->instances){
      for(int i = 0; i < inst->tempData->inputPortsUsed; i++){
         fprintf(accelOutputFile,"b%s %c%c%c%c\n",Bin(GetInputValue(inst,i)),currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3]);
         IncrementMapping();
      }

      for(int i = 0; i < inst->tempData->outputPortsUsed; i++){
         fprintf(accelOutputFile,"b%s %c%c%c%c\n",Bin(inst->outputs[i]),currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3]);
         IncrementMapping();
      }

      for(int i = 0; i < inst->declaration->nConfigs; i++){
         if(time == 0){
            fprintf(accelOutputFile,"b%s %c%c%c%c\n",Bin(inst->config[i]),currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3]);
         }
         IncrementMapping();
      }

      for(int i = 0; i < inst->declaration->nStates; i++){
         fprintf(accelOutputFile,"b%s %c%c%c%c\n",Bin(inst->state[i]),currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3]);
         IncrementMapping();
      }

      for(StaticInfo info : accel->staticInfo){
         for(int i = 0; i < info.nConfigs; i++){
            if(time == 0){
               fprintf(accelOutputFile,"b%s %c%c%c%c\n",Bin(info.ptr[i]),currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3]);
            }
            IncrementMapping();
         }
      }

      for(int i = 0; i < accel->delayAlloc.size; i++){
         fprintf(accelOutputFile,"b%s %c%c%c%c\n",Bin(accel->delayAlloc.ptr[i]),currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3]);
         IncrementMapping();
      }

      if(inst->declaration->implementsDone){
         fprintf(accelOutputFile,"%d%c%c%c%c\n",inst->done ? 1 : 0,currentMapping[0],currentMapping[1],currentMapping[2],currentMapping[3]);
         IncrementMapping();
      }

      if(inst->declaration->type == FUDeclaration::COMPOSITE){
         PrintVCD_(inst->compositeAccel,time);
      }
   }
}

static void PrintVCD(Accelerator* accel,int time,int clock){ // Need to put some clock signal
   #if DUMP_VCD == 0
      return;
   #endif

   ResetMapping();

   fprintf(accelOutputFile,"#%d\n",time * 10);
   fprintf(accelOutputFile,"%da\n",clock ? 1 : 0);
   PrintVCD_(accel,time);
}

static void VisitAcceleratorInstances_(FUInstance* inst,AcceleratorInstancesVisitor func){
   func(inst);

   if(inst->declaration->type == FUDeclaration::COMPOSITE && inst->compositeAccel){
      VisitAcceleratorInstances(inst->compositeAccel,func);
   }
}

static void VisitAcceleratorInstances(Accelerator* accel,AcceleratorInstancesVisitor func){
   LockAccelerator(accel,Accelerator::Locked::ORDERED);

   for(FUInstance* inst : accel->instances){
      VisitAcceleratorInstances_(inst,func);
   }
}

static void AcceleratorRunStartVisitor(FUInstance* inst){
   FUFunction startFunction = inst->declaration->startFunction;

   if(startFunction){
      int* startingOutputs = startFunction(inst);

      if(startingOutputs){
         memcpy(inst->outputs,startingOutputs,inst->declaration->nOutputs * sizeof(int));
         memcpy(inst->storedOutputs,startingOutputs,inst->declaration->nOutputs * sizeof(int));
      }
   }
}

static void AcceleratorRunStart(Accelerator* accel){
   LockAccelerator(accel,Accelerator::Locked::ORDERED);

   VisitAcceleratorInstances(accel,AcceleratorRunStartVisitor);
}

static bool AcceleratorDone(Accelerator* accel){
   for(FUInstance* inst : accel->instances){
      if(inst->declaration->implementsDone && !inst->done){
         return false;
      }
   }

   return true;
}

static void AcceleratorRunIteration(Accelerator* accel){
   for(int i = 0; i < accel->instances.Size(); i++){
      FUInstance* inst = accel->order.instances.ptr[i];

      if(inst->declaration->type == FUDeclaration::SPECIAL){
         continue;
      } else if(inst->declaration->type == FUDeclaration::COMPOSITE){
         // Set accelerator input to instance input
         for(int ii = 0; ii < inst->tempData->numberInputs; ii++){
            FUInstance* input = *inst->compositeAccel->inputInstancePointers.Get(ii);

            for(int iii = 0; iii < input->tempData->numberOutputs; iii++){
               int val = GetInputValue(inst,ii);
               input->outputs[iii] = val;
               input->storedOutputs[iii] = val;
            }
         }

         AcceleratorRunIteration(inst->compositeAccel);

         // Calculate unit done
         if(inst->declaration->implementsDone){
            inst->done = AcceleratorDone(inst->compositeAccel);
         }

         // Set output instance value to accelerator output
         FUInstance* output = inst->compositeAccel->outputInstance;
         if(output){
            for(int ii = 0; ii < output->tempData->numberInputs; ii++){
               int val = GetInputValue(output,ii);
               inst->outputs[ii] = val;
               inst->storedOutputs[ii] = val;
            }
         }
      } else {
         int* newOutputs = inst->declaration->updateFunction(inst);

         if(inst->declaration->latencies[0] == 0 && inst->tempData->nodeType != GraphComputedData::TAG_SOURCE){
            memcpy(inst->outputs,newOutputs,inst->declaration->nOutputs * sizeof(int));
            memcpy(inst->storedOutputs,newOutputs,inst->declaration->nOutputs * sizeof(int));
         } else {
            memcpy(inst->storedOutputs,newOutputs,inst->declaration->nOutputs * sizeof(int));
         }
      }
   }
}

void LoadConfiguration(Accelerator* accel,int configuration){
   // Implements the reverse of Save Configuration
}

void SaveConfiguration(Accelerator* accel,int configuration){
   //Assert(configuration < accel->versat->numberConfigurations);
}

void AcceleratorDoCycle(Accelerator* accel){
   VisitAcceleratorInstances(accel,[](FUInstance* inst){
      memcpy(inst->outputs,inst->storedOutputs,inst->declaration->nOutputs * sizeof(int));
   });
}

void AcceleratorRun(Accelerator* accel){
   static int numberRuns = 0;
   int time = 0;

   CalculateDelay(accel->versat,accel);
   SetDelayRecursive(accel);

   // Only used to lock all acelerators
   VisitAcceleratorInstances(accel,[](FUInstance* inst){
   });

   if(accel->versat->debug.outputAccelerator){
      char buffer[128];
      sprintf(buffer,"debug/accelRun%d.vcd",numberRuns++);
      accelOutputFile = fopen(buffer,"w");
      Assert(accelOutputFile);

      PrintVCDDefinitions(accel);
   }

   AcceleratorRunStart(accel);
   AcceleratorRunIteration(accel);

   if(accel->versat->debug.outputAccelerator){
      PrintVCD(accel,time++,0);
   }

   for(int cycle = 0; 1; cycle++){
      AcceleratorDoCycle(accel);
      AcceleratorRunIteration(accel);

      if(accel->versat->debug.outputAccelerator){
         PrintVCD(accel,time++,1);
         PrintVCD(accel,time++,0);
      }

      #if 1
      if(AcceleratorDone(accel)){
         break;
      }
      #endif
   }

   if(accel->versat->debug.outputAccelerator){
      PrintVCD(accel,time++,1);
      PrintVCD(accel,time++,0);
      fclose(accelOutputFile);
   }
}

static VersatComputedValues ComputeVersatValues(Versat* versat,Accelerator* accel){
   LockAccelerator(accel,Accelerator::Locked::GRAPH);

   VersatComputedValues res = {};

   for(FUInstance* inst : accel->instances){
      FUDeclaration* decl = inst->declaration;

      res.numberConnections += inst->tempData->numberOutputs;

      res.maxMemoryMapDWords = maxi(res.maxMemoryMapDWords,1 << decl->memoryMapBits);
      res.memoryMapped += (1 << decl->memoryMapBits);

      if(decl->isMemoryMapped)
         res.unitsMapped += 1;

      res.nConfigs += decl->nConfigs;
      for(int i = 0; i < decl->nConfigs; i++){
         res.configBits += decl->configWires[i].bitsize;
      }

      res.nStates += decl->nStates;
      for(int i = 0; i < decl->nStates; i++){
         res.stateBits += decl->stateWires[i].bitsize;
      }

      res.nDelays += decl->nDelays;
      res.delayBits += decl->nDelays * 32;

      res.nUnitsIO += decl->nIOs;
   }

   for(auto& unit : accel->staticInfo){
      res.nStatics += unit.nConfigs;

      for(int i = 0; i < unit.nConfigs; i++){
         res.staticBits += unit.wires[i].bitsize;
      }
   }

   // Versat specific registers are treated as a special maping (all 0's) of 1 configuration and 1 state register
   res.nConfigs += 1;
   res.nStates += 1;

   res.nConfigurations = res.nConfigs + res.nStatics + res.nDelays;
   res.configurationBits = res.configBits + res.staticBits + res.delayBits;

   res.memoryAddressBits = log2i(res.memoryMapped);

   res.memoryMappingAddressBits = res.memoryAddressBits;
   res.configurationAddressBits = log2i(res.nConfigurations);
   res.stateAddressBits = log2i(res.nStates);
   res.stateConfigurationAddressBits = maxi(res.configurationAddressBits,res.stateAddressBits);

   res.lowerAddressSize = maxi(res.stateConfigurationAddressBits,res.memoryMappingAddressBits);

   res.memoryConfigDecisionBit = res.lowerAddressSize;

   return res;
}

void OutputMemoryMap(Versat* versat,Accelerator* accel){
   VersatComputedValues val = ComputeVersatValues(versat,accel);

   printf("\n");
   printf("Total bytes mapped: %d\n",val.memoryMapped * 4);
   printf("Maximum bytes mapped by a unit: %d\n",val.maxMemoryMapDWords * 4);
   printf("Memory address bits: %d\n",val.memoryAddressBits);
   printf("Units mapped: %d\n",val.unitsMapped);
   printf("Memory mapping address bits: %d\n",val.memoryMappingAddressBits);
   printf("\n");
   printf("Config registers: %d\n",val.nConfigs);
   printf("Config bits used: %d\n",val.configBits);
   printf("\n");
   printf("Static registers: %d\n",val.nStatics);
   printf("Static bits used: %d\n",val.staticBits);
   printf("\n");
   printf("Delay registers: %d\n",val.nDelays);
   printf("Delay bits used: %d\n",val.delayBits);
   printf("\n");
   printf("Configuration registers: %d (including versat reg, static and delays)\n",val.nConfigurations);
   printf("Configuration address bits: %d\n",val.configurationAddressBits);
   printf("Configuration bits used: %d\n",val.configurationBits);
   printf("\n");
   printf("State registers: %d (including versat reg)\n",val.nStates);
   printf("State address bits: %d\n",val.stateAddressBits);
   printf("State bits used: %d\n",val.stateBits);
   printf("\n");
   printf("IO connections: %d\n",val.nUnitsIO);

   printf("\n");
   printf("Number units: %d\n",versat->accelerators.Get(0)->instances.Size());
   printf("\n");

   #define ALIGN_FORMAT "%-14s"
   #define ALIGN_SIZE 14

   int bitsNeeded = val.lowerAddressSize;

   printf(ALIGN_FORMAT,"Address:");
   for(int i = bitsNeeded; i >= 10; i--)
      printf("%d ",i/10);
   printf("\n");
   printf(ALIGN_FORMAT," ");
   for(int i = bitsNeeded; i >= 0; i--)
      printf("%d ",i%10);
   printf("\n");
   for(int i = bitsNeeded + (ALIGN_SIZE / 2); i >= 0; i--)
      printf("==");
   printf("\n");

   // Memory mapped
   printf(ALIGN_FORMAT,"MemoryMapped:");
   printf("1 ");
   for(int i = bitsNeeded - 1; i >= 0; i--)
      if(i < val.memoryAddressBits)
         printf("M ");
      else
         printf("0 ");
   printf("\n");
   for(int i = bitsNeeded + (ALIGN_SIZE / 2); i >= 0; i--)
      printf("==");
   printf("\n");

   // Versat registers
   printf(ALIGN_FORMAT,"Versat Regs:");
   for(int i = bitsNeeded - 0; i >= 0; i--)
      printf("0 ");
   printf("\n");
   for(int i = bitsNeeded + (ALIGN_SIZE / 2); i >= 0; i--)
      printf("==");
   printf("\n");

   // Config/State
   printf(ALIGN_FORMAT,"Config/State:");
   printf("0 ");
   for(int i = bitsNeeded - 1; i >= 0; i--){
      if(i < val.configurationAddressBits && i < val.stateAddressBits)
         printf("B ");
      else if(i < val.configurationAddressBits)
         printf("C ");
      else if(i < val.stateAddressBits)
         printf("S ");
      else
         printf("0 ");
   }
   printf("\n");
   for(int i = bitsNeeded + (ALIGN_SIZE / 2); i >= 0; i--)
      printf("==");
   printf("\n");

   printf("\n");
   printf("M - Memory mapped\n");
   printf("C - Used only by Config\n");
   printf("S - Used only by State\n");
   printf("B - Used by both Config and State\n");
   printf("\n");
   printf("Memory/Config bit: %d\n",val.memoryConfigDecisionBit);
   printf("Memory range: [%d:0]\n",val.memoryAddressBits - 1);
   printf("Config range: [%d:0]\n",val.configurationAddressBits - 1);
   printf("State range: [%d:0]\n",val.stateAddressBits - 1);
}

void OutputUnitInfo(FUInstance* instance){
   LockAccelerator(instance->accel,Accelerator::Locked::FIXED);
}

int GetInputValue(FUInstance* instance,int index){
   Assert(instance->tempData);

   for(int i = 0; i < instance->tempData->numberInputs; i++){
      ConnectionInfo connection = instance->tempData->inputs[i];

      if(connection.port == index){
         return connection.inst.inst->outputs[connection.inst.port];
      }
   }

   return 0;

   #if 0
   for(Edge* edge : accel->edges){
      if(edge->units[1].inst == instance && edge->units[1].port == index){
         return edge->units[0].inst->outputs[edge->units[0].port];
      }
   }
   #endif
}

// Connects out -> in
void ConnectUnits(FUInstance* out,int outIndex,FUInstance* in,int inIndex){
   ConnectUnitsWithDelay(out,outIndex,in,inIndex,0);
}

void ConnectUnitsWithDelay(FUInstance* out,int outIndex,FUInstance* in,int inIndex,int delay){
   FUDeclaration* inDecl = in->declaration;
   FUDeclaration* outDecl = out->declaration;

   Assert(out->accel == in->accel);
   Assert(inIndex < inDecl->nInputs);
   Assert(outIndex < outDecl->nOutputs);

   Accelerator* accel = out->accel;

   LockAccelerator(accel,Accelerator::Locked::FREE);

   Edge* edge = accel->edges.Alloc();

   edge->units[0].inst = out;
   edge->units[0].port = outIndex;
   edge->units[1].inst = in;
   edge->units[1].port = inIndex;
   edge->delay = delay;
}

void OutputCircuitSource(Versat* versat,FUDeclaration* decl,Accelerator* accel,FILE* file){
   #if 1
   LockAccelerator(accel,Accelerator::Locked::FIXED);

   TemplateSetCustom("accel",decl,"FUDeclaration");

   VersatComputedValues val = ComputeVersatValues(versat,accel);

   // Output configuration file
   TemplateSetCustom("versatValues",&val,"VersatComputedValues");
   TemplateSetCustom("instances",&accel->instances,"Pool<FUInstance>");
   TemplateSetNumber("numberUnits",accel->instances.Size());

   int nonSpecialUnits = 0;
   for(FUInstance* inst : accel->instances){
      if(inst->declaration->type != FUDeclaration::SPECIAL && !inst->declaration->isOperation){
         nonSpecialUnits += 1;
      }
   }

   TemplateSetNumber("nonSpecialUnits",nonSpecialUnits);

   TemplateSetNumber("unitsMapped",val.unitsMapped);
   TemplateSetNumber("configurationBits",val.configurationBits);

   TemplateSetNumber("memoryConfigDecisionBit",val.memoryConfigDecisionBit);
   TemplateSetCustom("versat",versat,"Versat");

   ProcessTemplate(file,"../../submodules/VERSAT/software/templates/versat_accelerator_template.tpl",&versat->temp);
   #endif
}

void OutputVersatSource(Versat* versat,Accelerator* accel,const char* sourceFilepath,const char* constantsFilepath,const char* dataFilepath){
   LockAccelerator(accel,Accelerator::Locked::FIXED);

   #if 1
   // No need for templating, small file
   FILE* c = fopen(constantsFilepath,"w");

   if(!c){
      printf("Error creating file, check if filepath is correct: %s\n",constantsFilepath);
      return;
   }

   VersatComputedValues val = ComputeVersatValues(versat,accel);

   #if 1
   std::vector<FUInstance*> accum;
   VisitAcceleratorInstances(accel,[&](FUInstance* inst){
      if(inst->namedAccess){
         accum.push_back(inst);
      }
   });
   #endif

   fprintf(c,"`define NUMBER_UNITS %d\n",accel->instances.Size());
   fprintf(c,"`define CONFIG_W %d\n",val.configurationBits);
   fprintf(c,"`define STATE_W %d\n",val.stateBits);
   fprintf(c,"`define MAPPED_UNITS %d\n",val.unitsMapped);
   fprintf(c,"`define MAPPED_BIT %d\n",val.memoryConfigDecisionBit);
   fprintf(c,"`define nIO %d\n",val.nUnitsIO);

   if(val.nUnitsIO){
      fprintf(c,"`define VERSAT_IO\n");
   }

   fclose(c);

   FILE* s = fopen(sourceFilepath,"w");

   if(!s){
      printf("Error creating file, check if filepath is correct: %s\n",sourceFilepath);
      return;
   }

   FILE* d = fopen(dataFilepath,"w");

   if(!d){
      fclose(s);
      printf("Error creating file, check if filepath is correct: %s\n",dataFilepath);
      return;
   }

   TemplateSetNumber("numberUnits",accel->instances.Size());
   TemplateSetCustom("versatValues",&val,"VersatComputedValues");
   TemplateSetCustom("versat",versat,"Versat");
   TemplateSetCustom("accel",accel,"Accelerator");
   TemplateSetNumber("nStatics",val.nStatics);
   TemplateSetNumber("nDelays",val.nDelays);

   // Output configuration file
   TemplateSetCustom("instances",&accel->instances,"Pool<FUInstance>");

   TemplateSetNumber("versatBase",versat->base);
   TemplateSetNumber("memoryAddressBits",val.memoryAddressBits);
   TemplateSetNumber("unitsMapped",val.unitsMapped);
   TemplateSetNumber("memoryConfigDecisionBit",val.memoryConfigDecisionBit);
   TemplateSetNumber("configurationBits",val.configurationBits);

   ProcessTemplate(s,"../../submodules/VERSAT/software/templates/versat_top_instance_template.tpl",&versat->temp);

   unsigned int hashTableSize = 50;
   Assert(accum.size() < (hashTableSize / 2));
   HashKey hashMap[hashTableSize];
   for(size_t i = 0; i < hashTableSize; i++){
      hashMap[i].data = -1;
      hashMap[i].key = (SizedString){nullptr,0};
   }

   Byte* mark = MarkArena(&versat->temp);
   for(size_t i = 0; i < accum.size(); i++){
      char* name = GetHierarchyNameRepr(accum[i]->name);

      SizedString ss = PushString(&versat->temp,MakeSizedString(name));

      unsigned int hash = 0;
      for(int ii = 0; ii < ss.size; ii++){
         hash *= 17;
         hash += ss.str[ii];
      }

      if(hashMap[hash % hashTableSize].data == -1){
         hashMap[hash % hashTableSize].data = i;
         hashMap[hash % hashTableSize].key = ss;
      } else {
         unsigned int counter;
         for(counter = 0; counter < hashTableSize; counter++){
            if(hashMap[(hash + counter) % hashTableSize].data == -1){
               hashMap[(hash + counter) % hashTableSize].data = i;
               hashMap[(hash + counter) % hashTableSize].key = ss;
               break;
            }
         }
         Assert(counter != hashTableSize);
      }
   }

   TemplateSetNumber("hashTableSize",hashTableSize);
   TemplateSetArray("instanceHashmap","HashKey",hashMap,hashTableSize);

   TemplateSetNumber("configurationSize",accel->configAlloc.size);
   TemplateSetArray("configurationData","int",accel->configAlloc.ptr,accel->configAlloc.size);

   TemplateSetNumber("config",(int)accel->configAlloc.ptr);
   TemplateSetNumber("state",(int)accel->stateAlloc.ptr);
   TemplateSetNumber("memMapped",(int)0);
   TemplateSetNumber("static",(int)accel->staticAlloc.ptr);
   TemplateSetNumber("staticEnd",(int)&accel->staticAlloc.ptr[accel->staticAlloc.reserved]);

   TemplateSetArray("delay","int",accel->delayAlloc.ptr,accel->delayAlloc.size);
   TemplateSetArray("staticBuffer","int",accel->staticAlloc.ptr,accel->staticAlloc.size);

   TemplateSetNumber("memoryAddressBits",val.memoryAddressBits);
   TemplateSetNumber("memoryConfigDecisionBit",val.memoryConfigDecisionBit);
   TemplateSetNumber("versatBase",versat->base);
   TemplateSetNumber("memoryMappedBase",1 << val.memoryConfigDecisionBit);
   TemplateSetNumber("nConfigs",val.nConfigs);
   TemplateSetNumber("nDelays",val.nDelays);
   TemplateSetNumber("nStatics",val.nStatics);
   TemplateSetCustom("instances",&accum,"std::vector<FUInstance*>");
   TemplateSetNumber("numberUnits",accum.size());
   ProcessTemplate(d,"../../submodules/VERSAT/software/templates/embedData.tpl",&versat->temp);

   PopMark(&versat->temp,mark);

   fclose(s);
   fclose(d);
   #endif
}

#define MAX_CHARS 64

void CalculateGraphData(Accelerator* accel){
   int memoryNeeded = sizeof(GraphComputedData) * accel->instances.Size() + 2 * accel->edges.Size() * sizeof(ConnectionInfo);

   ZeroOutAlloc(&accel->graphData,memoryNeeded);

   GraphComputedData* computedData = (GraphComputedData*) accel->graphData.ptr;
   ConnectionInfo* inputBuffer = (ConnectionInfo*) &computedData[accel->instances.Size()];
   ConnectionInfo* outputBuffer = &inputBuffer[accel->edges.Size()];

   // Associate computed data to each instance
   int i = 0;
   for(FUInstance* inst : accel->instances){
      inst->tempData = &computedData[i++];
   }

   // Set inputs and outputs
   for(FUInstance* inst : accel->instances){
      inst->tempData->inputs = inputBuffer;
      inst->tempData->outputs = outputBuffer;

      for(Edge* edge : accel->edges){
         if(edge->units[0].inst == inst){
            outputBuffer->inst = edge->units[1];
            outputBuffer->port = edge->units[0].port;
            outputBuffer += 1;

            inst->tempData->outputPortsUsed = maxi(inst->tempData->outputPortsUsed,edge->units[0].port + 1);
            inst->tempData->numberOutputs += 1;
         }
         if(edge->units[1].inst == inst){
            inputBuffer->inst = edge->units[0];
            inputBuffer->port = edge->units[1].port;
            inputBuffer->delay = edge->delay;
            inputBuffer += 1;

            inst->tempData->inputPortsUsed = maxi(inst->tempData->inputPortsUsed,edge->units[1].port + 1);
            inst->tempData->numberInputs += 1;
         }
      }
   }

   for(FUInstance* inst : accel->instances){
      inst->tempData->nodeType = GraphComputedData::TAG_UNCONNECTED;

      bool hasInput = (inst->tempData->numberInputs > 0);
      bool hasOutput = (inst->tempData->numberOutputs > 0);

      // If the unit is both capable of acting as a sink or as a source of data
      if(hasInput && hasOutput){
         #if 0
         if(inst->declaration->nDelays){
            inst->tempData->nodeType = GraphComputedData::TAG_SOURCE_AND_SINK;
         } else {
            inst->tempData->nodeType = GraphComputedData::TAG_COMPUTE;
         }
         #else
         if(CHECK_DELAY(inst,DELAY_TYPE_SINK_DELAY) || CHECK_DELAY(inst,DELAY_TYPE_SOURCE_DELAY)){
            inst->tempData->nodeType = GraphComputedData::TAG_SOURCE_AND_SINK;
         }  else {
            inst->tempData->nodeType = GraphComputedData::TAG_COMPUTE;
         }
         #endif
      } else if(hasInput){
         inst->tempData->nodeType = GraphComputedData::TAG_SINK;
      } else if(hasOutput){
         inst->tempData->nodeType = GraphComputedData::TAG_SOURCE;
      } else {
         //Assert(0); // Unconnected
      }
   }
}

struct HuffmanBlock{
   int bits;
   FUInstance* instance; // TODO: Maybe add the instance index (on the list) so we can push to the left instances that appear first and make it easier to see the mapping taking place
   HuffmanBlock* left;
   HuffmanBlock* right;
   enum {LEAF,NODE} type;
};

static void SaveMemoryMappingInfo(char* buffer,int size,HuffmanBlock* block){
   if(block->type == HuffmanBlock::LEAF){
      FUInstance* inst = block->instance;

      memcpy(inst->versatData->memoryMask,buffer,size);
      inst->versatData->memoryMask[size] = '\0';
      inst->versatData->memoryMaskSize = size;
   } else {
      buffer[size] = '1';
      SaveMemoryMappingInfo(buffer,size + 1,block->left);
      buffer[size] = '0';
      SaveMemoryMappingInfo(buffer,size + 1,block->right);
   }
}

void CalculateVersatData(Accelerator* accel){
   ZeroOutAlloc(&accel->versatData,accel->instances.Size());

   VisitAcceleratorInstances(accel,[](FUInstance* inst){
      if(inst->declaration->type == FUDeclaration::COMPOSITE && inst->compositeAccel){
         LockAccelerator(inst->compositeAccel,Accelerator::FIXED);
      }
   });

   VersatComputedData* mem = accel->versatData.ptr;

   auto Compare = [](const HuffmanBlock* a, const HuffmanBlock* b) {
      return a->bits > b->bits;
   };

   Pool<HuffmanBlock> blocks = {};
   std::priority_queue<HuffmanBlock*,std::vector<HuffmanBlock*>,decltype(Compare)> nodes(Compare);

   for(FUInstance* inst : accel->instances){
      inst->versatData = mem++;

      if(inst->declaration->isMemoryMapped){
         HuffmanBlock* block = blocks.Alloc();

         block->bits = inst->declaration->memoryMapBits;
         block->instance = inst;
         block->type = HuffmanBlock::LEAF;

         nodes.push(block);
      }
   }

   if(nodes.size() == 0){
      return;
   }

   while(nodes.size() > 1){
      HuffmanBlock* first = nodes.top();
      nodes.pop();
      HuffmanBlock* second = nodes.top();
      nodes.pop();

      HuffmanBlock* block = blocks.Alloc();
      block->type = HuffmanBlock::NODE;
      block->left = first;
      block->right = second;
      block->bits = maxi(first->bits,second->bits) + 1;

      nodes.push(block);
   }

   char bitMapping[33];
   memset(bitMapping,0,33 * sizeof(char));
   HuffmanBlock* root = nodes.top();

   SaveMemoryMappingInfo(bitMapping,0,root);

   int maxMask = 0;
   for(FUInstance* inst : accel->instances){
      if(inst->declaration->isMemoryMapped){
         maxMask = maxi(maxMask,inst->versatData->memoryMaskSize + inst->declaration->memoryMapBits);
      }
   }

   for(FUInstance* inst : accel->instances){
      if(inst->declaration->isMemoryMapped){
         int memoryAddressOffset = 0;
         for(int i = 0; i < inst->versatData->memoryMaskSize; i++){
            int bit = inst->versatData->memoryMask[i] - '0';

            memoryAddressOffset += bit * (1 << (maxMask - i - 1));
         }
         inst->versatData->memoryAddressOffset = memoryAddressOffset;

         // TODO: Not a good way of dealing with calculating memory map to lower units, but should suffice for now
         if(inst->declaration->type == FUDeclaration::COMPOSITE && inst->compositeAccel){
            VisitAcceleratorInstances(inst->compositeAccel,[&](FUInstance* inst){
               if(inst->declaration->isMemoryMapped){
                  inst->versatData->memoryAddressOffset += memoryAddressOffset;
               }
            });
         }
      }
   }
}

// Fixes edges such that unit before connected to after, is reconnected to new unit
void InsertUnit(Accelerator* accel, FUInstance* before, int beforePort, FUInstance* after, int afterPort, FUInstance* newUnit){
   LockAccelerator(accel,Accelerator::Locked::FREE);

   for(Edge* edge : accel->edges){
      if(edge->units[0].inst == before && edge->units[0].port == beforePort && edge->units[1].inst == after && edge->units[1].port == afterPort){
         ConnectUnits(newUnit,0,after,afterPort);

         edge->units[1].inst = newUnit;
         edge->units[1].port = 0;

         return;
      }
   }
}

void SendLatencyUpwards(FUInstance* inst){
   int b = inst->tempData->inputDelay;

   for(int i = 0; i < inst->tempData->numberInputs; i++){
      ConnectionInfo* info = &inst->tempData->inputs[i];

      int a = inst->declaration->inputDelays[info->port];
      int e = inst->tempData->inputs[info->port].delay;

      FUInstance* other = inst->tempData->inputs[i].inst.inst;
      for(int ii = 0; ii < other->tempData->numberOutputs; ii++){
         ConnectionInfo* otherInfo = &other->tempData->outputs[ii];

         int c = other->declaration->latencies[info->inst.port];

         if(info->inst.inst == other && info->inst.port == otherInfo->port &&
            otherInfo->inst.inst == inst && otherInfo->inst.port == info->port){
            otherInfo->delay = b + a + e - c;
         }
      }
   }
}

void CalculateDelay(Versat* versat,Accelerator* accel){
   #define OUTPUT_DOT 1
   static int graphs = 0;
   LockAccelerator(accel,Accelerator::Locked::ORDERED);

   DAGOrder order = accel->order;

   // Clear everything, just in case
   for(FUInstance* inst : accel->instances){
      inst->tempData->inputDelay = 0;

      for(int i = 0; i < inst->tempData->numberOutputs; i++){
         inst->tempData->outputs[i].delay = 0;
      }
   }

   for(int i = 0; i < order.numberSinks; i++){
      FUInstance* inst = order.sinks[i];

      SendLatencyUpwards(inst);
      #if OUTPUT_DOT == 1
      OutputGraphDotFile(accel,false,"debug/out1_%d.dot",graphs++);
      #endif
   }

   for(int i = accel->instances.Size() - order.numberSinks - 1; i >= 0; i--){
      FUInstance* inst = order.instances.ptr[i];

      if(inst->tempData->nodeType == GraphComputedData::TAG_UNCONNECTED){
         continue;
      }

      int minimum = (1 << 30);
      for(int ii = 0; ii < inst->tempData->numberOutputs; ii++){
         minimum = mini(minimum,inst->tempData->outputs[ii].delay);
      }

      for(int ii = 0; ii < inst->tempData->numberOutputs; ii++){
         inst->tempData->outputs[ii].delay -= minimum;
      }

      inst->tempData->inputDelay = minimum;
      inst->baseDelay = abs(inst->tempData->inputDelay);

      SendLatencyUpwards(inst);
      #if OUTPUT_DOT == 1
      OutputGraphDotFile(accel,false,"debug/out2_%d.dot",graphs++);
      #endif
   }

   int minimum = 0;
   for(FUInstance* inst : accel->instances){
      minimum = mini(minimum,inst->tempData->inputDelay);
   }

   minimum = abs(minimum);
   for(FUInstance* inst : accel->instances){
      inst->tempData->inputDelay += minimum;
   }

   for(FUInstance* inst : accel->instances){
      if(inst->tempData->nodeType == GraphComputedData::TAG_SOURCE_AND_SINK){
         for(int ii = 0; ii < inst->tempData->numberOutputs; ii++){
            inst->tempData->outputs[ii].delay = 0;
         }
      }
      inst->baseDelay = inst->tempData->inputDelay;
   }

   #if 1
   //accel->locked = Accelerator::Locked::FREE; // Hackish way of adding new instances but keeping previous ordering data
   LockAccelerator(accel,Accelerator::Locked::FREE,false);
   // Insert delay units if needed
   int delaysInserted = 0;
   for(int i = accel->instances.Size() - 1; i >= 0; i--){
      FUInstance* inst = order.instances.ptr[i];

      for(int ii = 0; ii < inst->tempData->numberOutputs; ii++){
         ConnectionInfo* info = &inst->tempData->outputs[ii];

         if(info->delay != 0){
            char buffer[128];
            int size = sprintf(buffer,"delay%d",delaysInserted++);

            FUInstance* newInst = CreateFUInstance(accel,versat->delay,MakeSizedString(buffer,size));

            InsertUnit(accel,inst,info->port,info->inst.inst,info->inst.port,newInst);

            newInst->baseDelay = info->delay - versat->delay->latencies[0];
            newInst->isStatic = true; // By default all delays are static

            if(newInst->config){
               newInst->config[0] = newInst->baseDelay;
            }
            Assert(newInst->baseDelay >= 0);
         }
      }
   }
   LockAccelerator(accel,Accelerator::Locked::ORDERED);

   for(FUInstance* inst : accel->instances){
      inst->tempData->inputDelay = inst->baseDelay;


      #if 0
      if(inst->declaration->type != FUDeclaration::COMPOSITE && inst->delay){
         inst->delay[0] = inst->baseDelay;
      }
      #endif
   }
   #endif

   #if OUTPUT_DOT == 1
   OutputGraphDotFile(accel,false,"debug/out3_%d.dot",graphs++);
   #endif
}

void SetDelayRecursive_(FUInstance* inst,int delay){
   if(inst->declaration == inst->accel->versat->delay){
      inst->config[0] = inst->baseDelay;
      return;
   }

   int totalDelay = inst->baseDelay + delay;

   if(inst->declaration->nDelays){
      inst->delay[0] = totalDelay;
   }

   if(inst->declaration->type == FUDeclaration::COMPOSITE){
      for(FUInstance* child : inst->compositeAccel->instances){
         SetDelayRecursive_(child,totalDelay);
      }
   }
}

void SetDelayRecursive(Accelerator* accel){
   for(FUInstance* inst : accel->instances){
      SetDelayRecursive_(inst,0);
   }
}

#if 0
static int NodeMappingConflict(Edge edge1,Edge edge2){
   FUInstance* list[4] = {edge1.units[0].inst,edge1.units[1].inst,edge2.units[0].inst,edge2.units[1].inst};
   FUInstance* instances[4] = {0,0,0,0};

   if(!(edge1.units[0].port == edge2.units[0].port && edge1.units[1].port == edge2.units[1].port)){
      return 0;
   }

   for(int i = 0; i < 4; i++){
      for(int ii = 0; ii < 4; ii++){
         if(instances[ii] == list[i]){
            return 1;
         } else if(instances[i] == 0){
            instances[ii] = list[i];
            break;
         }
      }
   }

   return 0;
}

static int MappingConflict(MappingNode map1,MappingNode map2){
   int res = (NodeMappingConflict(map1.edges[0],map1.edges[1]) ||
              NodeMappingConflict(map1.edges[0],map2.edges[0]) ||
              NodeMappingConflict(map1.edges[0],map2.edges[1]) ||
              NodeMappingConflict(map1.edges[1],map2.edges[0]) ||
              NodeMappingConflict(map1.edges[1],map2.edges[1]) ||
              NodeMappingConflict(map2.edges[0],map2.edges[1]));

   return res;
}

ConsolidationGraph GenerateConsolidationGraph(Accelerator* accel1,Accelerator* accel2){
   ConsolidationGraph graph = {};

   graph.nodes = (MappingNode*) malloc(sizeof(MappingNode) * 1024);
   graph.edges = (MappingEdge*) malloc(sizeof(MappingEdge) * 1024);

   //LockAccelerator(accel1);
   //LockAccelerator(accel2);

   // Check node mapping
   #if 0
   for(int i = 0; i < accel1->nInstances; i++){
      for(int ii = 0; ii < accel2->nInstances; ii++){
         FUInstance* inst1 = &accel1->instances[i];
         FUInstance* inst2 = &accel2->instances[ii];

         if(inst1->declaration == inst2->declaration){
            MappingNode node = {};

            node.edges[0].inst[0] = inst1;
            node.edges[0].inst[1] = inst1;
            node.edges[1].inst[0] = inst2;
            node.edges[1].inst[1] = inst2;

            graph.nodes[graph.numberNodes++] = node;
         }
      }
   }
   #endif

   #if 0
   // Check possible edges
   for(int i = 0; i < accel1->nInstances; i++){
      for(int ii = 0; ii < accel2->nInstances; ii++){
         FUInstance* inst1 = &accel1->instances[i];
         FUInstance* inst2 = &accel2->instances[ii];

         for(int iii = 0; iii < inst1->tempData->numberOutputs; iii++){
            for(int iv = 0; iv < inst2->tempData->numberOutputs; iv++){
               FUInstance* other1 = inst1->tempData->outputs[iii].inst;
               FUInstance* other2 = inst2->tempData->outputs[iv].inst;

               // WRONG, INSTANCES CAN HAVE MULTIPLE PORTS CONNECTED
               // THIS WAY ONLY HAD 1 POSSIBLE

               PortEdge port1 = GetPort(inst1,other1);
               PortEdge port2 = GetPort(inst2,other2);

               if(inst1->declaration == inst2->declaration &&
                  other1->declaration == other2->declaration &&
                  port1.inPort == port2.inPort &&
                  port1.outPort == port2.outPort){

                  MappingNode node = {};

                  node.edges[0].inst[0] = inst1;
                  node.edges[0].inst[1] = other1;
                  node.edges[0].inPort = port1.inPort;
                  node.edges[0].outPort = port1.outPort;
                  node.edges[1].inst[0] = inst2;
                  node.edges[1].inst[1] = other2;
                  node.edges[1].inPort = port1.inPort;
                  node.edges[1].outPort = port1.outPort;

                  graph.nodes[graph.numberNodes++] = node;
               }
            }
         }
      }
   }

   // Check edge conflicts
   for(int i = 0; i < graph.numberNodes; i++){
      for(int ii = 0; ii < graph.numberNodes; ii++){
         if(i == ii){
            continue;
         }

         MappingNode node1 = graph.nodes[i];
         MappingNode node2 = graph.nodes[ii];

         if(MappingConflict(node1,node2)){
            continue;
         }

         MappingEdge edge = {};

         edge.nodes[0] = node1;
         edge.nodes[1] = node2;

         graph.edges[graph.numberEdges++] = edge;
      }
   }
   #endif

   return graph;
}

int EdgeEqual(Edge edge1,Edge edge2){
   int res = (memcmp(&edge1,&edge2,sizeof(Edge)) == 0);

   return res;
}

int MappingNodeEqual(MappingNode node1,MappingNode node2){
   int res = (EdgeEqual(node1.edges[0],node2.edges[0]) &&
              EdgeEqual(node1.edges[1],node2.edges[1]));

   return res;
}

void OnlyNeighbors(ConsolidationGraph* graph,int validNode){
   int neighbors[1024];

   MappingNode* node = &graph->nodes[validNode];

   for(int i = 0; i < 1024; i++){
      neighbors[i] = 0;
   }

   for(int i = 0; i < graph->numberEdges; i++){
      MappingEdge* edge = &graph->edges[i];

      if(MappingNodeEqual(edge->nodes[0],*node)){
         for(int j = 0; j < graph->numberNodes; j++){
            if(MappingNodeEqual(edge->nodes[1],graph->nodes[j])){
               neighbors[j] = 1;
            }
         }
      }
      if(MappingNodeEqual(edge->nodes[1],*node)){
         for(int j = 0; j < graph->numberNodes; j++){
            if(MappingNodeEqual(edge->nodes[0],graph->nodes[j])){
               neighbors[j] = 1;
            }
         }
      }
   }

   for(int i = 0; i < graph->numberNodes; i++){
      graph->validNodes[i] &= neighbors[i];
   }
}

ConsolidationGraph Copy(ConsolidationGraph graph){
   ConsolidationGraph res = {};

   res.nodes = (MappingNode*) calloc(graph.numberNodes,sizeof(MappingNode));
   memcpy(res.nodes,graph.nodes,sizeof(MappingNode) * graph.numberNodes);
   res.numberNodes = graph.numberNodes;

   res.edges = (MappingEdge*) calloc(graph.numberEdges,sizeof(MappingEdge));
   memcpy(res.edges,graph.edges,sizeof(MappingEdge) * graph.numberEdges);
   res.numberEdges = graph.numberEdges;

   res.validNodes = (int*) calloc(graph.numberNodes,sizeof(int));
   memcpy(res.validNodes,graph.validNodes,sizeof(int) * graph.numberNodes);

   return res;
}

int NumberNodes(ConsolidationGraph graph){
   int num = 0;
   for(int i = 0; i < graph.numberNodes; i++){
      if(graph.validNodes[i]){
         num += 1;
      }
   }

   return num;
}

static int max = 0;
static int found = 0;
static int table[1024];
static ConsolidationGraph clique;

void Clique(ConsolidationGraph graphArg,int size){
   if(NumberNodes(graphArg) == 0){
      if(size > max){
         max = size;
         clique = graphArg;
         found = true;
      }
      return;
   }

   ConsolidationGraph graph = Copy(graphArg);

   int num = 0;
   while((num = NumberNodes(graph)) != 0){
      if(size + num <= max){
         return;
      }

      int i;
      for(i = 0; i < graph.numberNodes; i++){
         if(graph.validNodes[i]){
            break;
         }
      }

      if(size + table[i] <= max){
         return;
      }

      OnlyNeighbors(&graph,i);
      Clique(graph,size + 1);
      if(found == true){
         return;
      }
   }
}

ConsolidationGraph MaxClique(ConsolidationGraph graph){
   ConsolidationGraph res = {};

   max = 0;
   found = 0;
   for(int i = 0; i < 1024; i++){
      table[i] = 0;
   }

   graph.validNodes = (int*) calloc(sizeof(int),graph.numberNodes);
   for(int i = graph.numberNodes - 1; i >= 0; i--){
      for(int j = 0; j < graph.numberNodes; j++){
         graph.validNodes[j] = (i <= j ? 1 : 0);
         printf("%d ",graph.validNodes[j]);
      }
      printf("\n");

      OnlyNeighbors(&graph,i);
      graph.validNodes[i] = 1;

      for(int j = 0; j < graph.numberNodes; j++){
         printf("%d ",graph.validNodes[j]);
      }
      printf("\n");
      printf("\n");

      Clique(graph,1);

      table[i] = max;
   }

   return graph;
}

void AddMapping(Mapping* mappings, FUInstance* source,FUInstance* sink){
   for(int i = 0; i < 1024; i++){
      if(mappings[i].source == nullptr){
         mappings[i].source = source;
         mappings[i].sink = sink;
         return;
      }
   }
}

Mapping* GetMapping(Mapping* mappings, FUInstance* source){
   for(int i = 0; i < 1024; i++){
      if(mappings[i].source == source){
         return &mappings[i];
      }
   }

   return nullptr;
}

FUInstance* NodeMapped(FUInstance* inst, ConsolidationGraph graph){
   for(int i = 0; i < graph.numberNodes; i++){
      MappingNode node = graph.nodes[i];

      if(!graph.validNodes[i]){
         continue;
      }

      if(node.edges[0].units[0].inst == node.edges[0].units[1].inst){ // Node mapping
         if(node.edges[0].units[0].inst == inst){
            return node.edges[1].units[0].inst;
         }
         if(node.edges[1].units[0].inst == inst){
            return node.edges[0].units[0].inst;
         }
      } else { // Edge mapping
         if(node.edges[0].units[0].inst == inst){
            return node.edges[1].units[0].inst;
         }
         if(node.edges[0].units[1].inst == inst){
            return node.edges[1].units[1].inst;
         }
         if(node.edges[1].units[0].inst == inst){
            return node.edges[0].units[0].inst;
         }
         if(node.edges[1].units[1].inst == inst){
            return node.edges[0].units[1].inst;
         }
      }
   }

   return nullptr;
}

Accelerator* MergeGraphs(Versat* versat,Accelerator* accel1,Accelerator* accel2,ConsolidationGraph graph){
   Mapping* graphToFinal = (Mapping*) calloc(sizeof(Mapping),1024);

   InstanceMap map = {};

   Accelerator* newGraph = CreateAccelerator(versat);

   //LockAccelerator(accel1);
   //LockAccelerator(accel2);

   // Create base instances (accel 1)
   for(FUInstance* inst : accel1->instances){
      FUInstance* newNode = CreateFUInstance(newGraph,inst->declaration);

      map.insert({inst,newNode});
   }

   #if 1
   for(FUInstance* inst : accel2->instances){
      FUInstance* mappedNode = NodeMapped(inst,graph); // Returns node in graph 1

      if(mappedNode){
         mappedNode = map.find(mappedNode)->second;
      } else {
         mappedNode = CreateFUInstance(newGraph,inst->declaration);
      }

      AddMapping(graphToFinal,inst,mappedNode);
   }
   #endif

   // Add edges from accel1
   for(FUInstance* inst : accel1->instances){
      for(int ii = 0; ii < inst->tempData->numberOutputs; ii++){
         FUInstance* other = inst->tempData->outputs[ii].inst.inst;

         FUInstance* mappedInst = map.find(inst)->second;
         FUInstance* mappedOther = map.find(other)->second;

         //PortEdge portEdge = GetPort(inst,other);

         //ConnectUnits(mappedInst,portEdge.outPort,mappedOther,portEdge.inPort);
      }
   }

   #if 1
   for(FUInstance* inst : accel2->instances){
      for(int ii = 0; ii < inst->tempData->numberOutputs; ii++){
         FUInstance* other = inst->tempData->outputs[ii].inst.inst;

         FUInstance* mappedInst =  map.find(inst)->second;
         FUInstance* mappedOther = map.find(other)->second;

         //PortEdge portEdge = GetPort(inst,other);

         //ConnectUnits(mappedInst,portEdge.outPort,mappedOther,portEdge.inPort);
      }
   }
   #endif

   return newGraph;
}
#endif

#undef NUMBER_OUTPUTS
#undef OUTPUT

#include "verilogParser.hpp"

static void T(Accelerator* accel){
   for(FUInstance* inst : accel->instances){
      printf("%s %p\n",inst->name.str,inst->memMapped);

      if(inst->declaration->type == FUDeclaration::COMPOSITE){
         T(inst->compositeAccel);
      }
   }
}

void Hook(Versat* versat,Accelerator* accel,FUInstance* inst){
   Free(versat);

   #if 0
   VisitAcceleratorInstances(accel,[](FUInstance* inst){
      printf("%s\n",GetHierarchyNameRepr(inst->name));
   });
   #endif
}


























