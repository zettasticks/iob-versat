#include <unordered_map>
#include <sys/wait.h>
#include <ftw.h>

#include "debugVersat.hpp"
#include "declaration.hpp"
#include "filesystem.hpp"
#include "globals.hpp"
#include "logger.hpp"
#include "memory.hpp"
#include "structParsing.hpp"
#include "utilsCore.hpp"
#include "versat.hpp"
#include "utils.hpp"
#include "verilogParsing.hpp"
#include "type.hpp"
#include "templateEngine.hpp"
#include "textualRepresentation.hpp"
#include "codeGeneration.hpp"
#include "accelerator.hpp"
#include "dotGraphPrinting.hpp"
#include "versatSpecificationParser.hpp"
#include "symbolic.hpp"

#include <filesystem>
namespace fs = std::filesystem;

// TODO: For some reason, in the makefile we cannot stringify the arguments that we want to
//       Do not actually want to do these preprocessor things. Fix the makefile so that it passes as a string
#define DO_STRINGIFY(ARG) #ARG
#define STRINGIFY(ARG) DO_STRINGIFY(ARG)

// TODO: Small fix for common template. Works for now 
void SetIncludeHeader(CompiledTemplate* tpl,String name);

#include "parser.hpp"

static Value HexValue(Value in,Arena* out){
  static char buffer[128];

  int number = ConvertValue(in,ValueType::NUMBER,nullptr).number;

  int size = sprintf(buffer,"0x%x",number);

  Value res = {};
  res.type = ValueType::STRING;
  res.str = PushString(out,String{buffer,size});

  return res;
}

static Value Identify(Value val,Arena* out){
  //Assert(val.type == GetType(STRING("FUInstance*")));

  FUInstance* inst;
  if(val.type == GetType(STRING("FUInstance*"))){
    FUInstance** instPtr = (FUInstance**) val.custom;
    inst = *instPtr;
  } else if(val.type == GetType(STRING("FUInstance"))){
    inst = (FUInstance*) val.custom;
  } else {
    Assert(false);
  }
  
  String ret = PushString(out,"%.*s_%d",UNPACK_SS(inst->name),inst->id);
  
  return MakeValue(ret);
}

#include "templateData.hpp"

void LoadTemplates(Arena* perm,Arena* temp){
  CompiledTemplate* commonTpl = CompileTemplate(versat_common_template,"common",perm,temp);
  SetIncludeHeader(commonTpl,STRING("common"));

  BasicTemplates::acceleratorTemplate = CompileTemplate(versat_accelerator_template,"accel",perm,temp);
  BasicTemplates::topAcceleratorTemplate = CompileTemplate(versat_top_instance_template,"top",perm,temp);
  BasicTemplates::topConfigurationsTemplate = CompileTemplate(versat_configurations_template,"top_configurations",perm,temp);
  BasicTemplates::acceleratorHeaderTemplate = CompileTemplate(versat_header_template,"header",perm,temp);
  BasicTemplates::externalInternalPortmapTemplate = CompileTemplate(external_memory_internal_portmap_template,"ext_internal_port",perm,temp);
  BasicTemplates::externalPortTemplate = CompileTemplate(external_memory_port_template,"ext_port",perm,temp);
  BasicTemplates::externalInstTemplate = CompileTemplate(external_memory_inst_template,"ext_inst",perm,temp);
  BasicTemplates::iterativeTemplate = CompileTemplate(versat_iterative_template,"iter",perm,temp);
  BasicTemplates::internalWiresTemplate = CompileTemplate(versat_internal_memory_wires_template,"internal wires",perm,temp);
  
  RegisterPipeOperation(STRING("MemorySize"),[](Value val,Arena* out){
    ExternalMemoryInterface* inter = (ExternalMemoryInterface*) val.custom;
    int byteSize = ExternalMemoryByteSize(inter);
    return MakeValue(byteSize);
  });
  RegisterPipeOperation(STRING("Hex"),HexValue);
  RegisterPipeOperation(STRING("Identify"),Identify);
  RegisterPipeOperation(STRING("Type"),[](Value val,Arena* out){
    Type* type = val.type;

    return MakeValue(type->name);
  });
}

#include <argp.h>

struct OptionsGather{
  ArenaList<String>* verilogFiles;
  ArenaList<String>* extraSources;
  ArenaList<String>* includePaths;
  ArenaList<String>* unitPaths;

  Options* options;
};

static int
parse_opt (int key, char *arg,
           argp_state *state)
{
  OptionsGather* opts = (OptionsGather*) state->input;

  // TODO: Better error handling
  switch (key)
    {
    case 'S': *PushListElement(opts->extraSources) = STRING(arg); break;
    case 'I': *PushListElement(opts->includePaths) = STRING(arg); break;
    case 'u': *PushListElement(opts->unitPaths) = STRING(arg); break;

    case 'b': opts->options->databusDataSize = ParseInt(STRING(arg)); break;
    case 'x': opts->options->databusAddrSize = ParseInt(STRING(arg)); break;

    case 'd': opts->options->useDMA = true; break;
    case 'D': opts->options->architectureHasDatabus = true; break;
    case 's': opts->options->addInputAndOutputsToTop = true; break;

    case 'p': opts->options->copyUnitsConvenience = true; break;
    case 'L': opts->options->generetaSourceListName = STRING(arg); break;

    case 'g': opts->options->debugPath = STRING(arg); opts->options->debug = true; break;
    case 't': opts->options->topName = STRING(arg); break;
    case 'o': opts->options->hardwareOutputFilepath = STRING(arg); break;
    case 'O': opts->options->softwareOutputFilepath = STRING(arg); break;
      
    case ARGP_KEY_ARG: opts->options->specificationFilepath = STRING(arg); break;
    case ARGP_KEY_END: break;
    }
  return 0;
}

template<typename Key,typename Data>
struct MyHashmapIterator{
  HashmapIterator<Key,Data> iter;
  HashmapIterator<Key,Data> end;

  bool HasNext(){
    return (iter != end);
  };

  Pair<Key,Data*> Next(){
    auto res = *iter;
    ++iter;
    return res;
  };
};

template<typename Key,typename Data>
MyHashmapIterator<Key,Data> Iterate(Hashmap<Key,Data>* hashmap){
  MyHashmapIterator<Key,Data> res = {};

  res.iter = begin(hashmap);
  res.end  = end(hashmap);
  return res;
}

template<typename Key,typename Data>
MyHashmapIterator<Key,Data> Iterate(Hashmap<Key,Data>& hashmap){
  MyHashmapIterator<Key,Data> res = {};

  res.iter = begin(&hashmap);
  res.end  = end(&hashmap);
  return res;
}

template struct MyHashmapIterator<int,int>;
template MyHashmapIterator<int,int> Iterate(Hashmap<int,int>* hashmap);
template MyHashmapIterator<int,int> Iterate(Hashmap<int,int>& hashmap);

template struct MyHashmapIterator<String,Array<Token>>;
template MyHashmapIterator<String,Array<Token>> Iterate(Hashmap<String,Array<Token>>* hashmap);
template MyHashmapIterator<String,Array<Token>> Iterate(Hashmap<String,Array<Token>>& hashmap);

struct GenericArrayIterator{
  void* array;
  int sizeOfType;
  int alignmentOfType;
  int index;
};

GenericArrayIterator IterateArray(void* array,int sizeOfType,int alignmentOfType){
  GenericArrayIterator res = {};
  res.array = array;
  res.sizeOfType = sizeOfType;
  res.alignmentOfType = alignmentOfType;

  return res;
}

bool HasNext(GenericArrayIterator iter){
  //Array<int>* view = (*(Array<int>**) iter.array);
  void** arrayData = (void**) iter.array;
  int* size = (int*) (arrayData + 1);

  return iter.index < *size;
}

void* Next(GenericArrayIterator& iter){
  //Array<int>* view = (*(Array<int>**) iter.array);
  //void* arrayStart = (void*) view->data; 

  void** arrayData = (void**) iter.array;
  void* arrayStart = (void*) arrayData[0]; 

  char* view = (char*) arrayStart;
  void* data = (void*) &view[iter.index * iter.sizeOfType];
  iter.index += 1;
  
  return data;
}

struct GenericHashmapIterator{
  void* hashmap;
  int sizeOfType;
  int alignmentOfType;
  int index;
};

GenericHashmapIterator IterateHashmap(void* hashmap,int sizeOfType,int alignmentOfType){
  GenericHashmapIterator res = {};
  res.hashmap = hashmap;
  res.sizeOfType = sizeOfType;
  res.alignmentOfType = alignmentOfType;

  return res;
}

bool HasNext(GenericHashmapIterator iter){
  Hashmap<int,int>* view = ((Hashmap<int,int>*) iter.hashmap);

  return iter.index < view->nodesUsed;
}

void* Next(GenericHashmapIterator& iter){
  Hashmap<int,int>* view = ((Hashmap<int,int>*) iter.hashmap);

  char* arrayView = (char*) view->data;
  void* data = (void*) &arrayView[iter.index * iter.sizeOfType];
  iter.index += 1;
  return data;
}

// Only for graphs that we know for sure are DAG
Array<int> CalculateDAG(int maxNode,Array<Pair<int,int>> edges,int start,Arena* out,Arena* temp){
  BLOCK_REGION(temp);

  int size = maxNode;
  Stack<int>* toSee = PushQueue<int>(temp,size * 2);
  Array<int> marked = PushArray<int>(temp,size);
  Memset(marked,0);
  
  toSee->Push(start);

  int NOT_SEEN = 0;
  int WAIT_CHILDREN = 1;
  int PERMANENT = 2;
    
  DynamicArray<int> arr = StartArray<int>(out);
  while(toSee->Size()){
    int head = toSee->Pop();

    if(marked[head] == WAIT_CHILDREN){
      marked[head] = PERMANENT;
      *arr.PushElem() = head;
      continue;
    }
    
    if(marked[head]){
      continue;
    }

    bool firstSon = true;
    for(auto p : edges){
      if(p.first == head){
        if(!marked[p.second]){
          if(firstSon){
            firstSon = false;
            marked[head] = WAIT_CHILDREN;
            // Basically using the stack to accomplish two things. Store the children and to store another visit to the parent node, which is now marked with WAIT_CHILDREN and we only arrive at this after we have seen all the children.
            // The parent node can only be visit after all the children have been visited, since we are working on graphs that we know are DAG. 
            toSee->Push(head); // So we are sure to reach the WAIT_CHILDREN check
          }
          toSee->Push(p.second);
        }
      }
    }

    // No sons
    if(firstSon == true){
      marked[head] = PERMANENT;
      *arr.PushElem() = head;
    }
  }
  return EndArray(arr);
}

// This structure needs to represent the entire work that is required to perform 
struct Work{
  TypeDefinition definition;

  bool calculateDelayFixedGraph;
  bool flattenWithMapping;
};

void Print(Work* work){
  printf("Name: %.*s\n",UNPACK_SS(work->definition.base.name));
  printf("calculateDelayFixedGraph: %d\n",work->calculateDelayFixedGraph ? 1 : 0);
  printf("flattenWithMapping: %d\n",work->flattenWithMapping ? 1 : 0);
}

void GetSubWorkRequirement(Hashmap<String,Work>* typeToWork,TypeDefinition type,Arena* temp,Arena* temp2){
  BLOCK_REGION(temp2);
  Array<Token> subTypesUsed = TypesUsed(type,temp,temp2);
  
  for(Token tok : subTypesUsed){
    Work* work = typeToWork->Get(tok);
    if(!work){
      continue;
    }

    // For now, assume that we want everything
    work->calculateDelayFixedGraph = true;
    work->flattenWithMapping = true;
  }
}

// TODO: Better error handling
struct argp_option options[] =
  {
    { 0, 'S',"File", 0, "Extra sources"},
    { 0, 'b',"Size", 0, "Databus size connected to external memory (8,16,default:32,64,128,256)"},
    { 0, 'x',"Size", 0, "Address size (default:32,64)"},
    { 0, 'd', 0,     0, "Use DMA"},
    { 0, 'D', 0,     0, "Architecture has databus"},
    { 0, 'I',"Path", 0, "Include paths"},
    { 0, 't',"Name", 0, "Top unit name"},
    { 0, 'p', 0,     0, "Versat also copies the source files of the units, including custom ones"},
    { 0, 'L',"Name", 0, "Writes to a file a list of all files generated by Versat"},
    { 0, 's', 0,     0, "Insert consts and regs if Top unit contains inputs and outputs"},
    { 0, 'u',"Path", 0, "Units path"},
    { 0, 'g',"Path", OPTION_HIDDEN, "Debug path"},
    { 0, 'o',"Path", 0, "Hardware output path"},
    { 0, 'O',"Path", 0, "Software output path"},
    { 0 }
  };

int main(int argc,char* argv[]){
  InitDebug();
  
  *globalPermanent = InitArena(Megabyte(128));
  Arena* perm = globalPermanent;
  Arena tempInst = InitArena(Megabyte(128));
  Arena* temp = &tempInst;
  Arena temp2Inst = InitArena(Megabyte(128));
  Arena* temp2 = &temp2Inst;
  
  argp argp = { options, parse_opt, "SpecFile", "Dataflow to accelerator compiler. Check tutorial in https://github.com/IObundle/iob-versat to learn how to write a specification file"};

  OptionsGather gather = {};
  gather.verilogFiles = PushArenaList<String>(temp);
  gather.extraSources = PushArenaList<String>(temp);
  gather.includePaths = PushArenaList<String>(temp);
  gather.unitPaths = PushArenaList<String>(temp);

  globalOptions = DefaultOptions(perm);
  gather.options = &globalOptions;

  if(argp_parse(&argp, argc, argv, 0, 0, &gather) != 0){
    printf("Error parsing arguments. Call -h help to print usage and argument help\n");
    return -1;
  }

  if(Empty(globalOptions.topName)){
    printf("Need to specify top unit with -t\n");
    exit(-1);
  }

  globalOptions.verilogFiles = PushArrayFromList(perm,gather.verilogFiles);
  globalOptions.extraSources = PushArrayFromList(perm,gather.extraSources);
  globalOptions.includePaths = PushArrayFromList(perm,gather.includePaths);
  globalOptions.unitPaths = PushArrayFromList(perm,gather.unitPaths);
  
  RegisterTypes();
  
  InitializeTemplateEngine(perm);
  LoadTemplates(perm,temp);
  InitializeSimpleDeclarations();

  globalDebug.outputAccelerator = true;
  globalDebug.outputAcceleratorInfo = true;
  globalDebug.outputVersat = true;
  globalDebug.outputGraphs = true;
  globalDebug.outputConsolidationGraphs = true;
  globalDebug.outputVCD = true;

  // Collect all verilog source files.
  Array<String> allVerilogFiles = {};
  region(temp){
    Set<String>* allVerilogFilesSet = PushSet<String>(temp,999);

    for(String str : globalOptions.verilogFiles){
      allVerilogFilesSet->Insert(str);
    }
    for(String& path : globalOptions.unitPaths){
      String dirPaths = path;
      Tokenizer pathSplitter(dirPaths,"",{});

      while(!pathSplitter.Done()){
        Token path = pathSplitter.NextToken();

        Opt<Array<String>> res = GetAllFilesInsideDirectory(path,temp);
        if(!res){
          printf("\n\nCannot open dir: %.*s\n\n",UNPACK_SS(path));
        } else {
          for(String& str : res.value()){
            String fullPath = PushString(perm,"%.*s/%.*s",UNPACK_SS(path),UNPACK_SS(str));
            allVerilogFilesSet->Insert(fullPath);
          }
        }
      }
    }

    allVerilogFiles = PushArrayFromSet(perm,allVerilogFilesSet);
  }
  globalOptions.verilogFiles = allVerilogFiles; // TODO: Kind of an hack. We lose information about file origin (from user vs from us)
  
  // Parse verilog files and register all the simple units.
  for(String filepath : globalOptions.verilogFiles){
    String content = PushFile(temp,filepath);

    if(Empty(content)){
      printf("Failed to open file %.*s\n. Exiting\n",UNPACK_SS(filepath));
      exit(-1);
    }

    String processed = PreprocessVerilogFile(content,globalOptions.includePaths,temp,temp2);
    Array<Module> modules = ParseVerilogFile(processed,globalOptions.includePaths,temp,temp2);
    
    for(Module& mod : modules){
      ModuleInfo info = ExtractModuleInfo(mod,perm,temp);
      RegisterModuleInfo(&info,temp);
    }
  }
  
  // We need to do this after parsing the modules because the majority of these special types come from verilog files
  BasicDeclaration::buffer = GetTypeByName(STRING("Buffer"));
  BasicDeclaration::fixedBuffer = GetTypeByName(STRING("FixedBuffer"));
  BasicDeclaration::pipelineRegister = GetTypeByName(STRING("PipelineRegister"));
  BasicDeclaration::multiplexer = GetTypeByName(STRING("Mux2"));
  BasicDeclaration::combMultiplexer = GetTypeByName(STRING("CombMux2"));
  BasicDeclaration::stridedMerge = GetTypeByName(STRING("StridedMerge"));
  BasicDeclaration::timedMultiplexer = GetTypeByName(STRING("TimedMux"));
  BasicDeclaration::input = GetTypeByName(STRING("CircuitInput"));
  BasicDeclaration::output = GetTypeByName(STRING("CircuitOutput"));

  String specFilepath = globalOptions.specificationFilepath;
  String topLevelTypeStr = globalOptions.topName;

  // Basically using a simple DAG approach to detect the modules that we only care about. We do not process modules that are not needed

  // TODO: Simplify this part. 
  FUDeclaration* simpleType = GetTypeByName(topLevelTypeStr);
  
  if(!simpleType && specFilepath.size && !CompareString(topLevelTypeStr,"VERSAT_RESERVED_ALL_UNITS")){
    String content = PushFile(temp,StaticFormat("%.*s",UNPACK_SS(specFilepath)));
    
    Array<TypeDefinition> types = ParseVersatSpecification(content,temp,temp2);
    int size = types.size;
    
    Hashmap<String,int>* typeToId = PushHashmap<String,int>(temp,size);
    for(int i = 0; i < size; i++){
      typeToId->Insert(types[i].base.name,i);
    }
    
    if(!typeToId->Exists(topLevelTypeStr)){
      printf("Did not find the top level type: %.*s\n",UNPACK_SS(topLevelTypeStr));
      return -1;
    }
    
    DynamicArray<Pair<int,int>> arr = StartArray<Pair<int,int>>(temp2);
    for(int i = 0; i < size; i++){
      Array<Token> subTypesUsed = TypesUsed(types[i],temp,temp2);

      for(String str : subTypesUsed){
        int* index = typeToId->Get(str);
        if(index){
          *arr.PushElem() = {i,*index};
        }
      }
    }
    Array<Pair<int,int>> edges = EndArray(arr);
    
    Array<int> order = CalculateDAG(size,edges,typeToId->GetOrFail(topLevelTypeStr),temp,temp2);

    // Represents all the work that we need to do.
    Hashmap<String,Work>* typeToWork = PushHashmap<String,Work>(temp,order.size);

    for(int i : order){
      Work work = {};
      String name = types[i].base.name;
      work.definition = types[i];
      
      typeToWork->Insert(name,work);
    }
    
    for(int i : order){
      TypeDefinition type = types[i];
      String name = type.base.name;
      GetSubWorkRequirement(typeToWork,type,temp,temp2);
    }

    // For the TOP unit, currently we do everything:
    Work* topWork = &typeToWork->GetOrFail(topLevelTypeStr);
    topWork->calculateDelayFixedGraph = true;
    topWork->flattenWithMapping = true;
    
    for(auto p : typeToWork){
      Work work = *p.second;

      FUDeclaration* decl = nullptr;
      
      if(work.definition.type == DefinitionType_MODULE){
        decl = InstantiateBarebonesSpecifications(content,p.second->definition,temp,temp2);
      } else if(work.definition.type == DefinitionType_MERGE){
        decl = InstantiateSpecifications(content,p.second->definition,temp,temp2);
      }
      decl->signalLoop = true;
      
#if 0
      if(work.calculateDelayFixedGraph){
        Accelerator* copy = CopyAccelerator(decl->baseCircuit,AcceleratorPurpose_FIXED_DELAY,true,nullptr);

        DAGOrderNodes order = CalculateDAGOrder(&copy->allocated,temp);
        CalculateDelayResult delays = CalculateDelay(copy,order,temp);

        decl->baseConfig.calculatedDelays = PushArray<int>(perm,delays.nodeDelay->nodesUsed);
        Memset(decl->baseConfig.calculatedDelays,0);
        int index = 0;
        for(Pair<FUInstance*,DelayInfo*> p : delays.nodeDelay){
          if(p.first->declaration->baseConfig.delayOffsets.max > 0){
            decl->baseConfig.calculatedDelays[index] = p.second->value;
            index += 1;
          }
        }

        region(temp){
          FixDelays(copy,delays.edgesDelay,temp);
        }

        decl->fixedDelayCircuit = copy;
        decl->fixedDelayCircuit->name = decl->name;

        FillDeclarationWithDelayType(decl);
      }
#endif

      if(work.flattenWithMapping){
        Pair<Accelerator*,SubMap*> p = Flatten2(decl->baseCircuit,99,temp);
  
        decl->flattenedBaseCircuit = p.first;
        decl->flattenMapping = p.second;
      }

      Print(p.second);
    }
  }

  FUDeclaration* type = GetTypeByName(topLevelTypeStr);
  if(!type && !CompareString(topLevelTypeStr,"VERSAT_RESERVED_ALL_UNITS")){
    printf("Did not find the top level type: %.*s\n",UNPACK_SS(topLevelTypeStr));
    return -1;
  }

  Accelerator* accel = nullptr;
  FUInstance* TOP = nullptr;

  if(CompareString(topLevelTypeStr,"VERSAT_RESERVED_ALL_UNITS")){
    accel = CreateAccelerator(STRING("allVersatUnits"),AcceleratorPurpose_MODULE);
    
    FUDeclaration* constType = GetTypeByName(STRING("Const"));
    FUDeclaration* regType = GetTypeByName(STRING("Reg"));

    int constsAdded = 0;
    int regsAdded = 0;
    for(FUDeclaration* decl : globalDeclarations){
      if(decl->type == FUDeclarationType_SINGLE){
        int input = decl->NumberInputs();
        int output = decl->NumberOutputs();

        FUInstance* testUnit = CreateFUInstance(accel,decl,decl->name);
        
        for(int i = 0; i < input; i++){
          String name = PushString(perm,"const%d",constsAdded++);
          FUInstance* inUnit = CreateFUInstance(accel,constType,name);

          ConnectUnits(inUnit,0,testUnit,i);
        }

        for(int i = 0; i < output; i++){
          String name = PushString(perm,"reg%d",regsAdded++);
          FUInstance* outUnit = CreateFUInstance(accel,regType,name);

          ConnectUnits(testUnit,i,outUnit,0);
        }
      }
    }
    
    type = RegisterSubUnit(accel,temp,temp2);
    type->signalLoop = true;

    accel = CreateAccelerator(topLevelTypeStr,AcceleratorPurpose_MODULE);
    TOP = CreateFUInstance(accel,type,STRING("TOP"));
  } else if(globalOptions.addInputAndOutputsToTop && !(type->NumberInputs() == 0 && type->NumberOutputs() == 0)){
    // TODO: This process needs to be simplified and more integrated with the approach used by versat spec.
    //       Instead of doing everything manually.

    const char* name = StaticFormat("%.*s_Simple",UNPACK_SS(topLevelTypeStr));
    accel = CreateAccelerator(STRING(name),AcceleratorPurpose_MODULE);
    
    int input = type->NumberInputs();
    int output = type->NumberOutputs();

    Array<FUInstance*> inputs = PushArray<FUInstance*>(perm,input);
    Array<FUInstance*> outputs = PushArray<FUInstance*>(perm,output);

    FUDeclaration* constType = GetTypeByName(STRING("TestConst"));
    FUDeclaration* regType = GetTypeByName(STRING("Reg"));
    
    // We need to create input and outputs first before instance
    // to guarantee that the configs and states are at the beginning of the accelerator structs
    for(int i = 0; i < input; i++){
      String name = PushString(perm,"input_%d",i);
      inputs[i] = CreateFUInstance(accel,constType,name);
    }
    for(int i = 0; i < output; i++){
      String name = PushString(perm,"output_%d",i);
      outputs[i] = CreateFUInstance(accel,regType,name);
    }

    TOP = CreateFUInstance(accel,type,STRING("simple"));

    for(int i = 0; i < input; i++){
      ConnectUnits(inputs[i],0,TOP,i);
    }
    for(int i = 0; i < output; i++){
      ConnectUnits(TOP,i,outputs[i],0);
    }

    FUInstance* node = TOP;
    
    type = RegisterSubUnit(accel,temp,temp2);
    type->definitionArrays = PushArray<Pair<String,int>>(perm,2);
    type->definitionArrays[0] = (Pair<String,int>){STRING("input"),input};
    type->definitionArrays[1] = (Pair<String,int>){STRING("output"),output};

    type->signalLoop = true;
    
    name = StaticFormat("%.*s_Simple",UNPACK_SS(topLevelTypeStr));
    accel = CreateAccelerator(STRING(name),AcceleratorPurpose_MODULE);
    TOP = CreateFUInstance(accel,type,STRING("TOP"));
  } else {
    accel = CreateAccelerator(topLevelTypeStr,AcceleratorPurpose_MODULE);

    TOP = CreateFUInstance(accel,type,STRING("TOP"));

    for(int i = 0; i < type->NumberInputs(); i++){
      String name = PushString(perm,"input%d",i);
      FUInstance* input = CreateOrGetInput(accel,name,i);
      ConnectUnits(input,0,TOP,i);
    }
    FUInstance* output = CreateOrGetOutput(accel);
    for(int i = 0; i < type->NumberOutputs(); i++){
      ConnectUnits(TOP,i,output,i);
    }
  }

  if(!SetParameter(TOP,STRING("AXI_ADDR_W"),STRING("AXI_ADDR_W"))){
    printf("Error\n");
  }
  SetParameter(TOP,STRING("LEN_W"),STRING("LEN_W"));
  
  OutputVersatSource(accel,
                     globalOptions.hardwareOutputFilepath.data,
                     globalOptions.softwareOutputFilepath.data,
                     globalOptions.addInputAndOutputsToTop,temp,temp2);

  OutputVerilatorWrapper(type,accel,globalOptions.softwareOutputFilepath,temp,temp2);

  String versatDir = STRING(STRINGIFY(VERSAT_DIR));
  OutputVerilatorMake(accel->name,versatDir,temp,temp2);

  ArenaList<String>* allFilesOutputted = PushArenaList<String>(temp2);
  
  for(FUDeclaration* decl : globalDeclarations){
    BLOCK_REGION(temp);

    if(decl->type == FUDeclarationType_COMPOSITE ||
       decl->type == FUDeclarationType_ITERATIVE ||
       decl->type == FUDeclarationType_MERGED){

      if(globalOptions.debug){
        GraphPrintingContent content = GenerateDefaultPrintingContent(decl->fixedDelayCircuit,temp,temp2);
        String repr = GenerateDotGraph(content,temp,temp2);
        String debugPath = PushDebugPath(temp,decl->name,STRING("NormalGraph.dot"));

        FILE* file = OpenFile(debugPath,"w",FilePurpose_DEBUG_INFO);
        DEFER_CLOSE_FILE(file);
        if(!file){
          printf("Error opening file for debug outputting: %.*s\n",UNPACK_SS(debugPath));
        } else {
          fwrite(repr.data,sizeof(char),repr.size,file);
        }
      }

      String path = PushString(temp,"%.*s/modules/%.*s.v",UNPACK_SS(globalOptions.hardwareOutputFilepath),UNPACK_SS(decl->name));

      *allFilesOutputted->PushElem() = PushString(temp2,"./modules/%.*s.v",UNPACK_SS(decl->name));
      
      FILE* sourceCode = OpenFileAndCreateDirectories(path,"w",FilePurpose_VERILOG_CODE);
      DEFER_CLOSE_FILE(sourceCode);

      if(decl->type == FUDeclarationType_COMPOSITE || decl->type == FUDeclarationType_MERGED){
        OutputCircuitSource(decl,sourceCode,temp,temp2);
      } else if(decl->type == FUDeclarationType_ITERATIVE){
        OutputIterativeSource(decl,sourceCode,temp,temp2);
      }
    }
  }

  // TODO: We probably need to move this to a better place since we also need to copy include files in order to separate build from setup. We need a lot of file copying and it's probably best to put all in one place.
  fs::path hardwareDestinationPath(StaticFormat("%.*s",UNPACK_SS(globalOptions.hardwareOutputFilepath)));
  auto options = fs::copy_options::update_existing;
  for(String filepath : globalOptions.verilogFiles){
    fs::path path(StaticFormat("%.*s",UNPACK_SS(filepath)));
    
    fs::copy(path,hardwareDestinationPath,options);
  }
  
  // This should be the last thing that we do, no further file creation can occur after this point
  Array<String> allOutputLocations = PushArrayFromList(temp,allFilesOutputted);

  for(FileInfo f : CollectAllFilesInfo(temp)){
    if(f.purpose == FilePurpose_VERILOG_INCLUDE && f.mode == FileOpenMode_WRITE){
      printf("Filename: %.*s Type: VERILOG_INCLUDE\n",UNPACK_SS(f.filepath));
    }
    if(f.purpose == FilePurpose_VERILOG_CODE && f.mode == FileOpenMode_WRITE){
      printf("Filename: %.*s Type: VERILOG_CODE\n",UNPACK_SS(f.filepath));
    }
    if(f.purpose == FilePurpose_MAKEFILE && f.mode == FileOpenMode_WRITE){
      printf("Filename: %.*s Type: MAKEFILE\n",UNPACK_SS(f.filepath));
    }
    if(f.purpose == FilePurpose_SOFTWARE && f.mode == FileOpenMode_WRITE){
      printf("Filename: %.*s Type: SOFTWARE\n",UNPACK_SS(f.filepath));
    }
    if(f.purpose == FilePurpose_MISC && f.mode == FileOpenMode_WRITE){
      printf("Filename: %.*s Type: MISC\n",UNPACK_SS(f.filepath));
    }
  }

  return 0;
}

/*

Currently we removed a lot of the cruft that we had with the baseConfig and the configInfo approach. There is probably a lot of code that can be simplified, based on the code that we already have simplified. Functions that previously worked on graph data and such could now be rewritten to work from the AccelInfo structure.

I think that is the best approach, because if we make the AccelInfo the source of truth, then debugging just becomes inspecting the AccelInfo structure and seeing if anything is being miscalculated (as well as some graph inspections and the likes, but that is mostly solved by now).

*/

/*

What do I have to do next?

We need to reconciliate the DelayCalculation function that operates normaly and the one that uses "partitions".
Which basically means that we need to pass a AccelInfoIterator and to build DelayCalculation on top of that.
 
What this means is that the flow is gonna be something like:
  Create an AccelInfo.
  Populate AccelInfo with all the info that we currently have (from subunits and the likes).
  Call DelayCalculation with an iterator for that AccelInfo.
  
  We might need to store on InstanceInfo a member that is an array with inputDelays and outputLatencies. DelayCalculation needs to work directly from the AccelInfo information.

Basically, the best way that I can see of integrating partition code is to use the AccelInfoIterator approach. 

Still need to see how things play out with merge and the likes, but realistically I think that this is the best approach. Just stuff everything into a giant table, simplify the logic as much as possible while being easy to spot mistakes/bugs by inspecting the table.

*/

/*

How to handle merge with the new accelInfoIterator.

We have a member that indicates whether a unit belongs or not to a given merge instance. The belong member.
If we change all the calculation functions to check for the belong member and then act based on it, then we do not need to make anything special for the majority of calculation functions.

The flow then becomes:

Subunit register or merge register creates the starting Array<InstanceInfo>.
We then call functions on these that look at certain members to decide what to do.
If the functions depend on the corresponding data correctly, then as long as the starting data is correct, we should be able to successfully abstract the differences.


TODO: Need to check the isSource member of ModuleInfo and so on. 

TODO: Because of the delay implementation, there is a possibility of allowing inter-hierarchy optimizations:
      Think a VRead connected a reg file that uses delays to store a value per reg.
      The normal implementation would insert a bunch of delays with an incremental value which is wasteful because we could just change reg delays.
      This requires to keep track for each module which one allows this to happen and then take that into account in the delay calculation functions. Note: Input delay does not work. We are talking about delay info propagating downards accross the hiearchy while input delay propagates upwards. We can still resolve this during the register FUDeclaration by calcuting the delays of lower units inside the register of the upper unit.

Problem. We are genereting the structs with the multiplexer configs inside.
I do not like that. I want all the merge multiplexers to be at the top of accelConfig and none inside the units themselves.

What I currently imagine.

If we have all the information that we need inside an array of tables (where each table is associated to a merge possibility) then everything becomes so much easier to do/debug.

For example, for each multiplexer, we could have a member that indicates the input port that is associated to that merge datapath.

We could also store graph data inside that representation. The way in which we handle graphs is soo clubersome. 


// Current state.

We have a couple of old functions in configurations that are responsible for calculation the AccelInfo.
The calculation of the instances for base accelerators as already been taken care of, but nothing has been done for merge instances.

FUDeclaration still contains ConfigurationInfo and instanceInfo members. We still have not put AccelInfo values inside FUDeclaration and no code relies on accessing this info, with some exceptions because instanceInfo is being used somewhat.

Functions that register modules and merge do not interact in any shape or form with InstanceInfo.

Overall, we only have a small part of the work done. Merge is really clubersome and we can't even test things out currently because the headers are broken. Maybe the best is to restore headers and go from there.

We want to remove ConfigurationInfo from FUDeclaration.
We need to replace it with AccelInfo.
We need merge to generate correct accel info beforehand.

Rembember: Calculating delays on the merge unit does not make sense.
           Delays can only be calculated on modules and in the recon of merge graphs.

 */



















