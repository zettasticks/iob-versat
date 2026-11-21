#include <argp.h>

#include "filesystem.hpp"
#include "globals.hpp"
#include "templateEngine.hpp"
#include "hierName.hpp"
#include "codeGeneration.hpp"

#include "filesystem_meta.hpp"

/*
  Stuff:
  - StringBuilder -> Write to file and Template helpers instead of double copy.

*/

int CopyFileGroup(Array<FileContent> fileGroup,String filepathBase,bool flattenedDirs,FilePurpose purpose){
  TEMP_REGION(temp,nullptr);
    
  String common = fileGroup[0].originalRelativePath;
  for(FileContent content : fileGroup){
    common = GetCommonPath(common,content.originalRelativePath,temp);
  }
  
  for(FileContent content : fileGroup){
    String pathWithoutCommon = Cut(content.originalRelativePath,common.size,0);

    String pathWithExpectedCommon = {};
    if(flattenedDirs){
      // Nothing
    } else if(Empty(content.commonFolder)){
      pathWithExpectedCommon = PushString(temp,".%.*s",UN(pathWithoutCommon));
      // Nothing
    } else {
      pathWithExpectedCommon = PushString(temp,"%.*s/%.*s",UN(content.commonFolder),UN(pathWithoutCommon));
    }

    String fullPath = PushString(temp,"%.*s/%.*s",UN(filepathBase),UN(pathWithExpectedCommon));
    fullPath = OS_NormalizePath(fullPath,temp);
    fullPath = PushString(temp,"%.*s/%.*s",UN(fullPath),UN(content.fileName));

    FILE* file = OpenFileAndCreateDirectories(fullPath,"w",purpose);
    
    if(!file){
      printf("Error opening file for output: %.*s\n",UN(fullPath));
      return -1;
      // TODO(Error)
    }
    
    String data = content.content;
    fwrite(data.data,sizeof(char),data.size,file);
    fclose(file);
  }

  return 0;
};

struct OptionsGather{
  Arena* arena;
  
  ArenaList<String>* verilogFiles; // Individual files, for cases where we want specific files inside a folder.
  ArenaList<String>* unitFolderPaths;
  ArenaList<String>* extraSources;
  ArenaList<String>* includePaths;
  
  ArenaList<DECL_Param>* paramDefinitions;

  Options* options;

  ArenaList<String>* parsingErrors;
};

static int parse_opt (int key, char *arg,argp_state *state){
  OptionsGather* opts = (OptionsGather*) state->input;
  Arena* out = opts->arena;

  // TODO: Better error handling
  switch (key)
    {
    case 'S': *opts->extraSources->PushElem() = arg; break;
    case 'I': *opts->includePaths->PushElem() = arg; break;

    case 'A': {
      const char* start = arg;
      const char* ptr = arg;

      for(;ptr; ptr += 1){
        if(*ptr == '='){
          break;
        }
      }

      String paramName = String(start,ptr - start);
      ptr += 1;

      const char* valueStart = ptr;
      for(;ptr; ptr += 1){
        if(*ptr == '\0'){
          break;
        }
      }

      String value = String(valueStart,ptr - valueStart);
      Token number = ParseNumber(value.data,value.data + value.size);

      if(number.type == TokenType_NUMBER){
        i64 value = ParseInt(number.val);
        
        DECL_Param* val = opts->paramDefinitions->PushElem();
        val->name = PushString(out,paramName);
        val->val = SYM_Lit(value);
      } else {
        *opts->parsingErrors->PushElem() = PushString(out,"Error parsing -A'%s', we expected a number but instead got '%.*s', make sure that you use the form '-AX=Y' where X is paramName and Y is an integer",arg,UN(value));
      }
    } break;

    // TODO: All the filepaths should be inserted into verilogFiles while this only takes in folders.
    case 'u': *opts->unitFolderPaths->PushElem() = arg; break;

    case 'b': opts->options->databusDataSize = ParseInt(arg); break;

    case 'd': opts->options->useDMA = true; break;
    case 'D': opts->options->architectureHasDatabus = true; break;

    case 'L': opts->options->generetaSourceListName = arg; break;

    case 'p': opts->options->prefixIObPort = arg; break;

    case 'T': opts->options->opMode = VersatOperationMode_GENERATE_TESTBENCH; break;

    case 'E': {
      opts->options->extraIOb = true;
      opts->options->useSymbolAddress = true;
    } break;

    case 128: {
      opts->options->insertDebugRegisters = true;
    } break;

    case 129: {
      opts->options->insertProfilingRegisters = true;
    } break;

    case 130: {
      opts->options->insertCaptureDatabusRegisters = true;
    } break;
      
    case 'g': opts->options->debugPath = arg; opts->options->debug = true; break;
    case 't': opts->options->topName = arg; break;
    case 'o': opts->options->hardwareOutputFilepath = arg; break;
    case 'O': opts->options->softwareOutputFilepath = arg; break;

    case ARGP_KEY_ARG: opts->options->specificationFilepath = arg; break;
    case ARGP_KEY_END: break;
    }
  return 0;
}

// TODO: Better error handling
struct argp_option options[] =
  {
    { "debug", 128 ,0, 0, "Insert debug registers on the generated accelerator",0},
    { "profile", 129 ,0, 0, "Insert profiling registers on the generated accelerator",0},
    {"captureDatabus",130,0,0, "Insert registers to capture databus data from transfers",0},
    { 0, 'A', "Arg",   0, "Set param to value",0},
    { 0, 'b',"Size",   0, "Databus size connected to external RAM (8,16,default:32,64,128,256)",0},
    { 0, 'd', 0,       0, "Use DMA",0},
    { 0, 'D', 0,       0, "Architecture has databus",0},
    { 0, 'E', 0,       0, "Enables a set of hardcoded options that is mostly some hacks for other projects (which in the future I hope to properly implement as proper configurable parameters)",0},
    { 0, 'g',"Path",   OPTION_HIDDEN, "Debug path",0},
    { 0, 'S',"File",   0, "Extra sources",0},
    { 0, 'I',"Path",   0, "Include paths",0},
    { 0, 'L',"Name",   0, "Writes to a file a list of all files generated by Versat",0},
    { 0, 'o',"Path",   0, "Hardware output path",0},
    { 0, 'O',"Path",   0, "Software output path",0},
    { 0, 'p',"Prefix", 0, "Prefix the IOb input port with user provided content",0},
    { 0, 't',"Name",   0, "Top unit name",0},
    { 0, 'T',0,        0, "Generate a testbench for the given unit that exercises it as if being used by an accelerator.",0},
    { 0, 'u',"Path",   0, "Units path",0},
    { 0,   0,     0,   0,            0,0}
  };

void ReportFileCreation(bool allFiles = false){
  TEMP_REGION(temp,nullptr);
  for(FileInfo f : CollectAllFilesInfo(temp)){
    if(allFiles || (f.purpose != FilePurpose_DEBUG_INFO && f.mode == FileOpenMode_WRITE)){
      String type = FilePurpose_Name(f.purpose);
      printf("Filename: %.*s Type: %.*s\n",UN(f.filepath),UN(type));
    }
  }
}

int main(int argc,char* argv[]){
#ifdef VERSAT_DEBUG
  printf("Running in debug mode\n");
#endif

  Arena globalPermanentInst = InitArena(Megabyte(128));
  globalPermanent = &globalPermanentInst;
  Arena tempInst = InitArena(Megabyte(128));
  Arena temp2Inst = InitArena(Megabyte(128));

  contextArenas[0] = &tempInst;
  contextArenas[1] = &temp2Inst;
  
  InitDebug(argv[0]);

  for(int i = 0; i < 8; i++){
    singleUseCasesArenas[i] = InitArena(Megabyte(16));
  }
  
  TEMP_REGION(temp,nullptr);
  TEMP_REGION(temp2,temp);
  
  Arena* perm = globalPermanent;

  // Init common stuff before compiler stuff.
  SYM_Init();
  TE_Init();
  HIER_Init();
  FILE_Init();

  // TODO: Move this to meta 
  InitializeDefaultData(perm);
  HW_Init(perm);

  DECL_Init();

  for(int i = 0; i < argc; i++){
    printf("ARGS: %s\n",argv[i]);
  }

  argp argp = { options, parse_opt, "SpecFile\n-T UnitName", "Dataflow to accelerator compiler. Check tutorial in https://github.com/IObundle/iob-versat to learn how to write a specification file"};

  FREE_ARENA(optsArena);
  
  OptionsGather gather = {};
  gather.arena = optsArena;
  gather.verilogFiles = PushList<String>(temp);
  gather.extraSources = PushList<String>(temp);
  gather.includePaths = PushList<String>(temp);
  gather.unitFolderPaths = PushList<String>(temp);
  gather.paramDefinitions = PushList<DECL_Param>(temp);
  gather.parsingErrors = PushList<String>(temp);

  globalOptions = DefaultOptions(perm);
  gather.options = &globalOptions;

  if(argp_parse(&argp, argc, argv, 0, 0, &gather) != 0){
    printf("Error parsing arguments. Call -h help to print usage and argument help\n");
    return -1;
  }

  if(!Empty(gather.parsingErrors)){
    for(String str : gather.parsingErrors){
      printf("%.*s\n",UN(str));
    }

    return -1;
  }
  
  globalOptions.verilogFiles = PushArray(perm,gather.verilogFiles);
  globalOptions.extraSources = PushArray(perm,gather.extraSources);
  globalOptions.includePaths = PushArray(perm,gather.includePaths);
  globalOptions.unitFolderPaths = PushArray(perm,gather.unitFolderPaths);
  
  if(globalOptions.opMode == VersatOperationMode_GENERATE_TESTBENCH){
    globalOptions.topName = globalOptions.specificationFilepath;
  }

  globalOptions.hardwareOutputFilepath = OS_NormalizePath(globalOptions.hardwareOutputFilepath,temp);
  globalOptions.softwareOutputFilepath = OS_NormalizePath(globalOptions.softwareOutputFilepath,temp);

  globalDebug.outputGraphs = true;
  globalDebug.outputConsolidationGraphs = true;
  globalDebug.outputVCD = true;
  
  if(Empty(globalOptions.topName)){
    char name[] = "versat";
    argp_help(&argp,stdout,ARGP_HELP_STD_HELP,name);
    printf("\nNeed to specify top unit with -t\n");
    exit(-1);
  }

  // TODO: Merge flow and check if we are overriding any units or not.
  struct SimpleTypeFileInfo{
    SimpleTypeFileInfo* next;
    String simpleTypeName;
    String content;
  };

  SimpleTypeFileInfo* fileInfoHead = 0;
  SimpleTypeFileInfo* fileInfoTail = 0;

  bool anyError = 0;
  for(FileContent& file : defaultVerilogUnits){
    String content = file.content;
    V_ParseResult parsed = V_ParseVerilogFile(content,perm);

    if(parsed.errors.size > 0){
      for(String str : parsed.errors){
        printf("%.*s\n",UN(str));
      }
      
      anyError = 1;
      continue;
    }

    for(V_Node* ptr = parsed.node->childs; ptr; ptr = ptr->next){
      Assert(ptr->type == V_NodeType_MODULE);

      String name = ptr->token.val;
      bool error = DECL_RegisterMeta(name,ptr);
      if(error){
        printf("Error registering module \"%.*s\"\nAlready exists a module with the same name\n",UN(name));
        anyError |= error;
      }

      SimpleTypeFileInfo* info = PushStruct<SimpleTypeFileInfo>(perm);
      info->simpleTypeName = PushString(perm,name);
      info->content = file.content;
      LL_Append(fileInfoHead,fileInfoTail,next,info);
    }
  }

  for(String folderPath : globalOptions.unitFolderPaths){
    Array<String> filePaths = OS_GetFilenamesInsideDir(folderPath,temp);

    for(String filename : filePaths){
      String path = PushString(temp,"%.*s/%.*s",UN(folderPath),UN(filename));
      String content = PushFile(temp,path);
      V_ParseResult parsed = V_ParseVerilogFile(content,perm);

      if(parsed.errors.size > 0){
        for(String str : parsed.errors){
          printf("%.*s\n",UN(str));
        }
      
        anyError = 1;
        continue;
      }

      for(V_Node* ptr = parsed.node->childs; ptr; ptr = ptr->next){
        Assert(ptr->type == V_NodeType_MODULE);

        String name = ptr->token.val;
        bool error = DECL_RegisterMeta(name,ptr);
        if(error){
          printf("Error registering module \"%.*s\"\nAlready exists a module with the same name\n",UN(name));
          anyError |= error;
        }

        SimpleTypeFileInfo* info = PushStruct<SimpleTypeFileInfo>(perm);
        info->simpleTypeName = PushString(perm,name);
        info->content = content;
        LL_Append(fileInfoHead,fileInfoTail,next,info);
      }
    }
  }
  
  if(anyError){
    return -1;
  }

  // Parse spec file
  String specFilepath = globalOptions.specificationFilepath;
  if(specFilepath.size){
    String content = PushFile(temp,StaticFormat("%.*s",UN(specFilepath)));
    // TODO: Handle file error
    SP_Node* top = SP_ParseSpec(content,perm);
    Assert(top->type == SP_Type_TOP);

    for(SP_Node* ptr = top->childs; ptr; ptr = ptr->next){
      String name = ptr->token.val;
      DECL_RegisterMeta(name,ptr);
    }
  }

  String topLevelTypeStr = globalOptions.topName;
  auto b = PushList<DECL_Param>(temp);
  *b->PushElem() = DECL_Param{"DATA_W",SYM_32};
  *b->PushElem() = DECL_Param{"DELAY_W",SYM_DelayW};
  *b->PushElem() = DECL_Param{"LEN_W",SYM_LenW};
  *b->PushElem() = DECL_Param{"AXI_DATA_W",SYM_AxiDataW};
  *b->PushElem() = DECL_Param{"AXI_ADDR_W",SYM_AxiAddrW};
  for(DECL_Param p : gather.paramDefinitions){
    *b->PushElem() = p;
  }
  Array<DECL_Param> params = PushArray(temp,b);
  
  FUDeclaration* topType = DECL_GetType(topLevelTypeStr,params,true);

  if(IsNil(topType)){
    printf("Did not find module named: '%.*s'\n",UN(topLevelTypeStr));
    return -1;
  }

  // Only output if units are actually used by top level ========================
  int compositeCount = 0;
  FUDeclarationNode* simpleHead = 0;
  FUDeclarationNode* simpleTail = 0;

  FUDeclarationNode* compositeHead = 0;
  FUDeclarationNode* compositeTail = 0;
  {
    static FUDeclarationNode top = {};
    top.val = topType;
    compositeCount += 1;
    LL_Append(compositeHead,compositeTail,next,&top);

    for(FUDeclarationNode* ptr = compositeHead; ptr; ptr = ptr->next){
      for(COM_Unit* unit = ptr->val->fixedDelays.units.head; unit; unit = unit->next){
        FUDeclaration* decl = unit->decl;
        FUDeclarationNode* exists = 0;

        if(DECL_IsVersatGenerated(decl)){
          LL_Find(compositeHead,next,exists,it->val->name == decl->name);

          if(!exists){
            FUDeclarationNode* node = PushStruct<FUDeclarationNode>(temp);
            node->val = decl;
            compositeCount += 1;
            LL_Append(compositeHead,compositeTail,next,node);
          }

          continue;
        } else if(DECL_IsVersatDefault(decl)){
          LL_Find(simpleHead,next,exists,it->val->name == decl->name);

          if(!exists){
            FUDeclarationNode* node = PushStruct<FUDeclarationNode>(temp);
            node->val = decl;
            LL_Append(simpleHead,simpleTail,next,node);
          }
        }
      }
    }
  }

  Array<String> generated = PushArray<String>(temp,compositeCount);

  for(FUDeclarationNode* node = simpleHead; node; node = node->next){
    FUDeclaration* decl = node->val;

    SimpleTypeFileInfo* info = 0;
    LL_Find(fileInfoHead,next,info,it->simpleTypeName == decl->name);
    
    Assert(info);
    String path = PushString(temp,"%.*s/%.*s.v",UN(globalOptions.hardwareOutputFilepath),UN(decl->name));
    FILE* file = OpenFileAndCreateDirectories(path,"w",FilePurpose_VERILOG_CODE);
    fprintf(file,"%.*s",UN(info->content));

    fclose(file);
  }

  {
    int index = 0;
    for(FUDeclarationNode* node = compositeHead; node; index++,node = node->next){
      FUDeclaration* decl = node->val;
      generated[index] = decl->name;

      String path = PushString(temp,"%.*s/%.*s.v",UN(globalOptions.hardwareOutputFilepath),UN(decl->name));
      FILE* sourceCode = OpenFileAndCreateDirectories(path,"w",FilePurpose_VERILOG_CODE);
      DEFER_CLOSE_FILE(sourceCode);

      String content = GEN_OutputCircuitSource(decl,temp);
      fprintf(sourceCode,"%.*s",UN(content));

      printf("Outputted: %.*s\n",UN(decl->name));
    }
  }

  GEN_TopInfo info = GEN_CalculateTopInfo(topType,temp);
  GEN_TopContent content = GEN_OutputTopLevel(&info,temp);
  String makefileContent = GEN_OutputVerilatorMakefile(&info,generated,temp);

  String hardware = globalOptions.hardwareOutputFilepath;
  String software = globalOptions.softwareOutputFilepath;
  
  auto Output = [&](String path,String name,FilePurpose p,String content){
    String fullpath = SF("%.*s/%.*s",UN(path),UN(name));
    FILE* output = OpenFileAndCreateDirectories(fullpath,"w",p);
    fprintf(output,"%.*s",UN(content));
    if(p == FilePurpose_SCRIPT){
      OS_SetScriptPermissions(output);
    }
    fclose(output);
  };

  Output(hardware,"iob_versat.v",FilePurpose_VERILOG_CODE,content.topHardwareWrapper);
  Output(hardware,"versat_instance.v",FilePurpose_VERILOG_CODE,content.hardwareInstance);
  Output(hardware,"versat_configurations.v",FilePurpose_VERILOG_CODE,content.hardwareConfigs);
  Output(hardware,"versat_defs.vh",FilePurpose_VERILOG_INCLUDE,content.verilogDefines);
  Output(hardware,"versat_undefs.vh",FilePurpose_VERILOG_INCLUDE,content.verilogUndefs);
  Output(hardware,"versat_internal_memory_wires.vh",FilePurpose_VERILOG_INCLUDE,content.externalMemWires);
  Output(hardware,"versat_external_memory_inst.vh",FilePurpose_VERILOG_INCLUDE,content.externalMemInst);
  Output(hardware,"versat_internal_memory_wires.vh",FilePurpose_VERILOG_INCLUDE,content.externalMemWires);
  Output(hardware,"versat_external_memory_port.vh",FilePurpose_VERILOG_INCLUDE,content.externalMemPorts);
  Output(hardware,"versat_external_memory_internal_portmap.vh",FilePurpose_VERILOG_INCLUDE,content.externalMemPortmap);

  Output(software,"VerilatorMake.mk",FilePurpose_MAKEFILE,makefileContent);
  Output(software,"versat_accel.h",FilePurpose_SOFTWARE,content.headerFile);
  Output(software,"iob_versat.c",FilePurpose_SOFTWARE,content.softwareFirmware);
  Output(software,"Versat_verilator_wrapper.v",FilePurpose_VERILOG_CODE,content.verilatorHardwareWrapper);
  Output(software,"verilator_wrapper.cpp",FilePurpose_VERILOG_CODE,content.verilatorSoftwareWrapper);
  Output(software,"GetVerilatorRoot.sh",FilePurpose_SCRIPT,content.getVerilatorRoot);
  Output(software,"ExtractVerilatedSignals.py",FilePurpose_SCRIPT,content.extractVerilatedSignal);

  int res = CopyFileGroup(commonVersatVerilog,hardware,true,FilePurpose_VERILOG_COMMON_CODE);
  if(res){
    return res;
  }

  printf("ADDR_W:%d\n",info.addressBits);
  if(info.numberDatabus){
    printf("HAS_AXI:True\n");
  }

  if(globalOptions.exportInternalMemories){
    int index = 0;
#if 0
    for(ExternalMemorySymbolic inter : external){
      switch(inter.type){
      case ExternalMemoryType::ExternalMemoryType_DP:{
        printf("DP - %d",index++);
        for(int i = 0; i < 2; i++){
          printf(",%.*s",UN(SYM_Repr(inter.dp[i].bitSize,temp)));
          printf(",%.*s",UN(SYM_Repr(inter.dp[i].dataSizeOut,temp)));
          printf(",%.*s",UN(SYM_Repr(inter.dp[i].dataSizeIn,temp)));
        }
        printf("\n");
      }break;
      case ExternalMemoryType::ExternalMemoryType_2P:{
        printf("2P - %d",index++);
        printf(",%.*s",UN(SYM_Repr(inter.tp.bitSizeOut,temp)));
        printf(",%.*s",UN(SYM_Repr(inter.tp.bitSizeIn,temp)));
        printf(",%.*s",UN(SYM_Repr(inter.tp.dataSizeOut,temp)));
        printf(",%.*s",UN(SYM_Repr(inter.tp.dataSizeIn,temp)));
        printf("\n");
      }break;
      }
    }
#endif
  }

  ReportFileCreation();

  return 0;
}

/*

// ============================================================================
// Bugs

BUG: There was a bug in versat_ai that forced me to put a ResetAccelerator call inside the BatchNormalization layer. The problem only appear if we called Conv before Batch. Otherwise everything was fine. Did not test with the other operations. What I suppose is hapenning is that the Conv is loading some configuration that makes units produce some data that Batch should not depend on but it actually does. Maybe garbage data from merged units is influencing the accel somewhat. Regardless, I did not have the tools needed to debug such a problem at that point and opted to use the quick and dirty way of fixing stuff.

BUG: Need more tests for modules that contain units but are not connected to anything. Also test merge for these cases as well.

BUG: Debug folder gets created inside the iob-soc-versat repo when running ./test.py tests. Something about the python3 script -> makefile is causing the path to not be the same as if just calling makefile directly.

BUG: localOrder appears to be broken. 

BUG: Since the name of the units are copied directly to the header file, it is possible to have conflict with C reserved keywords, like const, static, and stuff like that. 

In fact, need to start checking every keyword used by C,C++ and Verilog and make sure that the names that are passed in the versat spec file never conflict with any special keyword.

BUG: Partial share is missing in the spec parser for multiple units with different names. Only working for arrays.

There is probably a lot of cleanup left (overall and inside Merge).

Need to take a look at State and Mem struct interfaces. State is not taking into account merge types and neither is Mem. Also need to see if we can generate the Mem as an actual structure where the programmer must use pointer arithmetic directly.

BUG:

module Test(){
Mem m0;
Mem m1;
#
m0 -> m1;
m1 -> m0;
}

produces a design that cannot work directly. Both units are given a delay of 0 but each unit takes 3 cycles to produce data which means that the design is fundamentally broken.

Fixing this bug would also give me an opportunity to booster up the delay and graph side of the code. It is probably more complex than if I would write it today.

BUG:

An empty AddressGen is not working correctly. The address gen parser is programmed to return an error if the address gen expression does not contain anything, but it is technically good syntax. Either we allow the program to have empty address gens or we put an error that detects such case and reports this info to the user.

*/

/*

Code generation for customizable features:
- There exists a bunch of code that gets generated even though it is not needed. Stuff like profile functions are declared and even defined in wrapper and firmware. While this is not a problem, since the stuff that is extra does not affect the correctness or causes any error, one thing that is does is slow down the testing, since now every change affects all the tests even though it should only change the tests that actually use the features.

-- Basically, it is not enough to control generation of functions, we also should control stuff like function declarations and the likes in order to make it so that tests that do not use the features are not forced to be recompiled and retested for a change that does not affect them.

Code generation lint friendly:

- If a module is composed of units that do not contain certain signals, like clk, rst, run and the likes, then the module should not have those signals as well. The only thing missing to implement this is changing the wrapper to support the verilated unit not containing these signals.

More user error checking:

- Verilog parsed content does not check direction of ports. We should encode all the interfaces that we expect as data and perform the checks and report errors.

Error handling:

- Versat is currently incapable of handling even the slighest deviation from good data. Very poor handling of error conditions.
- Some stuff that we want:
-- Parser errors and related stuff. Any problem with the input data that is related to the spec file needs to report the error with a message that identifies in the spec file the source of the problem.
-- Problems with the graph most be reported with a good message. Missing connections might be warnings, extra connections need a location error on the spec file. Problems with delay calculations need an identification of the path that is causing the problem and stuff like that.

Usability:

"Bugs" that arise from forgetting to activate the merged accelerator are starting to become problematic.
Need to find a way, at least in PC-Emul, of detecting that the proper merged accelerator was not activated.
There are two things that we can do.
First, start the accelerator in an empty state and give an error if no merged was set when doing a run. Very likely an error. The user cannot rely on the fact that the accelerator starts on merged 0.
Second, we can make it so that changing an accelerator merge state clears the config structure and we can figure out which config values are associated to each merged state.
What this means is that we can potently figure out which merged state the user is likely configuring the accelerator regardless of the actual state the accelerator is set to and we can warn the user if
it is very likely that the user is missing some configuration or it appears to miss configure the current merged accelerator.

We spend a good deal of time writing address gens that access matrix like structures.
- Maybe it would be a good idea to push some of this logic to the address gens.
-- Imagine being able to declare that the data follows a certain matrix like structure and then being able to provide addresses by accessing it.

Something like:

addressGen test(width,height){
  format[height][width];
  for y 0..height
  for x 0..width
  addr = format[y][x];
}

Usability:

- Need to check parameters and sizes and report stuff at Versat compile time.
-- An example is the xunitF which requires input to be 32 bits, meaning that we cannot just change DATA_W and expect this to work.

Optimizations:

-- We could collapse certain loops, assuming that we have the info needed (or we can push it into runtime).
-- Leftover loops can be used to reduce the preassure on the lower loops (less bits needed and stuff like that, we currently pay for the upper loops bits wether we use them or not)

- There is an optimization that we can make in order to reduce memory usage. For loops where the innermost term is not a '1' we are fetching more memory than we care about and that is fine. The problem is that we are storing those unused values inside the internal memory when we do not have to. If the innermost term was a 4, we could make it so that the internal memory only stores every 4 values read. For double loop address gens, we make sure that in hardware we reset every time we have a loop (so that the address gen lines up correctly). That way, for a loop that contains a constant term N, we reduce the amount of internal memory used by a factor of N.

-- The same concept applies to writes, but instead of storing memory, we must control the transfer process to not write over memory by completely disabling the strobe for the values that we do not want to write over.

--- How would we handle bigger transfer sizes, like AXI_DATA_W = 256? For both reads and writes it become more difficult to save memory. 

Testability:

- While we test if versat runs and produces valid code, we do not test how does versat handle errors.

-- How does Versat handle not being able to parse a Module Verilog?
-- How does Versat handle not being able to parse the specs file?
-- How does it handle graphs that contain loops, misuse in constructs like share, repeated variable names and the likes?
-- Etc. Need to develop tests that only need to exercise this conditions and need to make sure that at least Versat produces some valid output and does not simply crash without giving any useful information.

Misc:

Hierarchical Merge:

- Hierarchical merge produces a lot of structures in the header files which are not actually needed.
-- Need to collapse same structure format into a single format.
-- Same thing for merge types, merge names and stuff like that. We want to collapse everything that is equal into a single form, reduce complexity and 
--- IMPORTANT: The delays also need to be collapsed. There is no point in storing 

Generated code does not take into account parameters when it should.
- Some wires are given fixed sizes when they depend on verilog parameters. 

Debugability:

- We kinda lost the .dot files generated in order to help debug graph algorithms. Its not a big deal now but eventually would like to have them back, as the .dot files are the only proper way we currently have of debugging graph operations (which I do not think we will be doing much soon, but eventually need to address this).

Wrapper:

- The databus does not take into consideration the strobe of the databus.

- Remove the while from the simulate function.
  -  Also put in a comment. We cannot have any "while" inside the wrapper. Every function must be guarantee to end, even if it just a for loop of 1 million iterations that then ends by reporting an error to the user.

Parser:

- I should be able to use write: "share config Type unit[N];", instead of having to write: "share config Type{ unit[N]; }"

*/
