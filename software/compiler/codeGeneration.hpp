#pragma once

#include "declaration.hpp"
#include "accelerator_meta.hpp"

/*
  NOTE: Very important. We do not want to have conditions affect the generated code that interfaces with the user. 
        We want to generate the same "stuff" regardless of what the value is.
        Trying to be clever and avoiding generating stuff because the value is not "needed"
        might cause problems because some symbol might be missing in the generated code 
        which causes the compiler to give out errors and that is not good.
        And forcing the user to disable certain sections or changing its code is not good.
        There is no problem in generating functions that are empty, structs that you know will not be used
        or enums that you know will not be needed. 
        Anything that belongs to the user interface must be generated, even if empty.

        For internal usage then it is different. At this point we do want to make sure that we only generate what
        we actually use in order to avoid compiler warnings, improve compilation speed and such.
*/

// ======================================
// C Struct generation code

struct GEN_StructInfo;

// A struct elemenent. If union then childs contain the various possibilities
// TODO: We might be able to reconcile structs and unions by assuming
//       that a struct is just a one element union and work from there
struct GEN_StructElem{
  GEN_StructElem* next;
  GEN_StructElem* childs; // For unions

  String name;
  
  // Type info;
  GEN_StructInfo* type;
  String simpleTypename;
  bool isMuxMultiplexer;
};

struct GEN_StructInfo{
  GEN_StructInfo* next;

  FUDeclaration* originalType;
  String name;
  GEN_StructElem* elements;
};

struct GEN_StructInfoList{
  GEN_StructInfo* head;
  GEN_StructInfo* tail;
};

// ======================================
// Gen Type

GEN_StructInfo* GEN_GenerateConfigStruct(COM_Unit* top,GEN_StructInfoList* allStructs,Arena* out);
GEN_StructInfo* GEN_GenerateStateStruct(COM_Unit* top,GEN_StructInfoList* allStructs,Arena* out);

GEN_StructInfo* GEN_AddStruct(GEN_StructInfoList* list,FUDeclaration* originalType,GEN_StructElem* head,Arena* out);

bool GEN_IsSimpleType(GEN_StructElem* elem);
bool GEN_IsUnion(GEN_StructElem* top);

String GEN_TypeName(GEN_StructElem* elem);

String GEN_Repr(GEN_StructInfo* info,Arena* out);

// ======================================
// TopLevelInfo

enum GEN_WireType{
  GEN_WireType_NONE = 0,
  GEN_WireType_IS_VERSAT = (1 << 1),
  GEN_WireType_IS_STATIC = (1 << 2),
  GEN_WireType_IS_DELAY  = (1 << 3)
};
C_STYLE_ENUM(GEN_WireType);

struct GEN_Wire{
  String name;
  VersatStage stage;
  SYM_Expr size;
  SYM_Expr startBit;
  int address; // Byte aligned
  GEN_WireType type;
};

struct GEN_MMInfo{
  String decisionMask;
  String addrName;
  int    addrStart;
  int    addrSize;
};

// nocheckin
struct GEN_MMNameInfo{
  String name;
  GEN_MMInfo* info;
};

struct GEN_TopInfo{
  FUDeclaration* decl;

  Array<VersatRegister> registers;
  Array<GEN_MMInfo> memoryMapped;
  Array<GEN_MMNameInfo> memoryMappedNames;

  Array<GEN_Wire> allConfigs;
  Array<GEN_Wire> allStates;

  int totalConfigs; // Config + Static + Delay
  int configStart;
  int staticStart;
  int delayStart;

  SYM_Expr configOnlyBitCount;
  SYM_Expr configTotalBitCount;

  // TODO:
  int staticCount;
  SYM_Expr staticBitCount;

  int stateCount;
  SYM_Expr stateBitCount;

  // Address is divided into two regions (memory mapped vs config/state)
  // If no memory mapped then no division (config/state occupies the entire address space).
  //   At which point addressBits == memoryConfigDecisionBit
  // nocheckin: TODO: We only care about two regions, why do we have memoryMappedBit and configDecisionBit?
  int addressBits;
  int memoryMappedDecisionBit;
  int memoryConfigDecisionBit;

  int numberDatabus; // Can differ from declaration because of DMA
};

GEN_TopInfo GEN_CalculateTopInfo(FUDeclaration* topLevel,Arena* out);
int         GEN_GetRegisterIndex(GEN_TopInfo* info,VersatRegister reg);

struct GEN_OutputData{
  String wireName;
  bool isReg;
  bool declare;
  bool isConnected;
};

struct GEN_Data{
  int index;
  Array<GEN_OutputData> inputInfo;
  Array<GEN_OutputData> outputInfo;
};

struct GEN_TopContent{
  // Hardware includes
  String verilogDefines;
  String verilogUndefs;

  String externalMemInst;
  String externalMemWires;
  String externalMemPorts;
  String externalMemPortmap;

  // Hardware top files 
  String topHardwareWrapper;
  String hardwareInstance;
  String hardwareConfigs;

  // Softare verilator helpers
  String verilatorHardwareWrapper;
  String verilatorSoftwareWrapper;
  String getVerilatorRoot;
  String extractVerilatedSignal;

  // Software code
  String headerFile;
  String softwareFirmware;
};

// ======================================
// Type

bool GEN_IsConfigOnly(GEN_WireType type);

// ======================================
// Naming schemes

String GEN_StaticName(DECL_StaticWire w,Arena* out);

// ======================================
// Helpers

void GEN_EmitCombOperations(VEmitter* m,COM_Unit* units,COM_Edge* edges);
void GEN_EmitInstanciateUnits(VEmitter* m,COM_Unit* units,COM_Edge* edges);
void GEN_EmitConnectOutputsToOut(VEmitter* v,COM_Unit* units,COM_Edge* edges);
void GEN_InitializeAuxGraphData(COM_Unit* units,COM_Edge* edge,Arena* out);

// ======================================
// Top Code generation

String         GEN_OutputCircuitSource(FUDeclaration* decl,Arena* out);
GEN_TopContent GEN_OutputTopLevel(GEN_TopInfo* info,Arena* out);
String         GEN_OutputVerilatorMakefile(GEN_TopInfo* info,Array<String> allUnitsGenerated,Arena* out);

String         GEN_OutputTestbench(FUDeclaration* decl,Arena* out);

// ======================================
// Individual code generation helpers

String GEN_OutputHardwareInstance(GEN_TopInfo* info,Arena* out);
String GEN_OutputHardwareConfigurations(GEN_TopInfo* info,Arena* out);
String GEN_OutputHeader(GEN_TopInfo* info,Arena* out);
String GEN_OutputVerilatorHardwareWrapper(GEN_TopInfo* info,Arena* out);
String GEN_OutputVerilatorSoftwareWrapper(GEN_TopInfo* top,Arena* out);
String GEN_OutputFirmware(GEN_TopInfo* info,Arena* out);
