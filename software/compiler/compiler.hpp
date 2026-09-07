#pragma once 

#include "versatSpecificationParser.hpp"
#include "configurations.hpp"

#if 0

// Remember, because of stuff like inserting buffers/delays/muxs, we need to be able to write to this.
// Changing name is required. Or maybe not. If we have access to the partitions we could just compute the 
// final name.

// Anyway, lets start small and work from there. The only thing that I want without fail is easy copy.

// COM_Instance needs to contain a bunch of read-only data that needs to be simply copied
// While also containing a bunch of data that can change 
struct COM_Instance{
  COM_Instance* next;
  COM_Instance* prev;
  COM_Instance* partNext;
  COM_Instance* partPrev;
  
#if 0  
  int level;
  FUDeclaration* decl;
  String typeName;
  String parentTypeName;

  int localIndex;

  int id;
  String name;
  String baseName; // NOTE: If the unit does not belong to the merge partition the baseName will equal name.
  //String fullName;

  //Array<Wire> configs;
  //Array<Wire> states;

  //Array<ExternalMemorySymbolic> externalMemory;
  SingleInterfaces singleInterfaces;
  
  Opt<int> globalStaticPos; // Separating static from global makes stuff simpler. If mixing together, do not forget that struct generation cares about source of configPos.
  Opt<int> globalConfigPos;
  Opt<int> localConfigPos;

  //Array<int> individualWiresGlobalStaticPos;
  //Array<int> individualWiresGlobalConfigPos;
  //Array<int> individualWiresLocalConfigPos;
  //Array<bool> individualWiresShared;
  
  //Array<ParamAndValue> params;

  bool isStatic;
  bool isGloballyStatic;
  
  bool isShared;
  int sharedIndex;
  
  Opt<int> statePos;
  
  // Nil if no mem map, 0 if mem mapped with no address bits and any positive number is the number of bits.
  SYM_Expr memMapSym;
  iptr memMapped; // If memMapSym is non nil then this contains the start address

  int memGlobalIndex;
  int memSize;
  //String globalMemDecisionMask;
  int memStart;
  int memEnd;

  Opt<int> delayPos;
  //Array<int> extraDelay;
  int baseNodeDelay;
  int numberDelays;

  // TODO: There are a couple of variables like these that we could just put into an union.
  // Only makes sense on buffer units.
  int variableBufferDelay;

  bool isComposite;
  bool isMerge;

  int nIOs;

  // Sepcific to merge muxs
  bool isMergeMultiplexer;
  int mergePort;
  int muxGroup; // TODO: I think that we can remove muxGroup. We know which units belong or not to a given merge partition and we know their input value so there is no point in keeping the harder to understand and compute muxGroups.

  bool doesNotBelong; // For merge units, if true then this unit does not actually exist for the given partition
  int special;
  int localOrder;
  //FUInstance* inst; // Points to the recon instance for merge declarations.
  bool debug;

  //Array<int> inputDelays;
  //Array<int> outputLatencies;
  //Array<int> portDelay;
  int partitionIndex; // TODO: What does this do? Probably a remnant from the old implementation.

  //Array<SimplePortConnection> inputs; 

  //Array<SimplePortInstance> inputsDirectly;
  //Array<bool> outputIsConnected;

  AddressGenInst supportedAddressGen;

  SpecialUnitType specialType;

  StructInfo* structInfo;
#endif
};

struct COM_Env{
  Arena* arena;
  Arena* errorArena;

  
};

COM_Instance* SP_InstantiateModule(SP_Node* moduleDef,Array<ParamNameAndValue> topLevelParams,Arena* out);
#endif
