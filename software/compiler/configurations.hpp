#pragma once

#if 0

#include "memory.hpp"
#include "accelerator.hpp"
#include "addressGen.hpp"
#include "verilogParsing.hpp"
#include "hierName.hpp"

#include "compiler.hpp"

struct SimplePortInstance{
  int inst;
  int port;
};

struct SimplePortConnection{
  int otherInst;
  int otherPort;
  int port;
};

struct StructInfo;
struct ConfigFunction;

struct MergePartition{
  String name;
  Array<InstanceInfo> info;

  // TODO: Composite units currently break the meaning of baseType.
  //       Since a composite unit with 2 merged instances would need to have 2 base types.
  FUDeclaration* baseType;
  AcceleratorMapping* baseTypeFlattenToMergedBaseCircuit;
  Set<PortInstance>*  mergeMultiplexers;
  
  // TODO: Weird that this is inside MergePartition but at the same time we need to know which merge partition this function belongs to in order to generate the 
  //       The weirdness is mostly the fact that userFunctions are mostly "Global" in the sense that they cannot repeat but at the same time we need the MergePartition info which means that we might just take this out and have to store the merge partition info in some other way.
  Array<ConfigFunction*> userFunctions;
  
  // TODO: All these are useless. We can just store the data in the units themselves.
  Array<int> inputDelays;
  Array<int> outputLatencies;
};

// NOTE: The member 'level' of InstanceInfo needs to be valid in order for this iterator to work. 
//       Do not know how to handle merged. Should we iterate Array<InstanceInfo> and let outside code work, or do we take the accelInfo and then allow the iterator to switch between different merges and stuff?      

struct Partition{
  int value;
  int max;
  FUDeclaration* decl;
};

AccelInfoIterator StartIteration(AccelInfo* info,int mergeIndex = 0);

void FillAccelInfoFromCalculatedInstanceInfo(AccelInfo* info,Accelerator* accel);

Array<InstanceInfo*> GetAllSameLevelUnits(AccelInfo* info,int level,int mergeIndex,Arena* out);

// TODO: mergeIndex seems to be the wrong approach. Check the correct approach when trying to simplify merge.
Array<InstanceInfo> GenerateInitialInstanceInfo(Accelerator* accel,Arena* out,Array<Partition> partitions,bool calculateOrder = true);

Array<Partition> GenerateInitialPartitions(Accelerator* accel,Arena* out);

void FillInstanceInfo(AccelInfoIterator initialIter,Arena* out);
void FillStaticInfo(AccelInfo* info,Arena* out);

// This function does not perform calculations that are only relevant to the top accelerator (like static units configs and such).
AccelInfo CalculateAcceleratorInfo(Accelerator* accel,bool recursive,Arena* out,bool calculateOrder = true);

Array<int> ExtractInputDelays(AccelInfoIterator top,Arena* out);
Array<int> ExtractOutputLatencies(AccelInfoIterator top,Arena* out);

// Array info related
// TODO: This should be built on top of AccelInfo (taking an iterator), instead of just taking the array of instance infos.
Array<String> ExtractStates(Array<InstanceInfo> info,Arena* out);
Array<Pair<String,int>> ExtractMem(Array<InstanceInfo> info,Arena* out);

// TODO: Do we add this to AccelInfo?
//       How much do we calculate at the time vs how much we just put inside accelInfo?
String GetEntityMemName(InstanceInfo* info,Arena* out);

String ReprStaticConfig(StaticId id,Wire* wire,Arena* out);

Opt<SYM_Expr> GetParameterValue(InstanceInfo* info,String name);

bool IsUnitCombinatorialOperation(InstanceInfo* info);

Opt<Wire*> CONF_GetEnableWire(InstanceInfo* info); // Memory accessing units might implement a "enable" wire.

// ======================================
// Static naming conventions

String GetStaticWireFullName(InstanceInfo* info,Wire wire,Arena* out);

// TODO: Reorganize, must be to a better place.
void InstantiateParameters(AccelInfo* info,Arena* temp);

// TODO: Reorganize
InstanceInfo* Find(AccelInfoIterator iter,HIER_Name hierarchicalNames);

// HACK: ======================================================================

void HACK_InitNode(AccelInfo* info);

/*

Parameters:

There are two kinds of parameters. Verilog parameters and Versat parameters.

Verilog parameters could in theory be propagated so that the final code generated keeps these parameters around. They become actual Verilog parameters and therefore the user instantiating the Verilog code can decide what they are.

Versat parameters on the other had have no Verilog counterpart. In fact they might affect the entire generated accelerator in such a way that is basically impossible to generalize to the runtime (it technically could be possible but we would need to compute a lot of stuff at runtime in order to do that).

Versat parameters are easy to handle. We make it so that we have a function that given a typename and the parameters we return a FUDeclaration that is an instantiation of those parameters. FUDeclaration now becomes an instantiation of a more generic form which might just be the result of the parsing step that we have at the beginning:

Parse -> Store parsed somewhere -> Parameter + ParsedResult = Compiled module.

The problem is the Verilog Parameters. If we make it so that we also follow the Versat parameters approach, then that means that a VWrite (.ADDR_W=8) and a VWrite (.ADDR_W=9) would be completely different FUDeclarations. We can still store the fact that they are the instances of a common unit, but we would then need to add another declaration type.

FUDeclaration and MetaFUDeclaration or something.

At the same time, because we still want to propagate parameters, we still need to represent stuff using symbolic expressions. We cannot escape them.

*/

#endif
