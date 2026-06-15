#pragma once

#include "utils.hpp"

/*

Extremely independent vcd parser.

User responsible for providing memory and content when requested.
If used to iterate in a straighforward manner then content will also be
requested in a straighforward manner. 

*/

enum VCD_ErrorType{

};

struct VCD_Error{
  VCD_ErrorType type;
};

// TODO: So many options that we could put in here.
//       Of the top of my head:
//       - MIN_MEMORY - Tries to use the least amount of extra memory possible.
//       - FORWARD_ONLY - Not allowed to request previously sent content. 
//       - BufferSize - User defines how much internal buffering its done. (0 is an option, parser uses no internal buffer). This might be useful if the VCD data is already in memory (no copying done).
struct VCD_Options{
  u64 fileBufferSize;
};

static inline VCD_Options VCD_DefaultOptions = {.fileBufferSize = (8 * 1024)};

struct VCD_Parser{
  u8* dataBuffer;

  u64 timestamp;
};

enum VCD_Bit{
  VCD_Bit_0 = 0,
  VCD_Bit_1 = 1,
  VCD_Bit_X = 2,
  VCD_Bit_Z = 3
};

struct VCD_Value{
  u16 bitSize;
  // Followed by u8[bitSize/4] values. A 8 bit signal is 2 bytes in the format 33221100 77665544;
};

struct VCD_Var{
  //
};

struct VCD_Request{
  u64 minMemoryToAllocate;

  // 
  u64 minContentToProvide;
  u64 contentOffset;
};

struct VCD_Snapshot{

};

// ======================================
// Parser creation

void VCD_StartHeader(VCD_Parser* parser,VCD_Options options = VCD_DefaultOptions);
bool VCD_HeaderFinished(VCD_Parser* parser);

// ======================================
// Data comsuption

VCD_Request VCD_GetRequest(VCD_Parser* parser);

void VCD_GiveMemory(VCD_Parser* parser,void* mem,u64 amount);
u64 VCD_Consume(VCD_Parser* parser,void* data,u64 amount); // Returns amount consumed. Parser is not expected to need the amount of memory consumed in near term, altough possible depending on usage.

// ======================================
// Error retrieval

bool VCD_IsError(VCD_Parser* parser);
VCD_Error VCD_GetError(VCD_Parser* parser);

// ======================================
// Info retrieval

Array<VCD_Var> VCD_GetVarInfo(VCD_Parser* parser,Arena* out);

// ======================================
// Snapshots

u64 VCD_GetSnapshotSizeInBytes(VCD_Parser* parser);
void VCD_TakeSnapshot(VCD_Parser* parser,VCD_Snapshot* snapBuffer);

Array<u32> VCD_GetAllChangedIndexes(VCD_Snapshot* snap,Arena* out);

u64 VCD_GetTimestamp(VCD_Parser* parser);
u64 VCD_GetTimestamp(VCD_Snapshot* parser);

VCD_Value* VCD_GetValue(VCD_Snapshot* snap,u32 index);

// ======================================
// State management

bool VCD_Done(VCD_Parser* parser);
void VCD_StartAdvance(VCD_Parser* parser);
bool VCD_AdvanceFinished(VCD_Parser* parser);

void VCD_Rollback(VCD_Parser* parser,VCD_Snapshot* snap);
