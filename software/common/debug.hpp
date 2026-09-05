#pragma once

#include "utils.hpp"

struct Arena;

 // Call before any other debug function but after setting up general purpose arenas
void InitDebug(const char* exeName);

struct Location{
  String functionName;
  String fileName;
  u32 line;
};

extern Arena* debugArena;
extern bool debugFlag; // True if currently debugging
extern bool currentlyDebugging;

#define debugRegion() if(debugFlag)
#define debugRegionIf(COND) if(debugFlag && (COND))

typedef void (*SignalHandler)(int sig);

bool CurrentlyDebugging();
void SetDebugSignalHandler(SignalHandler func);

// NOTE: Main is index 0, CollectStackTrace is not in the results.
Array<Location> CollectStackTrace(Arena* out,int offset = 1);
void PrintStacktrace();

struct LocationNode{
  LocationNode* next;
  LocationNode* child;
  LocationNode* parent;

  Location loc;
  i32 level;
  iptr tag;
};

Location DEBUG_Copy(Location in,Arena* out);
// Returns tail
LocationNode* DEBUG_AddLocation_(LocationNode* topTail,Arena* out,iptr TAG = 0);
#define DEBUG_AddLocation(HEAD,TAIL,ARENA) DEBUG_AddLocationTagged(HEAD,TAIL,ARENA,0)
#define DEBUG_AddLocationTagged(HEAD,TAIL,ARENA,TAG) \
  TAIL = DEBUG_AddLocation_(TAIL,ARENA,TAG); \
  if(!HEAD){ \
    HEAD = TAIL; \
  }

String DEBUG_Repr(LocationNode* head,Arena* out);
Array<LocationNode*> DEBUG_DepthFirst(LocationNode* head,Arena* out);
