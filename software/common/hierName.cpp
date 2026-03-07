#include "hierName.hpp"
#include "utilsCore.hpp"

static struct {
  Arena* arena;
  
  HIER_Node* hashTable[512];
} HIER_State;

static HIER_Name GetOrAllocate(String name,HIER_Name parent){
  
}

void HIER_Init();

String HIER_GetFullName(HIER_Name name,String separator,Arena* out){
  NOT_IMPLEMENTED();
}

HIER_Name HIER_BaseName(String name){
  NOT_IMPLEMENTED();
}

HIER_Name operator+(HIER_Name parent,HIER_Name bottom){
  NOT_IMPLEMENTED();
}

