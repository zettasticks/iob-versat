#include "hierName.hpp"
#include "utilsCore.hpp"

static struct {
  Arena* arena;
  
  HIER_Node* hashTable[512];
} HIER_State;

static HIER_Name GetOrAllocate(HIER_Name parent,String name){
  if(Empty(name)){
    return parent;
  }

  u32 parentHash = 0;
  if(parent.node){
    parentHash = parent.node->hash;
  }

  u32 hash = Hash(name) + parentHash;
  u32 index = hash % 512;

  HIER_Node* before = nullptr;
  HIER_Node* ptr = HIER_State.hashTable[index];
  for(; ptr != nullptr; ptr = ptr->nextChain){
    if(ptr->name == name && ptr->parent == parent.node){
      return ptr;
    }
    before = ptr;
  }

  HIER_Node* newNode = PushStruct<HIER_Node>(HIER_State.arena);
  newNode->name = PushString(HIER_State.arena,name);
  newNode->parent = parent.node;
  
  if(before){
    before->nextChain = newNode;
  } else {
    HIER_State.hashTable[index] = newNode;
  }
  
  return newNode;
}

static Array<HIER_Name> ParentToChildNodes(HIER_Name start,Arena* out){
  TEMP_REGION(temp,out);

  auto list = PushList<HIER_Name>(temp);
  for(HIER_Name ptr = start; HIER_IsValid(ptr); ptr = HIER_GetParent(ptr)){
    *list->PushElem() = ptr;
  }
  Array<HIER_Name> asArray = PushArray(out,list);
  ReverseInPlace(asArray);
  
  return asArray;
}

HIER_Name::HIER_Name(String str){
  HIER_Name res = GetOrAllocate({},str);
  *this = res;
}

void HIER_Init(){
  static Arena arenaInst = InitArena(Megabyte(4));
  HIER_State.arena = &arenaInst;
}

bool HIER_IsValid(HIER_Name name){
  bool res = (name.node != nullptr);
  return res;
}

HIER_Name HIER_GetParent(HIER_Name base){
  if(base.node){
    HIER_Name parent = {base.node->parent};
    return parent;
  }

  return {};
}

String HIER_GetFullName(HIER_Name name,String separator,Arena* out){
  TEMP_REGION(temp,nullptr);

  Array<HIER_Name> nodes = ParentToChildNodes(name,temp);
  int size = nodes.size;
  Array<String> asString = PushArray<String>(temp,nodes.size);
  for(int i = 0; i < size; i++){
    asString[i] = nodes[i].node->name;
  }
  
  String res = JoinStrings(asString,separator,out);
  return res;
}

HIER_Name operator+(HIER_Name parent,HIER_Name bottom){
  TEMP_REGION(temp,nullptr);

  Array<HIER_Name> nodes = ParentToChildNodes(bottom,temp);
  
  HIER_Name ptr = parent;
  for(HIER_Name name : nodes){
    ptr = GetOrAllocate(ptr,name.node->name);
  }

  return ptr;
}
