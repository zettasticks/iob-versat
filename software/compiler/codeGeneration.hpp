
#pragma once

#include "utilsCore.hpp"
#include "verilogParsing.hpp"
#include "utils.hpp"

static const int DELAY_SIZE = 7;

struct Accelerator;
struct InstanceInfo;
struct FUDeclaration;

// TODO: Config generation could be simplified if we instead had a immediate mode like interface for the specific of the structures. Something like a list of members and unions could be representing using a start union/end union command and the likes. Would simplify struct generation, but struct information would still need to be gathered from StructInfo

// Type can differ because of Merge.
struct SingleTypeStructElement{
  String type;
  String name;
  int arraySize; // Zero or one represent same thing: no array.
};

struct TypeStructInfoElement{
  Array<SingleTypeStructElement> typeAndNames; // Because of config sharing, we can have multiple elements occupying the same position. Type can also differ because of merge.
};

struct TypeStructInfo{
  String name;
  Array<TypeStructInfoElement> entries;
};

struct Difference{
  int index;
  int newValue;
};

struct DifferenceArray{
  int oldIndex;
  int newIndex;

  Array<Difference> differences;
};

struct MuxInfo{
  int configIndex;
  int val;
  String name;
  InstanceInfo* info;
};

struct WireInformation{
  Wire wire;
  int addr;
  int configBitStart;
  bool isStatic;
};

struct SameMuxEntities{
  int configPos;
  InstanceInfo* info;
};

template<> struct std::hash<SameMuxEntities>{
   std::size_t operator()(SameMuxEntities const& s) const noexcept{
     std::size_t res = s.configPos;
     return res;
   }
};

static bool operator==(const SameMuxEntities i0,const SameMuxEntities i1){
  bool res = (i0.configPos == i1.configPos);
  return res;
}

struct AddressGenDef;
extern Pool<AddressGenDef> savedAddressGen;

// In order to get the names, I need to associate a given member to a merge index.
// Since multiplexers can be shared accross multiple merge indexes, I need some map of some sorts.
// One multiplexer can be used by dozens of merge indexes.
struct StructInfo;

struct StructElement{
  StructInfo* childStruct; // If nulltpr, then leaf (whose type is type)
  String name;
  int pos;
  int size;
  bool isMergeMultiplexer;
};

struct StructInfo{
  String name;
  FUDeclaration* type;
  StructInfo* parent;

  ArenaDoubleList<StructElement>* list;
};

size_t HashStructInfo(StructInfo* info);

template<> struct std::hash<StructElement>{
   std::size_t operator()(StructElement const& s) const noexcept{
     std::size_t res = HashStructInfo(s.childStruct) + std::hash<String>()(s.name) + s.size + (s.isMergeMultiplexer ? 1 : 0);
     return res;
   }
};

template<> struct std::hash<StructInfo>{
   std::size_t operator()(StructInfo const& s) const noexcept{
     std::size_t res = 0;
     res += std::hash<void*>()(s.type);
     for(DoubleLink<StructElement>* ptr = s.list ? s.list->head : nullptr; ptr; ptr = ptr->next){
       res += std::hash<StructElement>()(ptr->elem);
     }
     res += std::hash<String>()(s.name);
     return res;
   }
};

static bool operator==(StructInfo& l,StructInfo& r);

static bool operator==(StructElement& l,StructElement& r){
  bool res = (*l.childStruct == *r.childStruct &&
              l.name == r.name &&
              l.size == r.size &&
              l.isMergeMultiplexer == r.isMergeMultiplexer);
  return res;
}

static bool operator==(StructInfo& l,StructInfo& r){
  if(l.type != r.type){
    return false;
  }

  DoubleLink<StructElement>* lPtr = l.list ? l.list->head : nullptr;
  DoubleLink<StructElement>* rPtr = r.list ? r.list->head : nullptr;
  for(; lPtr && rPtr; lPtr = lPtr->next,rPtr = rPtr->next){
    if(!(lPtr->elem == rPtr->elem)){
      return false;
    }
  }

  if(!(lPtr) != !(rPtr)){
    return false;
  }
  
  return (l.name == r.name);
}

Array<FUDeclaration*> SortTypesByMemDependency(Array<FUDeclaration*> types,Arena* out);

void OutputCircuitSource(FUDeclaration* decl,FILE* file);
void OutputIterativeSource(FUDeclaration* decl,FILE* file);
void OutputTopLevelFiles(Accelerator* accel,FUDeclaration* topLevelDecl,const char* hardwarePath,const char* softwarePath,bool isSimple);
