
#pragma once

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
};

struct SubTypesInfo{
  FUDeclaration* type;
  FUDeclaration* mergeTop;
  bool isFromMerged;
  bool containsMerged;
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

template<> struct std::hash<SubTypesInfo>{
   std::size_t operator()(SubTypesInfo const& s) const noexcept{
     std::size_t res = std::hash<void*>()(s.type) + std::hash<void*>()(s.mergeTop) + std::hash<bool>()(s.isFromMerged) + std::hash<bool>()(s.containsMerged);
     return res;
   }
};

static bool operator==(const SubTypesInfo i0,const SubTypesInfo i1){
  bool res = Memcmp(&i0,&i1,1);
  return res;
}

struct AddressGenDef;
extern Pool<AddressGenDef> savedAddressGen;

// In order to get the names, I need to associate a given member to a merge index.
// Since multiplexers can be shared accross multiple merge indexes, I need some map of some sorts.
// One multiplexer can be used by dozens of merge indexes.
enum StructInfoType{
  StructInfoType_UNION_WITH_MERGE_MULTIPLEXERS
};

struct StructElement;

struct StructInfo{
  String name;
  FUDeclaration* type;

  Array<StructElement> elements;
};

struct StructElement{
  StructInfo* childStruct; // If nulltpr, then leaf (whose type is type)
  String name;
  int pos;
  int size;
  bool isMergeMultiplexer;
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
     for(StructElement elem : s.elements){
       res += std::hash<StructElement>()(elem);
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
  if(l.elements.size != r.elements.size){
    return false;
  }
  if(l.type != r.type){
    return false;
  }
  for(int i = 0; i < l.elements.size; i++){
    if(!(l.elements[i] == r.elements[i])){
      return false;
    }
  }

  return (l.name == r.name);
}

// Lets not handle merge for now.

// Because of merge, we can potentially have different "views" of the modules structures.
// Although they have to line up in regards to the config pos of every unit.
// We cannot have one view have base x type in config pos 0 and another have base y type in config pos 0 as well.
// Although we can have the config of modules shared.
// We can have Test1MergeConfig in pos 0 and Test2MergeConfig in pos 0 at the same time (union).

// The biggest problem is when the base unit of one module belongs to a merge type but not to the other base type.
// In that case, we end up with 2 different types occupying the same position.

// For now, I do not care about position difference. We could just have multiple StructElements with the same pos and then we could extract the union semantics from detecting this.

// The most important part is to generate the StructElements that contain the differences. Worry about padding and union later.

Array<FUDeclaration*> SortTypesByConfigDependency(Array<FUDeclaration*> types,Arena* out,Arena* temp);
Array<FUDeclaration*> SortTypesByMemDependency(Array<FUDeclaration*> types,Arena* out,Arena* temp);
Array<TypeStructInfoElement> GenerateStructFromType(FUDeclaration* decl,Arena* out,Arena* temp);

void OutputCircuitSource(FUDeclaration* decl,FILE* file,Arena* temp,Arena* temp2);
void OutputIterativeSource(FUDeclaration* decl,FILE* file,Arena* temp,Arena* temp2);
void OutputVerilatorWrapper(FUDeclaration* type,Accelerator* accel,String outputPath,Arena* temp,Arena* temp2);
void OutputVerilatorMake(String topLevelName,String versatDir,Arena* temp,Arena* temp2);
void OutputVersatSource(Accelerator* accel,const char* hardwarePath,const char* softwarePath,bool isSimple,Arena* temp,Arena* temp2);
