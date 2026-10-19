#pragma once

#include "utils.hpp"
#include "symbolic.hpp"

struct VEmitter;

// ======================================
// Schema

enum HW_SchemaType{
  HW_SchemaType_NIL = 0,
  HW_SchemaType_TEXT,
  HW_SchemaType_WIRE,
  HW_SchemaType_INTERFACE,
  HW_SchemaType_PORT
};

struct HW_SchemaNode{
  HW_SchemaNode* next;
  HW_SchemaType type;
  String content;
};

struct HW_Schema{
  HW_SchemaNode* head;
  char sep;
};

struct HW_SchemaBuild{
  HW_Schema* res;
  bool error; // Simple but we do not care about complex error reporting for something like this currently.
};

enum HW_WireProperty{
  HW_WireProperty_NIL = 0,
  HW_WireProperty_CONTROLLER_INPUT = (1 << 1), // Otherwise assumed output
  HW_WireProperty_IS_SHARED = (1 << 2), // For unpacking purposes, this wire can just be shared.
  HW_WireProperty_DATA = (1 << 4),
  HW_WireProperty_ADDR = (1 << 5),
};
C_STYLE_ENUM(HW_WireProperty);

struct HW_WireDef{
  String name;
  SYM_Expr defaultSize;
  HW_WireProperty prop;
};

struct HW_InterfaceSizeSelect{
  SYM_Expr addrSize;
};

struct HW_Interface{
  String name;
  Array<HW_WireDef> wires;
  HW_Schema* schema;
  int maxPorts; // Minimum is 1
};

struct HW_Wire{
  HW_WireDef* def;
  SYM_Expr size;
  int port;
};

struct HW_Instance{
  HW_Interface* inter;
  Array<HW_Wire> wires;
  int index;
};

enum HW_ValueType{
  HW_ValueType_NIL = 0,
  HW_ValueType_TEXT,
  HW_ValueType_WIRE,
  HW_ValueType_INTERFACE,
  HW_ValueType_PORT,
  HW_ValueType_DIRECTION
};

struct HW_Value{
  HW_Value* next;
  HW_ValueType type;
  String asStr;
  int asInt;
};

struct HW_ValueResult{
  HW_Value* val; // Contains all values in the order they are encountered.
  int port;
  int interface;
  int wireIndex;
  Direction dir;
  bool error;
};

namespace HW_Default{
  extern HW_Interface HW_DP;
  extern HW_Interface HW_2P;
  extern HW_Interface HW_DATABUS;
  extern HW_Interface HW_MM;
};
extern Array<HW_Interface*> HW_AllInterfaces;

// ======================================
// Init - create default interfaces

void HW_Init(Arena* perm);

// ======================================
// Type

HW_Instance HW_Copy(HW_Instance in,Arena* out);
HW_Instance HW_InstantiateParams(HW_Instance in,TrieMap<String,SYM_Expr>* paramReplace,Arena* out);

bool HW_IsMemory2P(HW_Interface* in);
bool HW_IsMemoryDP(HW_Interface* in);

bool HW_IsMemoryInterface(HW_Interface* in);
bool HW_IsMemoryMapped(HW_Interface* in);
bool HW_IsDatabus(HW_Interface* in);

// ======================================
// Helpers

HW_Wire HW_MakeWire(HW_WireDef* def,SYM_Expr size,int port);

// ======================================
// Schema building

HW_SchemaBuild HW_SchemaFromString(String format,char sep,Arena* out);
HW_Schema*     HW_RemoveFromSchema(HW_Schema* in,HW_SchemaType toRemove,Arena* out);

// ======================================
// Extract values 

HW_ValueResult HW_ExtractValues(HW_Interface* expectedInterface,String content,Arena* out);

// ======================================
// Instantiation

HW_Instance HW_Instantiate(HW_Interface* inter,HW_InterfaceSizeSelect select,Arena* out);

// ======================================
// Size getters

SYM_Expr HW_GetDataSize(HW_Instance in,int port,Direction dir = Direction_NONE);
SYM_Expr HW_GetAddrSize(HW_Instance in,int port);

// ======================================
// Repr

String HW_GetWireRepresentation(HW_Interface* inter,int wireIndex,int port,int interface,Arena* out);
void   HW_GetWireRepresentation(StringBuilder* b,HW_Interface* inter,int wireIndex,int port,int interface);

String HW_GetWireRepresentation(HW_Instance* inst,int wireIndex,Arena* out);
void   HW_GetWireRepresentation(StringBuilder* b,HW_Instance* inst,int wireIndex);

// ======================================
// Emission

void HW_EmitPortDeclaration(VEmitter* v,HW_Instance* inst,bool isController);
//void HW_EmitPortConnect(VEmitter* v,HW_Instance* inst,HW_Schema* wire);
void HW_EmitUnpacking(VEmitter* v,Array<HW_Instance> instances,String packedPrefix,String unpackedPrefix,char sep,bool isController);
