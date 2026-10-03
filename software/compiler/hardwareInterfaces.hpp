#pragma once

#include "utils.hpp"
#include "symbolic.hpp"

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
  HW_WireProperty_ADDR = (1 << 1),
};
C_STYLE_ENUM(HW_WireProperty);

struct HW_WireDef{
  String name;
  HW_WireProperty prop;
};

struct HW_Interface{
  String name;
  Array<HW_WireDef> wires;
  HW_Schema* schema;
  int maxPorts; // Minimum is 1
};

struct HW_Wire{
  SYM_Expr size;
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
// Schema building

HW_SchemaBuild HW_SchemaFromString(String format,char sep,Arena* out);

// ======================================
// Extract values from string 

HW_ValueResult HW_ExtractValues(HW_Interface* expectedInterface,String content,Arena* out);

// ======================================
// Repr

String HW_GetWireRepresentation(HW_Interface* inter,int wireIndex,int port,int interface,Arena* out);

