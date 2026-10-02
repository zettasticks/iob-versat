#pragma once

#include "utils.hpp"

// ======================================
// Schema

enum HW_SchemaType{
  HW_SchemaType_NIL = 0,
  HW_SchemaType_TEXT,
  HW_SchemaType_WIRE,
  HW_SchemaType_INTERFACE,
  HW_SchemaType_PORT,
  HW_SchemaType_DIRECTION
};

struct HW_Schema{
  HW_Schema* next;
  HW_SchemaType type;
  String content;
};

struct HW_SchemaBuild{
  HW_Schema* res;
  bool error; // Simple but we do not care about complex error reporting for something like this currently.
};

enum HW_SignalProps{
  HW_SignalProps_NIL = 0,
};

struct HW_Wire{
  String name;
};

struct HW_Interface{
  Array<HW_Wire> wires;
  HW_Schema* schema;
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

HW_ValueResult HW_ExtractValues(HW_Interface* expectedInterface,String content,char sep,Arena* out);

