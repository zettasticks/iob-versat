#include "hardwareInterfaces.hpp"

namespace HW_Default{
  HW_Interface HW_DP;
  HW_Interface HW_2P;
  HW_Interface HW_DATABUS;
  HW_Interface HW_MM;
};
Array<HW_Interface*> HW_AllInterfaces;

void HW_Init(Arena* perm){
  using namespace HW_Default;

#define HW_INIT(NAME,FORMAT,COUNT) \
  { \
  auto build = HW_SchemaFromString(FORMAT,'_',perm); \
  Assert(!build.error); \
  NAME.schema = build.res; \
  NAME.wires = PushArray<HW_Wire>(perm,COUNT); \
  }

  HW_INIT(HW_DP,"dp_@{wire}_@{interface}_port_@{port}",5);
  HW_DP.wires[0].name = "addr";
  HW_DP.wires[1].name = "out";
  HW_DP.wires[2].name = "in";
  HW_DP.wires[3].name = "write";
  HW_DP.wires[4].name = "enable";

  HW_INIT(HW_2P,"2p_@{wire}_@{direction}_@{interface}",4);
  HW_2P.wires[0].name = "addr";
  HW_2P.wires[1].name = "data";
  HW_2P.wires[2].name = "write";
  HW_2P.wires[3].name = "read";

  HW_INIT(HW_DATABUS,"databus_@{wire}_@{interface}",8);
  HW_DATABUS.wires[0].name = "ready";
  HW_DATABUS.wires[1].name = "rvalid";
  HW_DATABUS.wires[2].name = "addr";
  HW_DATABUS.wires[3].name = "rdata";
  HW_DATABUS.wires[4].name = "wdata";
  HW_DATABUS.wires[5].name = "wstrb";
  HW_DATABUS.wires[6].name = "len";
  HW_DATABUS.wires[7].name = "last";

  HW_INIT(HW_MM,"@{wire}",6);
  HW_MM.wires[0].name = "rvalid";
  HW_MM.wires[1].name = "valid";
  HW_MM.wires[2].name = "addr";
  HW_MM.wires[3].name = "rdata";
  HW_MM.wires[4].name = "wdata";
  HW_MM.wires[5].name = "wstrb";

#undef HW_INIT

  HW_AllInterfaces = PushArray<HW_Interface*>(perm,4);
  HW_AllInterfaces[0] = &HW_DP;
  HW_AllInterfaces[1] = &HW_2P;
  HW_AllInterfaces[2] = &HW_DATABUS;
  HW_AllInterfaces[3] = &HW_MM;
}

HW_SchemaBuild HW_SchemaFromString(String format,char sep,Arena* out){
  const char* ptr = format.data;
  const char* end = ptr + format.size;

#define HW_SCAN(CH) while(ptr < end && *ptr != CH) ptr += 1;

  HW_Schema* head = 0;
  HW_Schema* tail = 0;

  bool error = 0;
  while(!error && ptr < end){
    const char* loopStart = ptr;
    char ch = *ptr;

    if(ch == sep){
      ptr += 1;
      continue;
    }

    String content = {};
    HW_SchemaType type = HW_SchemaType_NIL;
    if(ch == '@'){
      ptr += 1;
      if(*ptr != '{'){
        Assert(false);
        error = 1;
        break;
      }
      ptr += 1;

      const char* start = ptr;
      HW_SCAN('}');
      const char* end = ptr;
      ptr += 1;

      String typeStr = String(start,end - start);
      if(typeStr == "wire"){
        type = HW_SchemaType_WIRE;
      } else if(typeStr == "interface"){
        type = HW_SchemaType_INTERFACE;
      } else if(typeStr == "port"){
        type = HW_SchemaType_PORT;
      } else if(typeStr == "direction"){
        type = HW_SchemaType_DIRECTION;
      } else {
        Assert(false);
        error = 1;
      }
    } else {
      const char* start = ptr;
      if(sep != '\0'){
        HW_SCAN(sep);
      } else {
        HW_SCAN('@');
      }
      
      content = String(start,ptr - start);
      type = HW_SchemaType_TEXT;
    }

    Assert(!error);
    HW_Schema* newSchema = PushStruct<HW_Schema>(out);
    newSchema->content = PushString(out,content);
    newSchema->type = type;
    LL_Append(head,tail,next,newSchema);
  }

#undef HW_SCAN

  HW_SchemaBuild res = {};
  res.res = head;
  res.error = error;
  return res;
}

HW_ValueResult HW_ExtractValues(HW_Interface* expectedInterface,String content,char sep,Arena* out){
  const char* ptr = content.data;
  const char* end = ptr + content.size;

#define HW_SCAN(CH) while(ptr < end && *ptr != CH) ptr += 1;

  HW_Value* head = 0;
  HW_Value* tail = 0;

  int port = 0;
  int interface = 0;
  int wireIndex = 0;
  Direction dir = {};

  bool error = 0;
  bool lastWasSep = 0;
  HW_Schema* current = expectedInterface->schema;
  while(!error & ptr < end){
    char ch = *ptr;
    lastWasSep = 0;
    if(ch == sep){
      ptr += 1;
      lastWasSep = 1;
      continue;
    }

    // Get content and figure out type ============================================
    const char* loopStart = ptr;
    HW_SCAN(sep);

    String content = String(loopStart,ptr - loopStart);
    if(content.size == 0 && current){
      error = 1;
      break;
    }
    if(content.size != 0 && !current){
      error = 1;
      break;
    }

    // Match with schema ==========================================================
    bool isNumber = 1;
    for(int i = 0; i < content.size; i++){
      if(!(content[i] >= '0' && content[i] <= '9')){
        isNumber = 0;
      }
    }

    int number = 0;
    if(isNumber){
      number = ParseInt(content);
    }

    bool advance = 0;
    HW_ValueType type = HW_ValueType_NIL;
    String asStr = {};
    int asInt = 0;

    if(current->type == HW_SchemaType_TEXT ||
       current->type == HW_SchemaType_WIRE){
      if(isNumber){
        error = 1;
        break;
      }

      bool found = 0;
      if(!found && current->type == HW_SchemaType_TEXT){
        if(current->content == content){
          advance = 1;
          asStr = content;
          found = 1;
          type = HW_ValueType_NIL;
        }
      }

      if(!found && current->type == HW_SchemaType_DIRECTION){
        if(current->content == "out"){
          advance = 1;
          dir = Direction_OUTPUT;
          found = 1;
          type = HW_ValueType_DIRECTION;
        }
        if(current->content == "in"){
          advance = 1;
          dir = Direction_INPUT;
          found = 1;
          type = HW_ValueType_DIRECTION;
        }
      }

      if(!found && current->type == HW_SchemaType_WIRE){
        String wireName = content;

        for(int i = 0; i <  expectedInterface->wires.size; i++){
          HW_Wire sig = expectedInterface->wires[i];
          if(sig.name == wireName){
            asInt = i;
            wireIndex = i;
            advance = 1;
            found = 1;
            type = HW_ValueType_WIRE;
            break;
          }
        }
      }

      if(!found){
        asStr = content;
        type = HW_ValueType_TEXT;
      }
    }
    if(current->type == HW_SchemaType_PORT || 
       current->type == HW_SchemaType_INTERFACE){
      if(!isNumber){
        error = 1;
        break;
      }

      if(current->type == HW_SchemaType_PORT){ 
        type = HW_ValueType_PORT;
        port = number;
      };
      if(current->type == HW_SchemaType_INTERFACE){ 
        type = HW_ValueType_INTERFACE;
        interface = number;
      };

      asInt = number;
      advance = 1;
    }

    if(advance){
      current = current->next;
    }

    if(type != HW_ValueType_NIL){
      HW_Value* val = PushStruct<HW_Value>(out);
      val->asInt = asInt;
      val->asStr = PushString(out,asStr);
      val->type = type;
      LL_Append(head,tail,next,val);
    }
  }

  if(lastWasSep){ // A terminating sep with nothing following it is an error.
    error = 1;
  }
  if(current){
    error = 1;
  }

#undef HW_SCAN

  // Pack result ================================================================
  HW_ValueResult res = {};
  res.port = port;
  res.interface = interface;
  res.wireIndex = wireIndex;
  res.dir = dir;
  res.val = head;
  res.error = error;
  return res;
}
