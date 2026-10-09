#include "hardwareInterfaces.hpp"

#include "VerilogEmitter.hpp"

namespace HW_Default{
  HW_Interface HW_DP;
  HW_Interface HW_2P;
  HW_Interface HW_DATABUS;
  HW_Interface HW_MM;
};
Array<HW_Interface*> HW_AllInterfaces;

void HW_Init(Arena* perm){
  using namespace HW_Default;

#define HW_INIT(NAME,REPR,FORMAT,COUNT,PORTS) \
  { \
  auto build = HW_SchemaFromString(FORMAT,'_',perm); \
  Assert(!build.error); \
  NAME.name = REPR; \
  NAME.schema = build.res; \
  NAME.wires = PushArray<HW_WireDef>(perm,COUNT); \
  NAME.maxPorts = PORTS; \
  }

  HW_INIT(HW_DP,"DP","ext_dp_@{wire}_@{interface}_port_@{port}",5,2);
  HW_DP.wires[0] = {"addr",SYM_AddrW,HW_WireProperty_ADDR};
  HW_DP.wires[1] = {"out",SYM_DataW};
  HW_DP.wires[2] = {"in",SYM_DataW,HW_WireProperty_CONTROLLER_INPUT};
  HW_DP.wires[3] = {"write",SYM_1};
  HW_DP.wires[4] = {"enable",SYM_1};
  
  HW_INIT(HW_2P,"2P","ext_2p_@{wire}_@{interface}",6,1);
  HW_2P.wires[0] = {"addr_out",SYM_AddrW,HW_WireProperty_ADDR};
  HW_2P.wires[1] = {"addr_in",SYM_AddrW,HW_WireProperty_ADDR};
  HW_2P.wires[2] = {"data_out",SYM_DataW};
  HW_2P.wires[3] = {"data_in",SYM_DataW,HW_WireProperty_CONTROLLER_INPUT};
  HW_2P.wires[4] = {"write",SYM_1};
  HW_2P.wires[5] = {"read",SYM_1};

  HW_INIT(HW_DATABUS,"DATABUS","databus_@{wire}_@{interface}",8,1);
  HW_DATABUS.wires[0] = {"ready",SYM_1,HW_WireProperty_CONTROLLER_INPUT};
  HW_DATABUS.wires[1] = {"rvalid",SYM_1};
  HW_DATABUS.wires[2] = {"addr",SYM_AddrW,HW_WireProperty_ADDR};
  HW_DATABUS.wires[3] = {"rdata",SYM_DataW,HW_WireProperty_CONTROLLER_INPUT | HW_WireProperty_IS_SHARED};
  HW_DATABUS.wires[4] = {"wdata",SYM_DataW};
  HW_DATABUS.wires[5] = {"wstrb",SYM_DataStrobeW};
  HW_DATABUS.wires[6] = {"len",SYM_LenW};
  HW_DATABUS.wires[7] = {"last",SYM_1,HW_WireProperty_CONTROLLER_INPUT};

  HW_INIT(HW_MM,"MM","@{wire}",6,1);
  HW_MM.wires[0] = {"rvalid",SYM_1};
  HW_MM.wires[1] = {"valid",SYM_1,HW_WireProperty_CONTROLLER_INPUT};
  HW_MM.wires[2] = {"addr",SYM_AddrW,HW_WireProperty_CONTROLLER_INPUT | HW_WireProperty_ADDR};
  HW_MM.wires[3] = {"rdata",SYM_DataW};
  HW_MM.wires[4] = {"wdata",SYM_DataW,HW_WireProperty_CONTROLLER_INPUT};
  HW_MM.wires[5] = {"wstrb",SYM_DataStrobeW,HW_WireProperty_CONTROLLER_INPUT};
  
#undef HW_INIT

  HW_AllInterfaces = PushArray<HW_Interface*>(perm,4);
  HW_AllInterfaces[0] = &HW_DP;
  HW_AllInterfaces[1] = &HW_2P;
  HW_AllInterfaces[2] = &HW_DATABUS;
  HW_AllInterfaces[3] = &HW_MM;
}

// ======================================
// Type

bool HW_IsExternalMemory(HW_Interface* in){
  bool res = (in == &HW_Default::HW_DP || in == &HW_Default::HW_2P);
  return res;
}

HW_Instance HW_Copy(HW_Instance in,Arena* out){
  HW_Instance res = {};
  res.index = in.index;
  res.inter = in.inter;
  res.wires = CopyArray(in.wires,out);
  return res;
}

// ======================================
// Helpers

HW_Wire HW_MakeWire(HW_WireDef* def,SYM_Expr size,int port){
  HW_Wire res = {};
  res.def = def;
  res.size = size;
  res.port = port;
  return res;
}

// ======================================
// Schema building

HW_SchemaBuild HW_SchemaFromString(String format,char sep,Arena* out){
  const char* ptr = format.data;
  const char* end = ptr + format.size;

#define HW_SCAN(CH) while(ptr < end && *ptr != CH) ptr += 1;

  HW_SchemaNode* head = 0;
  HW_SchemaNode* tail = 0;

  bool error = 0;
  while(!error && ptr < end){
    UNUSED const char* loopStart = ptr;
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
    HW_SchemaNode* newSchema = PushStruct<HW_SchemaNode>(out);
    newSchema->content = PushString(out,content);
    newSchema->type = type;
    LL_Append(head,tail,next,newSchema);
  }

#undef HW_SCAN

  HW_Schema* schema = PushStruct<HW_Schema>(out);
  schema->sep = sep;
  schema->head = head;

  HW_SchemaBuild res = {};
  res.res = schema;
  res.error = error;
  return res;
}

// ======================================
// Extract values 

HW_ValueResult HW_ExtractValues(HW_Interface* expectedInterface,String content,Arena* out){
  const char sep = expectedInterface->schema->sep;
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
  HW_SchemaNode* current = expectedInterface->schema->head;
  while(!error && current && ptr < end){
    char ch = *ptr;
    lastWasSep = 0;
    if(ch == sep){
      ptr += 1;
      lastWasSep = 1;
      continue;
    }

    bool advance = 0;
    HW_ValueType type = HW_ValueType_NIL;
    String asStr = {};
    int asInt = 0;

    // Check if matches text based schema first ===================================
    bool found = 0;
    
    if(!found && current->type == HW_SchemaType_TEXT){
      String toCheck = current->content;
      const char* checkEnd = ptr + toCheck.size;

      if(checkEnd <= end){
        String check = String(ptr,checkEnd - ptr);

        if(toCheck == check){
          ptr += check.size;
          found = 1;
          advance = 1;
        }
      }
    }
    if(!found && current->type == HW_SchemaType_WIRE){
      for(int i = 0; i <  expectedInterface->wires.size; i++){
        HW_WireDef sig = expectedInterface->wires[i];
        String toCheck = sig.name;
        const char* checkEnd = ptr + toCheck.size;

        if(checkEnd <= end){
          String check = String(ptr,checkEnd - ptr);

          if(toCheck == check){
            ptr += check.size;
            found = 1;
            advance = 1;
            type = HW_ValueType_WIRE;
            asInt = i;
            wireIndex = i;
            break;
          }
        }
      }
    }

    if(!found){
      // Separate based on sep and check remaining cases ============================
      const char* loopStart = ptr;
      HW_SCAN(sep);
      String content = String(loopStart,ptr - loopStart);
      if((content.size == 0 && current) || (content.size != 0 && !current)){
        error = 1;
        break;
      }

      // Check if number and parse it ===============================================
      bool isNumber = IsNumber(content);
      int number = 0;
      if(isNumber){
        number = ParseInt(content);
      }

      if(!isNumber){
        asStr = content;
        type = HW_ValueType_TEXT;
        found = 1;
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
        found = 1;
      }
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

// ======================================
// Instantiation

HW_Instance HW_Instantiate(HW_Interface* inter,HW_InterfaceSizeSelect select,Arena* out){
  HW_Instance res = {};
  res.inter = inter;
  res.wires = PushArray<HW_Wire>(out,inter->wires.size * inter->maxPorts);

  for(int port = 0; port < inter->maxPorts; port++){
    for(int i = 0; i <  res.inter->wires.size; i++){
      int index = port * res.inter->wires.size + i;
      
      HW_WireDef& def = res.inter->wires[i];
      SYM_Expr size = def.defaultSize;

      if(def.prop & HW_WireProperty_ADDR){
        size = select.addrSize;
      }

      res.wires[index].size = def.defaultSize;
      res.wires[index].def = &def;
      res.wires[index].port = port;
    }
  }

  return res;
}

// ======================================
// Repr

String HW_GetWireRepresentation(HW_Interface* inter,int wireIndex,int port,int interface,Arena* out){
  TEMP_REGION(temp,out);

  auto b = StartString(temp);
  HW_GetWireRepresentation(b,inter,wireIndex,port,interface);
  String res = EndString(out,b);
  return res;
}

void HW_GetWireRepresentation(StringBuilder* b,HW_Interface* inter,int wireIndex,int port,int interface){
  HW_SchemaNode* head = inter->schema->head;
  char sep = inter->schema->sep;

  for(HW_SchemaNode* ptr = head; ptr; ptr = ptr->next){
    bool first = (ptr == head);
    if(!first){
      b->PushChar(sep);
    }

    switch(ptr->type){
    case HW_SchemaType_TEXT:{
      b->PushString(ptr->content);
    } break;
    case HW_SchemaType_WIRE:{
      b->PushString(inter->wires[wireIndex].name);
    } break;
    case HW_SchemaType_PORT: b->PushString("%d",port); break;
    case HW_SchemaType_INTERFACE: b->PushString("%d",interface); break;
    }
  }
}

String HW_GetWireRepresentation(HW_Instance* inst,int wireIndex,Arena* out){
  TEMP_REGION(temp,out);

  auto b = StartString(temp);
  HW_GetWireRepresentation(b,inst,wireIndex);
  String res = EndString(out,b);
  return res;
}

void HW_GetWireRepresentation(StringBuilder* b,HW_Instance* inst,int wireIndex){
  HW_Interface* inter = inst->inter;
  HW_SchemaNode* head = inter->schema->head;
  char sep = inter->schema->sep;
  HW_Wire w = inst->wires[wireIndex];
  int port = w.port;
  int interface = inst->index;
  
  for(HW_SchemaNode* ptr = head; ptr; ptr = ptr->next){
    bool first = (ptr == head);
    if(!first){
      b->PushChar(sep);
    }

    switch(ptr->type){
    case HW_SchemaType_TEXT:{
      b->PushString(ptr->content);
    } break;
    case HW_SchemaType_WIRE:{
      b->PushString(w.def->name);
    } break;
    case HW_SchemaType_PORT: b->PushString("%d",port); break;
    case HW_SchemaType_INTERFACE: b->PushString("%d",interface); break;
    }
  }
}

// ======================================
// Emission

void HW_EmitPortDeclaration(VEmitter* v,HW_Instance* inst,bool isController){
  TEMP_REGION(temp,v->arena);

  HW_Interface* inter = inst->inter;
  HW_Schema* schema = inter->schema;
  int interfaceIndex = inst->index;

  for(int i = 0; i <  inst->wires.size; i++){
    HW_Wire w = inst->wires[i];
    bool isControllerInput = (w.def->prop & HW_WireProperty_CONTROLLER_INPUT);
    bool isInput = (isControllerInput && isController);
    SYM_Expr size = w.size;

    String portName = HW_GetWireRepresentation(inst,i,temp);

    if(isInput){
      v->Input(portName,w.size);
    } else {
      v->Output(portName,w.size);
    }
  }
}

void HW_EmitUnpacking(VEmitter* v,Array<HW_Instance> instances,String packedPrefix,String unpackedPrefix,bool isController){
  TEMP_REGION(temp,v->arena);

  if(instances.size == 0){
    return;
  }

  {
    HW_Instance check = instances[0];
    for(HW_Instance inst : instances){
      Assert(inst.inter == check.inter && "We can only unpack if every hardware instance belongs to the same interface");
    }
  }

  int wireSize = instances[0].wires.size;
  Array<SYM_Expr> totalSizes = PushArray<SYM_Expr>(temp,wireSize);

  // Calculate wire sizes =======================================================
  for(HW_Instance inst : instances){
    Assert(wireSize == inst.wires.size && "All input instances must have the same wire sizes");
    for(int i = 0; i < wireSize; i++){
      totalSizes[i] = totalSizes[i] + inst.wires[i].size;
    }
  }

  for(int i = 0; i < wireSize; i++){
    printf("B:");
    SYM_Print(totalSizes[i]);
    printf("\n");
  }

  - LEFT HERE - Finish this

  HW_Interface* inter = instances[0].inter;
  for(HW_Instance inst : instances){
    
  }
}
