#include <fstapi.h>

#include "tools/libfst/src/fstapi.h"
#include "utils.hpp"

// ======================================
// Globals

static u64 lastTime;
static int progressReportIndex = 0;

enum FST_NodeType{
  FST_NodeType_Empty,
  FST_NodeType_VAR,
  FST_NodeType_SCOPE,
};

struct FST_Node{
  FST_Node* next;
  FST_Node* child;
  FST_Node* parent;

  FST_NodeType type;

  u32 handle;
  String name;
};

struct FST_SingleBitInfo{
  u64 timeSpent0;
  u64 timeSpent1;
  u64 timeSpentX;
  u64 timeSpentZ;
  u64 toggleCount;
};

enum FST_VarType{
  FST_VarType_UNKNOWN,
  FST_VarType_WIRE,
  FST_VarType_REG,
  FST_VarType_PARAMETER,
};

struct FST_VarInfo{
  FST_VarType type;

  int numberOfBits;
  char* lastBitPatternBuffer;
  
  u64 lastTime;
  FST_SingleBitInfo* singleBits;
};

struct FST_State{
  Array<FST_VarInfo> varInfoMap;
};

void Callback(void *user_callback_data_pointer,
              uint64_t time,
              fstHandle handle,
              const unsigned char *value){
  
  FST_State* state = (FST_State*) user_callback_data_pointer;
  FST_VarInfo& info = state->varInfoMap[handle];

  if(time > ((lastTime / 100) * progressReportIndex)){
    progressReportIndex += 1;
    printf("%lu/%lu\n",time,lastTime);
  }
  
  uint64_t timeDiff = time - info.lastTime;
  for(int i = 0; i < info.numberOfBits; i++){
    FST_SingleBitInfo& bit = info.singleBits[i];
    char lastBit = info.lastBitPatternBuffer[i];
    
    switch(lastBit){
    case '0':{
      bit.timeSpent0 += timeDiff;
    } break;
    case '1':{
      bit.timeSpent1 += timeDiff;
    } break;
    case 'Z':
    case 'z':{
      bit.timeSpentZ += timeDiff;
    } break;
    case 'X':
    case 'x':{
      bit.timeSpentX += timeDiff;
    } break;
    default: Assert(false);
    }
    
    bit.toggleCount += 1;

    info.lastBitPatternBuffer[i] = value[i];
  }

  info.lastTime = time;
}

int main(int argc,const char* argv[]){
  Arena tempInst = InitArena(Megabyte(128));
  Arena temp2Inst = InitArena(Megabyte(128));

  contextArenas[0] = &tempInst;
  contextArenas[1] = &temp2Inst;

  for(int i = 0; i < 8; i++){
    singleUseCasesArenas[i] = InitArena(Megabyte(1));
  }

  TEMP_REGION(temp,nullptr);

  fstReaderContext *reader = fstReaderOpen("uut.fst");

  fstReaderResetScope(reader);

  if (!fstReaderProcessHier(reader, NULL)){
    printf("Error\n");
  }
  fstReaderSetFacProcessMaskAll(reader);

  Arena* out = temp;

  fstHandle maxHandle = fstReaderGetMaxHandle(reader);
  Array<FST_VarInfo> handleToVarInfo = PushArray<FST_VarInfo>(out,maxHandle + 1);

  // Build hierarchy ============================================================
  FST_Node* top = nullptr;
  FST_Node* ptr = nullptr;
  int index = 0;
  while(1){
    fstHier * hier = fstReaderIterateHier(reader);
    
    if(!hier){
      break;
    }

    switch(hier->htyp){
    case FST_HT_SCOPE:{
      // Add scope node to current scope ============================================
      FST_Node* scopeNode = PushStruct<FST_Node>(out);
      if(ptr){
        scopeNode->parent = ptr->parent;
      }
      scopeNode->type = FST_NodeType_SCOPE;

      String parserName = String(hier->u.scope.name,hier->u.scope.name_length);

      String name = PushString(out,parserName);
      scopeNode->name = name;
      
      if(!top){
        top = scopeNode;
        ptr = scopeNode;
      } else {
        ptr->next = scopeNode;
        ptr = ptr->next;
      }

      // Add empty scope starting node ==============================================
      FST_Node* emptyStart = PushStruct<FST_Node>(out);
      emptyStart->parent = scopeNode;
      emptyStart->type = FST_NodeType_Empty;      

      ptr->child = emptyStart;
      ptr = ptr->child;
    } break;
    case FST_HT_VAR:{
      FST_Node* var = {};

      bool useEmpty = false;
      if(ptr->type == FST_NodeType_Empty){
        useEmpty = true;
        var = ptr;
      } else {
        var = PushStruct<FST_Node>(out);
      }

      int bitSize = hier->u.var.length;
      String parserName = String(hier->u.var.name,hier->u.var.name_length);
      String finalName = parserName;

      bool saveName = true;
      for(int i = 0; i < parserName.size; i++){
        if(parserName[i] == ' '){
          if(bitSize == 1){
            finalName = PushString(out,"%.*s\\[0\\]",i,parserName.data);
          } else {
            finalName = PushString(out,"%.*s",i,parserName.data);
          }

          saveName = false;
        }
      }

      if(saveName){
        finalName = PushString(out,finalName);
      }

      printf("%d %.*s\n",hier->u.var.typ,UN(finalName));

      u32 handle = hier->u.var.handle;

      var->type = FST_NodeType_VAR;
      var->handle = handle;
      var->name = finalName;

      if(!handleToVarInfo[handle].lastBitPatternBuffer){
        FST_VarInfo& info = handleToVarInfo[handle];

        switch(hier->u.var.typ){
          case FST_VT_VCD_INTEGER: {
            info.type = FST_VarType_UNKNOWN;
          } break;
          case FST_VT_VCD_PARAMETER: {
            info.type = FST_VarType_PARAMETER;
          } break;
          case FST_VT_VCD_WIRE: {
            info.type = FST_VarType_WIRE;
          } break;
          case FST_VT_SV_LOGIC: {
            info.type = FST_VarType_REG;
          } break;
          default:{
            // Do not expect anything different.
            // TODO: For now we assert since I want program to terminate if we find a non expected type.
            printf("Unexpected type: %d %s\n",hier->u.var.typ,hier->u.var.name);
            Assert(false);
          } break;
        }


        info.numberOfBits = bitSize;
        info.lastBitPatternBuffer = (char*) PushBytes(out,info.numberOfBits);
        memset(info.lastBitPatternBuffer,'x',info.numberOfBits);
        info.singleBits = PushArray<FST_SingleBitInfo>(out,info.numberOfBits).data;
      }

      if(!useEmpty){
        var->parent = ptr->parent;

        ptr->next = var;
        ptr = ptr->next;
      }
    } break;
    case FST_HT_ATTRBEGIN:{
    } break;
    case FST_HT_UPSCOPE:{
      ptr = ptr->parent;
      
      while(ptr->next){
        ptr = ptr->next;
      }
    } break;
    default: {
      return 0;
    } break;
    }
  }

  lastTime = fstReaderGetEndTime(reader);
  printf("LastTime: %lu\n",lastTime);

  FST_State state = {};
  state.varInfoMap = handleToVarInfo;
  int res = fstReaderIterBlocks(reader,Callback,&state,NULL);

  // Need to update with the final values =======================================

  for(FST_VarInfo& info : handleToVarInfo){
    uint64_t timeDiff = lastTime - info.lastTime;
    for(int i = 0; i < info.numberOfBits; i++){
      FST_SingleBitInfo& bit = info.singleBits[i];
      char lastBit = info.lastBitPatternBuffer[i];
    
      switch(lastBit){
      case '0':{
        bit.timeSpent0 += timeDiff;
      } break;
      case '1':{
        bit.timeSpent1 += timeDiff;
      } break;
      case 'Z':
      case 'z':{
        bit.timeSpentZ += timeDiff;
      } break;
      case 'X':
      case 'x':{
        bit.timeSpentX += timeDiff;
      } break;
      default: Assert(false);
      }
    }
  }

  bool ignoreMultibit = false;
  int levelToIgnore = 2;

  auto b = StartString(temp);

  b->PushString(R"FOO(
(SAIFILE
(SAIFVERSION "2.0")
(DIRECTION "backward")
(DESIGN "test.fst")
(DATE "2026-06-15 16:47:01.774642642 +01:00:00")
(VENDOR "surfer-project.org")
(PROGRAM_NAME "fst2saif")
(VERSION "0.1.0")
(DIVIDER / )
(TIMESCALE 1 s)
)FOO");
  
  b->PushString("(DURATION %lu)\n",lastTime);

  auto Recurse = [b,handleToVarInfo,ignoreMultibit,levelToIgnore](auto Recurse,FST_Node* top,int level) -> void{
    if(level >= 0){
      b->PushSpaces(level * 4);
      b->PushString("(INSTANCE %.*s\n",UN(top->name));
    }
    // Iterate subinstances first =================================================
    for(FST_Node* ptr = top->child; ptr; ptr = ptr->next){
      if(ptr->type == FST_NodeType_SCOPE){
        Recurse(Recurse,ptr,level + 1);
      }
    }

    // Iterate the remaining nets.
    if(level >= 0){
      b->PushSpaces((level + 1) * 4);
      b->PushString("(NET\n");

      for(FST_Node* ptr = top->child; ptr; ptr = ptr->next){
        if(ptr->type == FST_NodeType_VAR){
          u32 handle = ptr->handle;
          FST_VarInfo info = handleToVarInfo[handle];
          int bitSize = info.numberOfBits;

          if(info.type != FST_VarType_REG && info.type != FST_VarType_WIRE){
            continue;
          }

          if(ignoreMultibit && bitSize != 1){
            continue;
          }

          for(int i = 0; i < bitSize; i++){
            if(bitSize == 1){
              b->PushString("(%.*s",UN(ptr->name));
            } else {
              b->PushString("(%.*s \\[%d\\]",UN(ptr->name),i);
            }

            u64 trueToggleCount = info.singleBits[i].toggleCount - 1;
            if(info.singleBits[i].toggleCount == 0){
              trueToggleCount = 0;
            }
          
            b->PushString(" (T0 %lu)",info.singleBits[i].timeSpent0);
            b->PushString(" (T1 %lu)",info.singleBits[i].timeSpent1);
            b->PushString(" (TX %lu)",info.singleBits[i].timeSpentX);
            b->PushString(" (TZ %lu)",info.singleBits[i].timeSpentZ);
            b->PushString(" (TC %lu)",trueToggleCount);
            b->PushString(" (IG 0)");

            b->PushString(")\n");
          }
        }
      }
      b->PushSpaces((level + 1) * 4);
      b->PushString(")\n");

      b->PushSpaces(level * 4);
      b->PushString(")\n");
    }
  };

  Recurse(Recurse,top,-levelToIgnore);
  
  b->PushString(")");

  String content = EndString(temp,b);

  printf("%.*s\n",UN(content));
  
  FILE* f = fopen("test.saif","w");
  fwrite(content.data,content.size,sizeof(char),f);

  fstReaderClose(reader);

  return 0;
}

