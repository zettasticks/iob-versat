#include "debugVersat.hpp"

#include "globals.hpp"
#include "filesystem.hpp"

// Make sure that it matches the value of the corresponding enum
static String graphPrintingColorTable[] = {
  "dark",
  "dark",
  "darkblue",
  "darkred",
  "darkgreen",
  "darkyellow"
};

bool DOT_IsNode(DOT_Info* info){
  bool res = Empty(info->firstNode);
  if(res){
    Assert(Empty(info->secondNode) && "If node then both first and second must be empty");
  }
  return res;
}

bool DOT_IsEdge(DOT_Info* info){
  bool res = (!Empty(info->firstNode));
  if(res){
    Assert(!Empty(info->secondNode) && "If edge then both first and second must contain name of nodes");
  }
  return res;
}

String GenerateDotGraph(DOT_Graph graph,Arena* out){
  TEMP_REGION(temp,out);
  auto result = StartString(temp);

  result->PushString("digraph view {\n\tnode [fontcolor=white,style=filled,color=\"160,60,176\"];\n");

  if(graph.graphLabel.size > 0){
    result->PushString("label=\"%.*s\";\n",UN(graph.graphLabel));
  }
  
  for(DOT_Info& node : graph.content){
    if(!DOT_IsNode(&node)){
      continue;
    }

    Color c = node.color;
    if(c == Color_DEFAULT){
      c = Color_DEFAULT_NODE;
    }
    String color = graphPrintingColorTable[(int) c];

    String name = node.name;
    String label = node.middleContent;
    result->PushString("\t\"%.*s\" [color=%.*s label=\"%.*s\"];\n",UN(name),UN(color),UN(label));
  }

  for(DOT_Info& node : graph.content){
    if(!DOT_IsEdge(&node)){
      continue;
    }

    result->PushString("\t\"%.*s\" -> ",UN(node.firstNode));
    result->PushString("\"%.*s\"",UN(node.secondNode));

    Color c = node.color;
    if(c == Color_DEFAULT){
      c = Color_DEFAULT_EDGE;
    }
    String color = graphPrintingColorTable[(int) c];

    result->PushString("[color=%.*s",UN(color));
    if(!Empty(node.headContent)){
      result->PushString(" headlabel=\"%.*s\"",UN(node.headContent));
    }
    if(!Empty(node.middleContent)){
      result->PushString(" label=\"%.*s\"",UN(node.middleContent));
    }
    if(!Empty(node.tailContent)){
      result->PushString(" taillabel=\"%.*s\"",UN(node.tailContent));
    }
    result->PushString("];\n");
  }

  result->PushString("}\n");

  return EndString(out,result);
}

void OutputContentToFile(String filepath,String content){
  FILE* file = OpenFileAndCreateDirectories(filepath,"w",FilePurpose_DEBUG_INFO);
  DEFER_CLOSE_FILE(file);
  
  if(!file){
    printf("[OutputContentToFile] Error opening file: %.*s\n",UN(filepath));
    return;
  }

  int res = fwrite(content.data,sizeof(char),content.size,file);
  Assert(res == content.size);
}

String PushDebugPath(Arena* out,String folderName,String fileName){
  Assert(globalOptions.debug);
  Assert(!Contains(fileName,"/"));
  Assert(fileName.size != 0);
  
  const char* fullFolderPath = nullptr;
  if(folderName.size == 0){
    fullFolderPath = StaticFormat("%.*s",UN(globalOptions.debugPath));
  } else {
    fullFolderPath = StaticFormat("%.*s/%.*s",UN(globalOptions.debugPath),UN(folderName));
  }

  CreateDirectories(fullFolderPath);
  String path = PushString(out,"%s/%.*s",fullFolderPath,UN(fileName));
  
  return path;
}

// ============================================================================
// Debug path regions

enum DEBUG_Type{
  DEBUG_Type_NIL,
  DEBUG_Type_FUNCTION_NAME,
  DEBUG_Type_STAGE,
  DEBUG_Type_ENTITY
};

struct DEBUG_Node{
  DEBUG_Node* next;
  DEBUG_Node* prev;

  String name;
  DEBUG_Type type;
};

static DEBUG_Node* debugHead = 0;
static DEBUG_Node* debugTail = 0;
static DEBUG_Node* debugFreeList = 0;

DEBUG_Marker::DEBUG_Marker(String functionName,String entity,String stage){
  String work[] = {functionName,stage,entity};

  int processed = 0;
  for(int i = 0; i < 3; i++){
    DEBUG_Type type = (DEBUG_Type) (i + ((int) DEBUG_Type_FUNCTION_NAME));
    String name = work[i];

    if(Empty(name)){
      continue;
    }

    processed += 1;
    
    DEBUG_Node* node = 0;
    if(debugFreeList){
      node = debugFreeList;
      LL_PopFrontNoTail(debugFreeList,next);
      *node = {};
    } else {
      node = PushStruct<DEBUG_Node>(debugArena);
    }

    node->name = PushString(debugArena,name);
    node->type = type;
    DLL_Append(debugHead,debugTail,next,prev,node);
  }
  this->count = processed;
}

DEBUG_Marker::~DEBUG_Marker(){
  for(int i = 0; i < count; i++){
    DEBUG_Node* node = debugTail;
    DLL_Remove(debugHead,debugTail,next,prev,node);
    LL_PushNoTail(debugFreeList,next,node);
  }
}

String DEBUG_GetDebugRegionFilepath(String filename,Arena* out){
  TEMP_REGION(temp,out);

  DEBUG_Node* lastEntity = nullptr;
  for(DEBUG_Node* ptr = debugHead; ptr; ptr = ptr->next){
    if(ptr->type == DEBUG_Type_ENTITY){
      lastEntity = ptr;
    }
  }

  DEBUG_Node* lastStage = nullptr;
  for(DEBUG_Node* ptr = lastEntity; ptr; ptr = ptr->next){
    if(ptr->type == DEBUG_Type_STAGE){
      lastStage = ptr;
    }
  }

  String res = {};
  if(!lastEntity){
    res = PushString(out,"%.*s/%s/%.*s",UN(globalOptions.debugPath),"TOP",UN(filename));
  } else {
    auto* builder = StartString(temp);
  
    builder->PushString(globalOptions.debugPath);

    if(lastEntity){
      builder->PushString("/");
      builder->PushString(lastEntity->name);
    }

    DEBUG_Node* ptr = lastStage;
    if(!ptr){
      ptr = lastEntity->next;
    }

    for(; ptr; ptr = ptr->next){
      builder->PushString("/");
      builder->PushString(ptr->name);
    }

    builder->PushString("/");
    builder->PushString(filename);

    res = EndString(out,builder);
  }

  return res;
}
