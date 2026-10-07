#include "debugVersat.hpp"

#include "globals.hpp"
#include "filesystem.hpp"

// Make sure that it matches the value of the corresponding enum
static String graphPrintingColorTable[] = {
  "dark",
  "darkblue",
  "darkred",
  "darkgreen",
  "darkyellow"
};

String GenerateDotGraph(GraphPrintingContent content,Arena* out){
  TEMP_REGION(temp,out);
  auto result = StartString(temp);

  result->PushString("digraph view {\n\tnode [fontcolor=white,style=filled,color=\"160,60,176\"];\n");

  if(content.graphLabel.size > 0){
    result->PushString("label=\"%.*s\";\n",UN(content.graphLabel));
  }
  
  for(GraphPrintingNodeInfo& node : content.nodes){
    String color = graphPrintingColorTable[(int) node.color];
    String name = node.name;
    String label = node.content;
    result->PushString("\t\"%.*s\" [color=%.*s label=\"%.*s\"];\n",UN(name),UN(color),UN(label));
  }

  for(GraphPrintingEdgeInfo info : content.edges){
    result->PushString("\t\"%.*s\" -> ",UN(info.firstNode));
    result->PushString("\"%.*s\"",UN(info.secondNode));

    String color = graphPrintingColorTable[(int) info.color];
    String label = info.content;
      
    result->PushString("[color=%.*s label=\"%.*s\"];\n",UN(color),UN(label));
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

static Array<String> debugRegionStack;
static int debugRegionIndex;
static Arena debugRegionArenaInst;
static Arena* debugRegionArena;
static bool debugRegionInit;

DebugPathMarker::DebugPathMarker(String name){
  if(!debugRegionInit){
    debugRegionArenaInst = InitArena(Megabyte(16));
    debugRegionArena = &debugRegionArenaInst;
    debugRegionStack = PushArray<String>(debugRegionArena,10);
    
    debugRegionInit = true;
  }
  
  // TODO: This will eventually exhaust the arena since we do not free anything.
  //       Easy to fix by just storing the arena mark.
  debugRegionStack[debugRegionIndex] = PushString(debugRegionArena,name);

  debugRegionIndex += 1;
}

DebugPathMarker::~DebugPathMarker(){
  debugRegionIndex -= 1;
}

String GetDebugRegionFilepath(String filename,Arena* out){
  TEMP_REGION(temp,out);
  
  auto* builder = StartString(temp);
  
  builder->PushString(globalOptions.debugPath);

  if(debugRegionIndex == 0){
    builder->PushString("/TOP");
  } else {
    for(int i = 0; i < debugRegionIndex; i++){
      String component = debugRegionStack[i];
      builder->PushString("/");
      builder->PushString(component);
    }
  }
    
  builder->PushString("/");
  builder->PushString(filename);

  return EndString(out,builder);
}
