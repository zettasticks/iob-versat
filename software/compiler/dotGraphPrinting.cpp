#include "dotGraphPrinting.hpp"

#include "accelerator.hpp"
#include "memory.hpp"
#include "utils.hpp"
#include "utilsCore.hpp"
#include "versat.hpp"

// Make sure that it matches the value of the corresponding enum
static String graphPrintingColorTable[] = {
  STRING("dark"),
  STRING("darkblue"),
  STRING("darkred"),
  STRING("darkgreen"),
  STRING("darkyellow")
};

Color DefaultNodeColor(FUInstance* inst){
  Color color = Color_BLUE;
    
  if(inst->type == NodeType_SOURCE || inst->type == NodeType_SOURCE_AND_SINK){
    color = Color_GREEN;
  } else if(inst->type == NodeType_SINK){
    color = Color_BLACK;
  }

  return color;
}

GraphInfo DefaultNodeContent(FUInstance* inst,Arena* out){
  String str = PushString(out,"%.*s_%d",UNPACK_SS(inst->name),inst->id);
  return {str,DefaultNodeColor(inst)};
}

GraphInfo DefaultEdgeContent(Edge* edge,Arena* out){
  int inPort = edge->in.port;
  int outPort = edge->out.port;

  String content = PushString(out,"%d -> %d",outPort,inPort);

  return {content,Color_BLACK};
}

NodeContent defaultNodeContent = DefaultNodeContent;
EdgeContent defaultEdgeContent = DefaultEdgeContent;

String UniqueInstanceName(FUInstance* inst,Arena* out){
 String name = PushString(out,"%p",inst);
  
  return name;
}

GraphPrintingContent GeneratePrintingContent(Accelerator* accel,NodeContent nodeFunction,EdgeContent edgeFunction,Arena* out,Arena* temp){
  BLOCK_REGION(temp);

  Array<Edge> edges = GetAllEdges(accel,temp);

  ArenaList<GraphPrintingNodeInfo>* nodeInfo = PushArenaList<GraphPrintingNodeInfo>(temp);
  FOREACH_LIST(FUInstance*,inst,accel->allocated){
    GraphInfo content = nodeFunction(inst,out);
    String name = UniqueInstanceName(inst,out);

    *nodeInfo->PushElem() = {.name = name,.content = content.content,.color = content.color};
  }
  Array<GraphPrintingNodeInfo> nodes = PushArrayFromList(out,nodeInfo);

  ArenaList<GraphPrintingEdgeInfo>* edgeInfo = PushArenaList<GraphPrintingEdgeInfo>(temp);
  for(Edge edge : edges){
    GraphInfo content = edgeFunction(&edge,out);
    String first = UniqueInstanceName(edge.out.inst,out);
    String second = UniqueInstanceName(edge.in.inst,out); 

    *edgeInfo->PushElem() = {
      .content = content.content,
      .color = content.color,
      .first = first,
      .second = second
    };
  }
  Array<GraphPrintingEdgeInfo> edgeResult = PushArrayFromList(out,edgeInfo);
  
  GraphPrintingContent content = {};
  content.nodes = nodes;
  content.edges = edgeResult;

  return content;
}

GraphPrintingContent GenerateDefaultPrintingContent(Accelerator* accel,Arena* out,Arena* temp){
  GraphPrintingContent content = GeneratePrintingContent(accel,defaultNodeContent,defaultEdgeContent,out,temp);

  content.graphLabel = PushString(out,"%d",accel->id);
  
  return content;
}

String GenerateDotGraph(Accelerator* accel,GraphPrintingContent content,Arena* out,Arena* temp){
  auto result = StartString(out);

  PushString(out,"digraph view {\n\tnode [fontcolor=white,style=filled,color=\"160,60,176\"];\n");

  if(content.graphLabel.size > 0){
    PushString(out,"label=\"%.*s\";\n",UNPACK_SS(content.graphLabel));
  }
  
  for(GraphPrintingNodeInfo& node : content.nodes){
    String color = graphPrintingColorTable[(int) node.color];
    String name = node.name;
    String label = node.content;
    PushString(out,"\t\"%.*s\" [color=%.*s label=\"%.*s\"];\n",UNPACK_SS(name),UNPACK_SS(color),UNPACK_SS(label));
  }

  for(GraphPrintingEdgeInfo info : content.edges){
    PushString(out,"\t\"%.*s\" -> ",UNPACK_SS(info.first));
    PushString(out,"\"%.*s\"",UNPACK_SS(info.second));

    String color = graphPrintingColorTable[(int) info.color];
    String label = info.content;
      
    PushString(out,"[color=%.*s label=\"%.*s\"];\n",UNPACK_SS(color),UNPACK_SS(label));
  }

  PushString(out,"}\n");
    
  return EndString(result);
}

void OutputDebugDotGraph(Accelerator* accel,String fileName,Arena* temp){
  Arena* temp2 = debugArena;

  BLOCK_REGION(temp);
  BLOCK_REGION(temp2);

  String trueFileName = PushString(temp,fileName); // Make it safe to use StaticFormat outside this function
  
  String filePath = PushDebugPath(temp,accel->name,trueFileName);
      
  GraphPrintingContent content = GenerateDefaultPrintingContent(accel,temp,temp2);
  String result = GenerateDotGraph(accel,content,temp,debugArena);
  OutputContentToFile(filePath,result);
}

void OutputDebugDotGraph(Accelerator* accel,String fileName,FUInstance* highlight,Arena* temp){
  Arena* temp2 = debugArena;

  BLOCK_REGION(temp);
  BLOCK_REGION(temp2);

  String trueFileName = PushString(temp,fileName); // Make it safe to use StaticFormat outside this function
  
  String filePath = PushDebugPath(temp,accel->name,trueFileName);

  auto nodeFunction = [highlight](FUInstance* inst,Arena* out) -> GraphInfo {
    GraphInfo def = DefaultNodeContent(inst,out);
    if(inst == highlight){
      Assert(def.color != Color_GREEN);
      return {def.content,Color_GREEN};
    } else {
      return def;
    }
  };
  
  GraphPrintingContent content = GeneratePrintingContent(accel,nodeFunction,defaultEdgeContent,temp,temp2);
  String result = GenerateDotGraph(accel,content,temp,debugArena);
  OutputContentToFile(filePath,result);
}

void OutputDebugDotGraph(Accelerator* accel,String fileName,Set<FUInstance*>* highlight,Arena* temp){
  Arena* temp2 = debugArena;

  BLOCK_REGION(temp);
  BLOCK_REGION(temp2);

  String trueFileName = PushString(temp,fileName); // Make it safe to use StaticFormat outside this function
  
  String filePath = PushDebugPath(temp,accel->name,trueFileName);

  auto nodeFunction = [highlight](FUInstance* inst,Arena* out) -> GraphInfo {
    GraphInfo def = DefaultNodeContent(inst,out);
    if(highlight->Exists(inst)){
      Assert(def.color != Color_RED);
      return {def.content,Color_RED};
    } else {
      return def;
    }
  };
  
  GraphPrintingContent content = GeneratePrintingContent(accel,nodeFunction,defaultEdgeContent,temp,temp2);
  String result = GenerateDotGraph(accel,content,temp,debugArena);
  OutputContentToFile(filePath,result);
}





