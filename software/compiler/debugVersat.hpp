#pragma once

#include "debug.hpp"
#include "utils.hpp"

// ============================================================================
// Graph printing

// When adding new colors, also add info in the .cpp file
// TODO: We probably want to move this to meta data
enum Color{
  Color_DEFAULT,
  Color_BLACK,
  Color_BLUE,
  Color_RED,
  Color_GREEN,
  Color_YELLOW,

  Color_DEFAULT_NODE = Color_BLUE,
  Color_DEFAULT_EDGE = Color_BLACK
};

struct DOT_Info{
  // For edges, name of the node. For nodes, empty
  String firstNode;
  String secondNode;

  String name;
  String headContent;
  String middleContent;
  String tailContent;
  Color color;
};

struct DOT_Graph{
  String graphLabel;
  Array<DOT_Info> content;
};

bool DOT_IsNode(DOT_Info* info);
bool DOT_IsEdge(DOT_Info* info);

String GenerateDotGraph(DOT_Graph content,Arena* out);

void OutputContentToFile(String filepath,String content);

// ============================================================================
// Obtain debug path to output files to

// folderName can be empty string, file is created inside debug folder directly. 
String PushDebugPath(Arena* out,String folderName,String fileName);

// ============================================================================
// Debug path - Global way of defining a path tree for debug info

struct DEBUG_Marker{
  int count;
  DEBUG_Marker(String name,String entity,String stage);
  ~DEBUG_Marker();
};

String DEBUG_GetDebugRegionFilepath(String filename,Arena* out);

#define __debugmarker(LINE) debugmarker_ ## LINE
#define _debugmarker(LINE) __debugmarker( LINE )

// Debug folders are ordered in the form TOP/STAGE/FUNCTION
// Calling TOP again starts a new chain, meaning that it is not a problem if functions recurse.

#define DEBUG_TOP(ENTITY)  DEBUG_Marker _debugmarker(__LINE__){{},ENTITY,{}}
#define DEBUG_STAGE(STAGE) DEBUG_Marker _debugmarker(__LINE__){{},{},STAGE}
#define DEBUG_FUNCTION()    DEBUG_Marker _debugmarker(__LINE__){__FUNCTION__,{},{}}
