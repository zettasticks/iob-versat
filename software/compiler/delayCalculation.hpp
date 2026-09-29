#pragma once

#include "compiler.hpp"

struct DELAY_Info{
  int value;
  bool isAny;
};

//typedef Hashmap<COM_Edge*,DELAY_Info> EdgeDelay;
//typedef Hashmap<COM_Unit*,DELAY_Info> NodeDelay;
//typedef Hashmap<COM_Port,DELAY_Info> PortDelay;

struct DELAY_Result{
  //EdgeDelay* edgesDelay;
  //PortDelay* portDelay;
  //NodeDelay* nodeDelay;

  //TrieMap<FUInstance*,int>* variableBuffer;
};

struct DELAY_BufferToAdd{
  COM_Edge* edge;
  String bufferName;
  String bufferParameters;
  int bufferAmount;
};

DELAY_Result             CalculateDelay(COM_Unit* top,COM_Edge* edges,Arena* out);
//Array<DELAY_BufferToAdd> GenerateFixDelays(COM_Unit* top,EdgeDelay* edgeDelays,Arena* out);
