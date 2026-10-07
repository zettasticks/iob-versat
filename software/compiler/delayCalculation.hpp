#pragma once

#include "compiler.hpp"

struct DELAY_Info{
  int value;
  bool isAny;
};

struct DELAY_Result{
};

struct DELAY_BufferToAdd{
  COM_Edge* edge;
  String bufferName;
  String bufferParameters;
  int bufferAmount;
};

DELAY_Result CalculateDelay(COM_Unit* top,COM_Edge* edges,Arena* out);
