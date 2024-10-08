#pragma once

#include "utils.hpp"

struct Options{
  Array<String> verilogFiles;
  Array<String> extraSources;
  Array<String> includePaths;
  Array<String> unitPaths;
  
  String hardwareOutputFilepath;
  String softwareOutputFilepath;
  String verilatorRoot;
  String debugPath;
  
  String specificationFilepath;
  String topName;
  int databusAddrSize; // AXI_ADDR_W - used to be bitSize
  int databusDataSize; // AXI_DATA_W

  bool addInputAndOutputsToTop;
  bool debug;
  bool shadowRegister;
  bool architectureHasDatabus;
  bool useFixedBuffers;
  bool generateFSTFormat;
  bool disableDelayPropagation;
  bool useDMA;
  bool exportInternalMemories;
};

struct DebugState{
  uint dotFormat;
  bool outputGraphs;
  bool outputConsolidationGraphs;
  bool outputAccelerator;
  bool outputVersat;
  bool outputVCD;
  bool outputAcceleratorInfo;
  bool useFixedBuffers;
};

extern Options globalOptions;
extern DebugState globalDebug;
extern Arena* globalPermanent;
