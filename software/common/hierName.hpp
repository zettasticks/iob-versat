#pragma once

#include "utils.hpp"

void HIER_Init();

struct HIER_Node{
  String name;

  HIER_Node* parent;
  HIER_Node* nextChain;
};

struct HIER_Name{
  HIER_Node* node;
};

static HIER_Name HIER_Nil = {};

void HIER_Init();

String HIER_GetFullName(HIER_Name name,String separator,Arena* out);

HIER_Name HIER_BaseName(String name);

HIER_Name operator+(HIER_Name parent,HIER_Name bottom);
