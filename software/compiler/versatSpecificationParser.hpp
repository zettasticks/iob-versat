#pragma once

#include "userConfigs.hpp"
#include "parser.hpp"
#include "versatSpecificationParser_meta.hpp"

// Parser stuff ===============================================================
// TODO: Remove all the list nodes, we can just have them be free and the "compiler" will just switch on the base type instead, no need to group stuff like we are currently doing.

struct SP_Node{
  union {
    SP_Node* next;
    //SP_Node* first;
  };

  union {
    SP_Node* childs;
    //SP_Node* second;
  };

  SP_Type type;
  Token token;
};
extern SP_Node SP_Node_Nil;

struct SP_NodeNode{
  SP_NodeNode* next;
  SP_Node* node;
};

struct SP_NodeList{
  SP_NodeNode* head;
  SP_NodeNode* tail;
};

// ======================================
// Type

bool SP_Type_IsLoop(SP_Type in);
bool SP_Type_IsOperation(SP_Type in);

// ======================================
// Parsing Helpers

SP_Node* SP_PushNode(Arena* out,SP_Type type,Token token,SP_Node* childs);
#define  SP_Append(HEAD,TAIL,NODE) LL_Append(HEAD,TAIL,next,NODE)

// ======================================
// Print

String SP_Repr(SP_Node* top,Arena* out);

// ======================================
// Parsing helpers

SP_Node* SP_ParseExpression(Parser* parser,Arena* out,int bindingPower = 99);
SP_Node* SP_ParseRange(Parser* parser,Arena* out);
SP_Node* SP_ParseVar(Parser* parser,Arena* out);
SP_Node* SP_ParseVarDeclaration(Parser* parser,Arena* out);
SP_Node* SP_ParseModuleInputDeclaration(Parser* parser,Arena* out);
SP_Node* SP_ParseInstanceDeclaration(Parser* parser,Arena* out);
SP_Node* SP_ParseVarGroup(Parser* parser,Arena* out);
SP_Node* SP_ParseConnection(Parser* parser,Arena* out);
SP_Node* SP_ParseConfigStatements(Parser* parser,Arena* out);
SP_Node* SP_ParseConfigFunction(Parser* parser,Arena* out);
SP_Node* SP_ParseModuleDef(Parser* parser,Arena* out);

// ======================================
// Parsing

SP_Node* SP_ParseSpec(String content,Arena* out);

// ======================================
// Helpers

SP_NodeList  SP_Flatten(SP_Node* top,Arena* out);
SP_Node*     SP_GetRangeExpression(SP_Node* accessRangeType);
