#include "type.hpp"
#define SIMPLE_TYPES

#include <cstdio>

#include "memory.hpp"
#include "parser.hpp"
#include "utils.hpp"
#include "utilsCore.hpp"

#include "templateEngine.hpp"

String currentTemplateBeingProcessed;

struct MyFrame{
  Hashmap<String,Type*>* table;
  MyFrame* previousFrame;
};

static MyFrame* CreateFrame(MyFrame* previous,Arena* out){
  MyFrame* frame = PushStruct<MyFrame>(out);
  frame->table = PushHashmap<String,Type*>(out,256); // Testing a fixed hashmap for now.
  frame->previousFrame = previous;
  return frame;
}

static void SetValue(MyFrame* frame,String id,Type* t){
  frame->table->Insert(id,t);
}

static Opt<Type*> GetValue(MyFrame* frame,String var){
  MyFrame* ptr = frame;

  while(ptr){
    Type** possible = ptr->table->Get(var);
    if(possible){
      return *possible;
    } else {
      ptr = ptr->previousFrame;
    }
  }
  return {};
}

struct Writer{
  FILE* file;
  int spacing;
  bool lineIsEmpty;
  bool disabled;
  
  void StartStatement(){
    if(disabled){
      return;
    }
    
    if(lineIsEmpty){
      for(int i = 0; i < spacing; i++){
        fprintf(file," ");
      }
    }
    lineIsEmpty = false;

    fprintf(file,"out << ");
  }

  void EndStatement(){
    if(disabled){
      return;
    }

    fprintf(file,";\n");
    lineIsEmpty = true;
  }
  
  void WriteStatement(const char* format,...) __attribute__ ((format (printf, 2, 3))){
    if(disabled){
      return;
    }

    va_list args;
    va_start(args,format);

    StartStatement();
    
    fprintf(file,"\"");
    vWrite(format,args);
    fprintf(file,"\"");

    EndStatement();

    va_end(args);
  }

  void vWrite(const char* format,va_list args){
    if(disabled){
      return;
    }
    vfprintf(file,format,args);
  }
  
  void Write(const char* format,...) __attribute__ ((format (printf, 2, 3))){
    if(disabled){
      return;
    }
    va_list args;
    va_start(args,format);

    if(lineIsEmpty){
      for(int i = 0; i < spacing; i++){
        fprintf(file," ");
      }
    }

    vWrite(format,args);

    for(const char* ptr = format; *ptr; ptr+= 1){
      if(*ptr == '\n'){
        lineIsEmpty = true;
      } else {
        lineIsEmpty = false;
      }
    }
    
    va_end(args);
  }

  void Newline(){
    if(disabled){
      return;
    }
    fprintf(file,"\n");
    lineIsEmpty = true;
  }
  
  void Indent(){
    spacing += 2;
  }

  void Dedent(){
    spacing -= 2;
  }
  
  void PushBlock(){
    if(disabled){
      return;
    }
    for(int i = 0; i < spacing; i++){
      fprintf(file," ");
    }
    fprintf(file,"{\n");
    lineIsEmpty = true;
    Indent();
  }
  
  void PopBlock(){
    if(disabled){
      return;
    }
    Dedent();

    for(int i = 0; i < spacing; i++){
      fprintf(file," ");
    }
    fprintf(file,"}\n");
    lineIsEmpty = true;
  }
};

char* templateNameToContent[] = {};

// Every function that created a frame in the template engine, we now just add a '{' '}' block instead

// SET,IF
static void MyEval(Writer* writer,MyFrame* frame,Block* block);
static Type* MyEvalExpression(Writer* writer,MyFrame* frame,Expression* expr);
static Type* GetTypeOfExpression(Writer* writer,MyFrame* frame,Expression* expr);

static void MyEvalBlockCommand(Writer* writer,MyFrame* frame,Block* block){
  TEMP_REGION(temp,nullptr);

  Command* com = block->command;

  switch(com->definition->type){
  case CommandType_JOIN:{
    writer->PushBlock();
    writer->Write("std::stringstream myOut;\n");
    writer->PushBlock();
    writer->Write("std::ostream& out = myOut;\n");

    
    
    writer->PopBlock();
    writer->Write("out << myOut;\n");
    writer->PopBlock();
    //writer->Write("<JOINING>\n");
  } break;
  case CommandType_FOR:{
    String id = com->expressions[0]->id;
    writer->PushBlock();
    writer->Write("int index = 0;\n");

    Type* expressionType = GetTypeOfExpression(writer,frame,com->expressions[1]);

    if(expressionType->type == Subtype_BASE){
      writer->Write("for(%.*s %.*s = 0; %.*s <  ",UNPACK_SS(expressionType->name),UNPACK_SS(id),UNPACK_SS(id));

      MyEvalExpression(writer,frame,com->expressions[1]);

      writer->Write("; %.*s++){\n",UNPACK_SS(id));

      MyFrame* newFrame = CreateFrame(frame,temp);

      Type* iteratorType = expressionType;
    
      SetValue(newFrame,id,iteratorType);
    
      writer->Indent();
      for(Block* ptr : block->innerBlocks){
        MyEval(writer,newFrame,ptr);
      }
    
      writer->Write("index += 1;\n");
      writer->Dedent();
      writer->Write("}\n");
      writer->PopBlock();
    } else {
      writer->Write("for(auto %.*s : ",UNPACK_SS(id));

      MyEvalExpression(writer,frame,com->expressions[1]);

      writer->Write("){\n");

      MyFrame* newFrame = CreateFrame(frame,temp);

      Type* iteratorType = GetIteratingTypeOfContainer(expressionType);
    
      SetValue(newFrame,id,iteratorType);
    
      writer->Indent();
      for(Block* ptr : block->innerBlocks){
        MyEval(writer,newFrame,ptr);
      }
    
      writer->Write("index += 1;\n");
      writer->Dedent();
      writer->Write("}\n");
      writer->PopBlock();
    }
  } break;

#if 0
  case CommandType_TEMPLATE_ARGUMENT:{
    *templateArgumentsAndType->PushElem() = {com->expressions[0]->id,com->expressions[1]->id};
  } break;
#endif
    
  case CommandType_IF:{
    writer->Write("if(%.*s){\n",UNPACK_SS(com->expressions[0]->text));
    writer->Indent();
    
    for(Block* ptr : block->innerBlocks){
      MyEval(writer,frame,ptr);
      
      if(ptr->type == BlockType_COMMAND && ptr->command->definition->type == CommandType_ELSE){
        writer->Dedent();
        writer->Write("} else {\n");
        writer->Indent();
      }
    }
    writer->PopBlock();
  } break;
  }
}

static void PrintExpression(Writer* writer,Expression* expr){
  writer->Write("%.*s",UNPACK_SS(expr->text));
}

static Type* MyEvalExpression(Writer* writer,MyFrame* frame,Expression* expr);

static void MyEvalNonBlockCommand(Writer* writer,MyFrame* frame,Command* com){
  switch(com->definition->type){
  case CommandType_SET:{
    String id = com->expressions[0]->id;
    
    writer->Write("auto %.*s",UNPACK_SS(id));
    writer->Write("=");
    Type* type = MyEvalExpression(writer,frame,com->expressions[1]);
    writer->Write(";\n");
    //DEBUG_BREAK_IF(id == STRING("numberIOs"));
    SetValue(frame,id,type);
  } break;
  case CommandType_LET:{
    writer->Write("auto %.*s",UNPACK_SS(com->expressions[0]->id));
    writer->Write("=");
    Type* type = MyEvalExpression(writer,frame,com->expressions[1]);
    writer->Write(";\n");
    SetValue(frame,com->expressions[0]->id,type);
  } break;
  case CommandType_INC:{
    writer->Write("%.*s++;\n",UNPACK_SS(com->expressions[0]->id));
  } break;
  case CommandType_CALL:{
    writer->Write("%.*s(",UNPACK_SS(com->expressions[0]->id));

    for(int i = 1; i < com->expressions.size; i++){
      MyEvalExpression(writer,frame,com->expressions[i]);
      if(i != com->expressions.size - 1){
        writer->Write(",");
      }
    }

    writer->Write(")");
  } break;
  case CommandType_FORMAT:{
    writer->Write("<HandleFormatHere>");
  } break;

  }
}

static Type* GetTypeOfExpression(Writer* writer,MyFrame* frame,Expression* expr){
  bool previous = writer->disabled;
  writer->disabled = true;
  
  Type* type = MyEvalExpression(writer,frame,expr);

  writer->disabled = previous;
  return type;
}

static Type* MyEvalExpression(Writer* writer,MyFrame* frame,Expression* expr){
  static Type* boolType = GetType(STRING("bool"));
  
  switch(expr->type){
  case Expression::UNDEFINED:{
    Assert(false);
  } break;
  case Expression::OPERATION:{
    if(expr->op[0] == '|'){ // Pipe operation
      String pipeName = expr->expressions[1]->id;
      
      writer->Write("%.*s(",UNPACK_SS(pipeName));

      MyEvalExpression(writer,frame,expr->expressions[0]);
      writer->Write(");\n");
    } else if(CompareString(expr->op,"!")){
      writer->Write("!");
      MyEvalExpression(writer,frame,expr->expressions[0]);
      return boolType;
    } else if(CompareString(expr->op,"and")){
      MyEvalExpression(writer,frame,expr->expressions[0]);
      writer->Write("&&");
      MyEvalExpression(writer,frame,expr->expressions[1]);
      return boolType;
    } else if(CompareString(expr->op,"or")){
      MyEvalExpression(writer,frame,expr->expressions[0]);
      writer->Write("||");
      MyEvalExpression(writer,frame,expr->expressions[1]);
      return boolType;
    } else if(CompareString(expr->op,"xor")){
      MyEvalExpression(writer,frame,expr->expressions[0]);
      writer->Write("!=");
      MyEvalExpression(writer,frame,expr->expressions[1]);
      return boolType;
    } else {
      Type* leftType = MyEvalExpression(writer,frame,expr->expressions[0]);
      writer->Write("%s",expr->op);
      Type* rightType = MyEvalExpression(writer,frame,expr->expressions[1]);
      return rightType; // TODO: Kinda on an hack
    }
  } break;
  case Expression::IDENTIFIER:{
    writer->Write("%.*s",UNPACK_SS(expr->id));

    Opt<Type*> possible = GetValue(frame,expr->id);
    if(!possible.has_value()){
      printf("Did not find '%.*s' in template '%.*s'\n",UNPACK_SS(expr->id),UNPACK_SS(currentTemplateBeingProcessed));
      exit(0);
    }
    
    return possible.value();
  } break;
  case Expression::COMMAND:{
    Command* com = expr->command;
    Assert(!com->definition->isBlockType);
    
    MyEvalNonBlockCommand(writer,frame,com);
  } break;
  case Expression::LITERAL:{
    TEMP_REGION(temp,nullptr);
    String repr = GetDefaultValueRepresentation(expr->val,temp);
    writer->Write("%.*s",UNPACK_SS(repr));
    return expr->val.type;
  } break;
  case Expression::ARRAY_ACCESS:{
    Type* sub = MyEvalExpression(writer,frame,expr->expressions[0]);
    writer->Write("[%ld]",expr->val.number);

    return GetIteratingTypeOfContainer(sub);
  } break;
  case Expression::MEMBER_ACCESS:{
    Type* sub = MyEvalExpression(writer,frame,expr->expressions[0]);
    if(sub->type == Subtype_POINTER){
      writer->Write("->%.*s",UNPACK_SS(expr->id));

      Opt<Member*> m = GetMember(sub->pointerType,expr->id);
      
      return m.value()->type;
    } else {
      writer->Write(".%.*s",UNPACK_SS(expr->id));
      //return sub;
      Opt<Member*> m = GetMember(sub,expr->id);
      
      return m.value()->type;
    }
  } break;
  }
  
  return nullptr;
}

String EscapeString(String in,Arena* out){
  TEMP_REGION(temp,out);

  auto builder = StartString(temp);

  for(int i = 0; i < in.size; i++){
    char ch = in[i];

    switch(ch){
    case '"': {
      builder->PushChar('\\');
      builder->PushChar('\"');
    } break;
    case '\n': {
      builder->PushChar('\\');
      builder->PushChar('n');
    } break;
    case '\\': {
      builder->PushChar('\\');
      builder->PushChar('\\');
    } break;
    default:{
      builder->PushChar(ch);
    } break;
    }
  }

  return EndString(out,builder);
}

void MyEval(Writer* writer,MyFrame* frame,Block* block){
  TEMP_REGION(temp,nullptr);

  switch(block->type){
  case BlockType_COMMAND:{
    if(block->command->definition->isBlockType){
      MyEvalBlockCommand(writer,frame,block);
    } else {
      MyEvalNonBlockCommand(writer,frame,block->command);
    }
  }break;
  case BlockType_EXPRESSION:{
    writer->StartStatement();
    
    MyEvalExpression(writer,frame,block->expression);

    writer->EndStatement();
  } break;
  case BlockType_TEXT:{
    String escaped = EscapeString(block->textBlock,temp);
    writer->WriteStatement("%.*s",UNPACK_SS(escaped));
  } break;
  }
}

void GenerateTemplatePrinter(Writer* writer,CompiledTemplate* compiledTemplate){
  TEMP_REGION(temp,nullptr);
  auto templateArgumentsAndType = PushArenaList<Pair<String,String>>(temp);

  for(Block* block : compiledTemplate->blocks){
    if(block->type == BlockType_COMMAND){
      Command* com = block->command;
      if(com->definition->type == CommandType_TEMPLATE_ARGUMENT){
        *templateArgumentsAndType->PushElem() = {com->expressions[0]->val.str,com->expressions[1]->val.str};
      }
    }
  }
  Array<Pair<String,String>> argumentsAndTypes = PushArrayFromList(temp,templateArgumentsAndType);

  MyFrame* initialFrame = CreateFrame(nullptr,temp);
  
  //identifierToType = PushTrieMap<String,Type*>(temp);
  SetValue(initialFrame,STRING("index"),GetType(STRING("int")));
  for(Pair<String,String> p : argumentsAndTypes){
    Type* t = GetType(p.first);
    SetValue(initialFrame,p.second,t);
  }
  
  writer->Write("static void Template_%.*s(std::ostream& out",UNPACK_SS(compiledTemplate->name));

  for(Pair<String,String> p : argumentsAndTypes){
    writer->Write(",%.*s& %.*s",UNPACK_SS(p.first),UNPACK_SS(p.second));
  }
  writer->Write("){\n");

  writer->Indent();
  for(Block* block : compiledTemplate->blocks){
    MyEval(writer,initialFrame,block);
  }
  writer->PopBlock();
  writer->Newline();
}

// 0 - exe name
// 1+ - pair of templates to compile
int main(int argc,const char* argv[]){
  if(argc < 2){
    fprintf(stderr,"Need at least 2 arguments: <exe> [list of templates] \n");
    return -1;
  }

  InitDebug();
  
  Arena inst1 = InitArena(Megabyte(64));
  contextArenas[0] = &inst1;
  Arena inst2 = InitArena(Megabyte(64));
  contextArenas[1] = &inst2;
  
  RegisterTypes();

  FILE* file = fopen("templateTest.cpp","w");
  DEFER_CLOSE_FILE(file);

  Writer writer = {};
  writer.file = file;

  for(int i = 1; i < argc; i++){
    TEMP_REGION(temp,nullptr);
    TEMP_REGION(temp2,temp);

    String templatePath = STRING(argv[i]);

    Array<String> pathAsTokens = Split(templatePath,'/',temp); // For now only split over one char. 

    String templateName = pathAsTokens[pathAsTokens.size - 1];
    Array<String> splittedName = Split(templateName,'.',temp); // For now only split over one char. 

    String nameOnly = splittedName[0];

    String content = PushFile(temp2,templatePath);

    currentTemplateBeingProcessed = nameOnly;
    CompiledTemplate* compiledTemplate = CompileTemplate(content,StaticFormat("%.*s",UNPACK_SS(nameOnly)),temp2);
    
    GenerateTemplatePrinter(&writer,compiledTemplate);
  }

  return 0;
}
