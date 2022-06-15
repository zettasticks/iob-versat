#include <cstdio>

#include "parser.hpp"
#include "utils.hpp"

struct SimpleMemberInfo{
   char baseTypeName[512];
   char arrayDefinition[512];
   bool hasArray;
   int pointers;
   char name[512];
};

struct SimpleTypeInfo{
   SimpleMemberInfo* members;
   int nMembers;
   char name[512];
};

static SimpleTypeInfo typeInfoBuffer[1024*1024];
static int typeInfoIndex;
static SimpleMemberInfo memberBuffer[1024*1024];
static int memberIndex;

static void SkipQualifiers(Tokenizer* tok){
   while(1){
      Token peek = tok->PeekToken();

      if(CompareToken(peek,"const")){
         tok->AdvancePeek(peek);
         continue;
      }
      if(CompareToken(peek,"volatile")){
         tok->AdvancePeek(peek);
         continue;
      }

      break;
   }
}

static SimpleTypeInfo* NameToType(char* name){
   for(int i = 0; i < typeInfoIndex; i++){
      if(CompareString(typeInfoBuffer[i].name,name)){
         return &typeInfoBuffer[i];
      }
   }

   SimpleTypeInfo type = {};

   strcpy(type.name,name);

   SimpleTypeInfo* res = &typeInfoBuffer[typeInfoIndex];
   typeInfoBuffer[typeInfoIndex++] = type;

   return res;
}

static SimpleTypeInfo* AddType(SimpleTypeInfo info){
   SimpleTypeInfo* type = NameToType(info.name);

   *type = info;

   return type;
}

static int ParseEvalExpression(Tokenizer* tok){
   Token token = tok->NextToken(); // Only simple numbers for now

   int val = 0;
   for(int i = 0; i < token.size; i++){
      val *= 10;

      Assert(token.str[i] >= '0' && token.str[i] <= '9');
      val += token.str[i] - '0';
   }

   return val;
}

static SimpleMemberInfo ParseMember(Tokenizer* tok){
   SimpleMemberInfo member = {};
   SkipQualifiers(tok);

   Token token = tok->PeekToken();

   char typeStr[512];
   if(CompareToken(token,"enum")){
      tok->AdvancePeek(token);

      Token peek = tok->PeekToken();
      if(!CompareToken(peek,"{")){
         Token enumName = tok->NextToken();
         StoreToken(enumName,member.baseTypeName);
      }

      peek = tok->PeekToken();
      if(CompareToken(peek,"{")){
         tok->NextFindUntil("}");
         tok->AssertNextToken("}");
      }
      strcpy(member.baseTypeName,"int"); // TODO: Set enums to int, easier but hackish
   } else {
      token = tok->NextToken();

      Token peek = tok->PeekToken();
      if(CompareToken(peek,"<")){ // Template
         tok->NextFindUntil(">");
         Token end = tok->AssertNextToken(">");

         token = ExtendToken(token,end);
      }

      StoreToken(token,member.baseTypeName);
   }

   token = tok->PeekToken();
   while(CompareToken(token,"*")){
      tok->AdvancePeek(token);
      member.pointers += 1;
      token = tok->PeekToken();
   }

   Token name = tok->NextToken();
   StoreToken(name,member.name);

   // Skip array declaration, for now?
   Token peek = tok->PeekToken();
   if(CompareToken(peek,"[")){
      tok->AdvancePeek(peek);

      Token peek = tok->NextFindUntil("]");

      StoreToken(peek,member.arrayDefinition);
      member.hasArray = true;

      tok->AssertNextToken("]");
   }

   tok->AssertNextToken(";");

   return member;
}

static SimpleTypeInfo* ParseStruct(Tokenizer* tok,int insideStruct){
   static int unnamedStructIndex = 0;
   SimpleTypeInfo structInfo = {};

   Token token = tok->NextToken();
   Assert(CompareToken(token,"struct"));

   token = tok->PeekToken();

   if(CompareToken(token,"{")){ // Unnamed
      sprintf(structInfo.name,"unnamed_type_%d",unnamedStructIndex++);
   } else { // Named struct
      token = tok->NextToken();

      StoreToken(token,structInfo.name);
   }

   token = tok->PeekToken();

   if(CompareToken(token,";") || !CompareToken(token,"{")){
      return AddType(structInfo);
   } else {
      token = tok->NextToken();
   }

   Assert(CompareToken(token,"{"));

   //structInfo.type = instanceType;
   structInfo.members = &memberBuffer[memberIndex];
   while(1){
      token = tok->PeekToken();

      if(CompareToken(token,"}")){
         tok->NextToken();
         break;
      }

      SimpleMemberInfo info = ParseMember(tok);

      if(tok->Done()){
         break;
      }

      memberBuffer[memberIndex++] = info;
      structInfo.nMembers += 1;
   }

   return AddType(structInfo);
}

void ParseHeaderFile(Tokenizer* tok){
   while(!tok->Done()){
      Token skip = tok->PeekFindUntil("struct");

      if(skip.size == -1){
         tok->Finish();
         break;
      }
      tok->AdvancePeek(skip);

      ParseStruct(tok,false);
   }

   #if 0
   for(int i = 0; i < typeInfoIndex; i++){
      SimpleTypeInfo* info = &typeInfoBuffer[i];

      printf("\n%s\n",info->name);

      for(int ii = 0; ii < info->nMembers; ii++){
         Member* member = &info->members[ii];

         printf("  %s",member->baseTypeName);
         for(int i = 0; i < member->pointers; i++){
            printf("*");
         }

         printf("  %s",member->name);

         if(member->hasArray){
            printf("[%s]",member->arrayDefinition);
         }
         printf(";\n");
      }
   }
   #endif
}

void OutputRegisterTypesFunction(FILE* output){
   fprintf(output,"static void RegisterComplexTypes(){\n");

   for(int i = 0; i < typeInfoIndex; i++){
      SimpleTypeInfo* info = &typeInfoBuffer[i];

      fprintf(output,"  RegisterStruct(A(%s),{\n",info->name);

      bool first = true;
      for(int ii = 0; ii < info->nMembers; ii++){
         SimpleMemberInfo* m = &info->members[ii];

         const char* arrayElements = "0";
         const char* hasArray = "false";
         if(m->hasArray){
            arrayElements = m->arrayDefinition;
            hasArray = "true";
         }

         const char* preamble = "               ";
         if(!first){
            preamble = "              ,";
         } else {
            first = false;
         }

         char buffer[1024];
         char* ptr = buffer;
         ptr += sprintf(ptr,"%s",m->baseTypeName);
         for(int x = 0; x < m->pointers; x++){
            ptr += sprintf(ptr,"*");
         }
         if(m->hasArray){
            ptr += sprintf(ptr,"[%s]",m->arrayDefinition);
         }

         fprintf(output,"%sB(%s,%s,%s,%s,%d,%s,%s)\n",preamble,info->name,buffer,m->baseTypeName,m->name,m->pointers,hasArray,arrayElements);
      }
      fprintf(output,"             });\n");
   }
   fprintf(output,"}\n");
}

#ifdef STANDALONE
int main(int argc,const char* argv[]){
   if(argc < 3){
      printf("Error, need at least 3 arguments: <program> <outputFile> <inputFile1> ...");

      return 0;
   }

   char* buffer = (char*) malloc(256*1024*1024*sizeof(char));

   for(int i = 0; i < argc - 2; i++){
      FILE* input = fopen(argv[2+i],"r");

      if(!input){
         printf("Failed to open file: %s\n",argv[2+i]);
         return 0;
      }

      int fileSize = fread(buffer,sizeof(char),256*1024*1024,input);

      Tokenizer tok(MakeSizedString(buffer,fileSize),"[]{};*<>",{});
      ParseHeaderFile(&tok);
   }

   FILE* output = fopen(argv[1],"w");
   OutputRegisterTypesFunction(output);

   return 0;
}
#endif




