#include <cstdio>

#include "memory.hpp"
#include "parser.hpp"
#include "utils.hpp"
#include "utilsCore.hpp"

Arena arenaInst;
Arena* arena = &arenaInst;

inline u64 HashString(String data,u64 hash){
  for(int i = 0; i < data.size; i++){
    hash = ((hash << 5) + hash) + data[i];
  }

  return hash;
}

#include "newParser.hpp"

// 0 - exe name
// 1+ - pair of files to calculate the hash
int main(int argc,const char* argv[]){
  if(argc < 2){
    fprintf(stderr,"Need at least 2 arguments: <exe> [list of filepaths] \n");
    return -1;
  }

  arenaInst = InitArena(Megabyte(64));
  Arena inst1 = InitArena(Megabyte(64));
  contextArenas[0] = &inst1;
  Arena inst2 = InitArena(Megabyte(64));
  contextArenas[1] = &inst2;
  
  for(int i = 0; i < 8; i++){
    singleUseCasesArenas[i] = InitArena(Megabyte(1));
  }

  TEMP_REGION(temp,nullptr);

  FREE_ARENA(parsing);
  auto TokenizeFunction = [](const char* start,const char* end) -> TokenizeResult{
    TokenizeResult res = ParseWhitespace(start,end);
    res |= ParseComments(start,end);
    res |= ParseSymbols(start,end);
    res |= ParseNumber(start,end);
    res |= ParseIdentifier(start,end);

    return res;
  };
  
  // djb2
  u64 hash = 5381;
  i32 amountOfTokens = 0;
  for(i32 i = 0; i < argc - 1; i++){
    const char* filepath = argv[i + 1];

    BLOCK_REGION(arena);

    String content = PushFile(arena,filepath);
    
    Parser* parser = StartParsing(content,TokenizeFunction,parsing);

    while(!parser->Done()){
      NewToken token = parser->NextToken();

      String repr = token.originalData;
      for(int i = 0; i < repr.size; i++){
        hash = ((hash << 5) + hash) + repr[i];
      }

      amountOfTokens += 1;
    }
  }

  printf("HASH_RESULT:%d:%lu\n",amountOfTokens,hash);

  return 0;
}
