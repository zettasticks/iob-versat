#include "utils.hpp"

#include "vcdLib.hpp"
#include <linux/fs.h>

int main(int argc,const char* argv[]){
  if(argc != 2){
    printf("Expected a single argument, the input file name\n");
  }

  Arena tempInst = InitArena(Megabyte(128));
  Arena temp2Inst = InitArena(Megabyte(128));

  contextArenas[0] = &tempInst;
  contextArenas[1] = &temp2Inst;

  for(int i = 0; i < 8; i++){
    singleUseCasesArenas[i] = InitArena(Megabyte(1));
  }

  TEMP_REGION(temp,nullptr);

  FILE* file = fopen(argv[1],"r");

  void* fileContent = PushBytes(temp,4096);
  
  VCD_Parser parserInst = {};
  VCD_Parser* parser = &parserInst;

  VCD_StartHeader(parser);
  
  while(!VCD_HeaderFinished(parser)){
    VCD_Request req = VCD_GetRequest(parser);

    if(req.minMemoryToAllocate){
      u64 amountToAllocate = MAX(req.minMemoryToAllocate,4096);
      void* mem = PushBytes(temp,amountToAllocate);
      VCD_GiveMemory(parser,mem,amountToAllocate);
    }

    if(req.minContentToProvide){
      u64 dataOffset = req.contentOffset;
      u64 contentToRead = MAX(req.minContentToProvide,4096);
      
      fseek(file,dataOffset,SEEK_SET);

      ssize_t read = fread(fileContent,contentToRead,sizeof(char),file);

      if(read != contentToRead){
        if(read == 0){
          // Error reading?
        }
      }
      
      VCD_Consume(parser,fileContent,read);
    }
  }

  if(VCD_IsError(parser)){
    return -1;
  }

  
  u64 snapShotSize = VCD_GetSnapshotSizeInBytes(parser);

  struct SingleBitInfo{
    
  };

  // Extract all the variables being used =======================================

  
  
  // Iterate VCD and gather toggle rate info ====================================
  
  VCD_Snapshot* snapShotPreviousBuffer = (VCD_Snapshot*) PushBytes(temp,snapShotSize);
  VCD_Snapshot* snapShotBuffer = (VCD_Snapshot*) PushBytes(temp,snapShotSize);

  u64 lastTimeframe = 0;
  while(!VCD_Done(parser)){
    BLOCK_REGION(temp);

    VCD_TakeSnapshot(parser,snapShotBuffer);
    
    u64 currentTimeframe = VCD_GetTimestamp(snapShotBuffer);

    Array<u32> vars = VCD_GetAllChangedIndexes(snapShotBuffer,temp);

    for(u32 index : vars){
      VCD_Value* previosVal = VCD_GetValue(snapShotPreviousBuffer,index);
      VCD_Value* currentVal = VCD_GetValue(snapShotBuffer,index);

      // Do the thing with the values.
      
    }

    SWAP(snapShotBuffer,snapShotPreviousBuffer);

    VCD_StartAdvance(parser);
    while(!VCD_AdvanceFinished(parser)){
      VCD_Request req = VCD_GetRequest(parser);

      Assert(req.minMemoryToAllocate == 0);
      if(req.minContentToProvide){
        u64 dataOffset = req.contentOffset;
        u64 contentToRead = MAX(req.minContentToProvide,4096);
      
        fseek(file,dataOffset,SEEK_SET);

        ssize_t read = fread(fileContent,contentToRead,sizeof(char),file);

        if(read != contentToRead){
          if(read == 0){
            // Error reading?
          }
        }
      
        VCD_Consume(parser,fileContent,read);
      }
    }

    if(VCD_IsError(parser)){
      break;
    }
  }
  

  return 0;
}


