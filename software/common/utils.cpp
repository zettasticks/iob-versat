#include "utils.hpp"

#include <dirent.h>

Optional<Array<String>> GetAllFilesInsideDirectory(String dirPath,Arena* arena){
   DIR* dir = opendir(StaticFormat("%.*s",UNPACK_SS(dirPath))); // Make sure it's zero terminated

   if(dir == nullptr){
      return {};
   }

   // Count number of files
   int amount = 0;
   while(1){      
      dirent* d = readdir(dir);

      if(d == nullptr){
         break;
      }
      
      if(d->d_name[0] == '.' && d->d_name[1] == '\0'){
         continue;
      }
      if(d->d_name[0] == '.' && d->d_name[1] == '.' && d->d_name[2] == '\0'){
         continue;
      }

      amount += 1;
   }

   Array<String> arr = PushArray<String>(arena,amount);

   rewinddir(dir);

   int index = 0;
   while(1){      
      dirent* d = readdir(dir);

      if(d == nullptr){
         break;
      }

      if(d->d_name[0] == '.' && d->d_name[1] == '\0'){
         continue;
      }
      if(d->d_name[0] == '.' && d->d_name[1] == '.' && d->d_name[2] == '\0'){
         continue;
      }

      arr[index] = PushString(arena,"%s",d->d_name);
      index += 1;
   }
   
   return arr;
}

String EscapeString(String toEscape,char spaceSubstitute,Arena* out){
  Byte* mark = MarkArena(out);

  for(int i = 0; i < toEscape.size; i++){
    switch(toEscape[i]){
    case '\a': PushString(out,STRING("\\a")); break;
    case '\b': PushString(out,STRING("\\b")); break;
    case '\r': PushString(out,STRING("\\r")); break;
    case '\f': PushString(out,STRING("\\f")); break;
    case '\t': PushString(out,STRING("\\t")); break;
    case '\n': PushString(out,STRING("\\n")); break;
    case '\0': PushString(out,STRING("\\0")); break;
    case '\v': PushString(out,STRING("\\v")); break;
    case ' ': *PushBytes(out,1) = spaceSubstitute; break;
    default:  *PushBytes(out,1) = toEscape[i]; break;
    }
  }

  String res = PointArena(out,mark);
  return res;
}

