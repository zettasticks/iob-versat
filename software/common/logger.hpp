#pragma once

// TODO NOTE: Honestly do not know if this is any good. Since we already have a decent amount of functions that can print out the majority of data that we care about, we might just remove the few usages of logging. Running the program in a debugger while or checking the printouts manually seems a better approach to bug finding than putting a bunch of logs.

//                          76543210
#define ENABLE_LOG_MODULE 0b11111111
#define MINIMUM_LOG_LEVEL 0

enum LogModule{
   MEMORY      = 0x1,
   TOP_SYS     = 0x2,
   ACCELERATOR = 0x4,
   PARSER      = 0x8,
   TYPE        = 0x20,
   TEMPLATE    = 0x40,
   UTILS       = 0x80
};

enum LogLevel{
   DEBUG   = 0, // Info that user doesn't care about
   INFO    = 1, // General info that every user might want to know
   WARN    = 2, // Not a problem, but can lead to poor results (performance, unexpected results, etc..)
   ERROR   = 3, // Problem but can continue
   FATAL   = 4  // Problem that cannot continue
};

#define LogDebug(MODULE,...) Log_(MODULE,LogLevel::DEBUG,__LINE__,__FILE__,__PRETTY_FUNCTION__,__VA_ARGS__)
#define LogInfo(MODULE,...) Log_(MODULE,LogLevel::INFO,__LINE__,__FILE__,__PRETTY_FUNCTION__,__VA_ARGS__)
#define LogWarn(MODULE,...)  Log_(MODULE,LogLevel::WARN,__LINE__,__FILE__,__PRETTY_FUNCTION__,__VA_ARGS__)
#define LogError(MODULE,...) Log_(MODULE,LogLevel::ERROR,__LINE__,__FILE__,__PRETTY_FUNCTION__,__VA_ARGS__)
#define LogFatal(MODULE,...) Log_(MODULE,LogLevel::FATAL,__LINE__,__FILE__,__PRETTY_FUNCTION__,__VA_ARGS__)

void Log_(LogModule module,LogLevel level,int line,const char* filename,const char* funcName,const char* format, ...) __attribute__ ((format (printf, 6, 7)));

#define LogOnce(...) \
   do { \
      static bool firstTimeLog_ = true; \
      if(firstTimeLog_){ \
         Log(__VA_ARGS__); \
         firstTimeLog_ = false; \
      } \
   } while(0)
