#ifndef INCLUDED_MEMORY
#define INCLUDED_MEMORY

#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <stdarg.h>

#include "utils.hpp"
#include "logger.hpp"

template<typename T> inline int Hash(T const& t);

inline size_t Kilobyte(int val){return val * 1024;};
inline size_t Megabyte(int val){return Kilobyte(val) * 1024;};
inline size_t Gigabyte(int val){return Megabyte(val) * 1024;};

inline size_t BitToByteSize(int bitSize){return ((bitSize + 7) / 8);};

int GetPageSize();
void* AllocatePages(int pages);
void DeallocatePages(void* ptr,int pages);
void CheckMemoryStats();

template<typename T>
struct Allocation{
   T* ptr;
   int size;
   int reserved;
};

template<typename T>
bool ZeroOutAlloc(Allocation<T>* alloc,int newSize); // Possible reallocs (returns true if so) but clears all memory allocated to zero

template<typename T>
bool ZeroOutRealloc(Allocation<T>* alloc,int newSize); // Returns true if it actually performed reallocation

template<typename T>
void Reserve(Allocation<T>* alloc,int reservedSize);

template<typename T>
void Alloc(Allocation<T>* alloc,int newSize);

template<typename T>
bool Inside(Allocation<T>* alloc,T* ptr);

template<typename T>
void Free(Allocation<T>* alloc);

template<typename T>
int MemoryUsage(Allocation<T> alloc);

template<typename T>
class PushPtr{
public:
   T* ptr;
   int maximumTimes;
   int timesPushed;

   void Init(T* ptr,int maximum){
      this->ptr = ptr;
      this->maximumTimes = maximum;
      this->timesPushed = 0;
   }

   void Init(Array<T> arr){
      this->ptr = arr.data;
      this->maximumTimes = arr.size;
      this->timesPushed = 0;
   }

   void Init(Allocation<T> alloc){
      this->ptr = alloc.ptr;
      this->maximumTimes = alloc.size;
      this->timesPushed = 0;
   }

   T* Push(int times){
      T* res = &ptr[timesPushed];
      timesPushed += times;

      Assert(timesPushed <= maximumTimes);

      return res;
   }

   bool Empty(){
      bool res = (maximumTimes == timesPushed);
      return res;
   }
};

struct Arena{
   Byte* mem;
   size_t used;
   size_t totalAllocated;
};

void InitArena(Arena* arena,size_t size); // Calls calloc
Arena SubArena(Arena* arena,size_t size);
void PopToSubArena(Arena* top,Arena subArena);
void Free(Arena* arena);
Byte* MarkArena(Arena* arena);
void PopMark(Arena* arena,Byte* mark);
Byte* PushBytes(Arena* arena, int size);
SizedString PointArena(Arena* arena,Byte* mark);
SizedString PushFile(Arena* arena,const char* filepath);
SizedString PushString(Arena* arena,SizedString ss);
SizedString PushString(Arena* arena,const char* format,...) __attribute__ ((format (printf, 2, 3)));
SizedString vPushString(Arena* arena,const char* format,va_list args);
void PushNullByte(Arena* arena);

class ArenaMarker{
   Arena* arena;
   Byte* mark;
public:
   ArenaMarker(Arena* arena){this->arena = arena; this->mark = MarkArena(arena);};
   ~ArenaMarker(){PopMark(this->arena,this->mark);};
};

// Do not abuse stack arenas.
#define STACK_ARENA(NAME,SIZE) \
   Arena NAME = {}; \
   char buffer_##NAME[SIZE]; \
   NAME.mem = buffer_##NAME; \
   NAME.totalAllocated = SIZE;

template<typename T>
Array<T> PushArray(Arena* arena,int size){Array<T> res = {}; res.size = size; res.data = (T*) PushBytes(arena,sizeof(T) * size); return res;};

template<typename T>
Array<T> PointArray(Arena* arena,Byte* mark){SizedString data = PointArena(arena,mark); Array<T> res = {}; res.data = (T*) data.data; res.size = data.size / sizeof(T); return res;}

template<typename T>
T* PushStruct(Arena* arena){T* res = (T*) PushBytes(arena,sizeof(T)); return res;};

class BitArray;

class BitIterator{
public:
   BitArray* array;
   int currentByte;
   int currentBit;

public:
   bool operator!=(BitIterator& iter);
   void operator++();
   int operator*(); // Returns index where it is set to one;
};

class BitArray{
public:
   Byte* memory;
   int bitSize;

public:
   void Init(Byte* memory,int bitSize);
   void Init(Arena* arena,int bitSize);

   void Fill(bool value);
   void Copy(BitArray array);

   int Get(int index);
   void Set(int index,bool value);

   SizedString PrintRepresentation(Arena* output);

   void operator&=(BitArray& other);

   BitIterator begin();
   BitIterator end();
};

template<typename Key,typename Data>
class Hashmap;

class GenericHashmapIterator{
public:
   Byte* memory;
   BitIterator iter;
   BitIterator end;
   int keySize;
   int dataSize;

public:

   void Init(Byte* memory,BitIterator begin,BitIterator end,int keySize,int dataSize);

   bool HasNext();
   void operator++();
   Byte* operator*();
};

template<typename Key,typename Data>
class HashmapIterator{
public:
   Hashmap<Key,Data>* map;
   BitIterator bitIter;

public:
   bool operator!=(HashmapIterator& iter);
   void operator++();
   Pair<Key,Data>& operator*();
};

// An hashmap implementation that uses arenas. Does not allocate any memory after construction. Need to pass current amount of maxAmountOfElements
template<typename Key,typename Data>
struct Hashmap{
   Array<Pair<Key,Data>> memory; // Power of 2 size
   BitArray valid;
   int inserted;
   bool initialized;

public:

   void Init(Arena* arena,int maxAmountOfElements);

   Data* Insert(Key key,Data data);
   Data* InsertIfNotExist(Key key,Data data);

   Data* Get(Key key);
   Data GetOrFail(Key key);

   bool Exists(Key key);

   HashmapIterator<Key,Data> begin();
   HashmapIterator<Key,Data> end();
};

/*
   Pool
*/

struct PoolHeader{
   Byte* nextPage;
   int allocatedUnits;
};

struct PoolInfo{
   int unitsPerFullPage;
   int bitmapSize;
   int unitsPerPage;
   int pageGranuality;
};

struct PageInfo{
   PoolHeader* header;
   Byte* bitmap;
};

class GenericPoolIterator{
   PoolInfo poolInfo;
   PageInfo pageInfo;
   int fullIndex;
   int bit;
   int index;
   int elemSize;
   Byte* page;

public:

   void Init(Byte* page,int elemSize);

   bool HasNext();
   void operator++();
   Byte* operator*();
};

template<typename T> class Pool;

template<typename T>
class PoolIterator{
   Pool<T>* pool;
   PageInfo pageInfo;
   int fullIndex;
   int bit;
   int index;
   Byte* page;
   T* lastVal;

   friend class Pool<T>;

public:

   void Init(Pool<T>* pool,Byte* page);

   void Advance(); // Does not care if valid or not, always advance one position
   bool IsValid();

   bool HasNext(){return (page != nullptr);};
   bool operator!=(PoolIterator& iter);
   void operator++();
   T* operator*();
};

PoolInfo CalculatePoolInfo(int elemSize);
PageInfo GetPageInfo(PoolInfo poolInfo,Byte* page);

// Pool
template<typename T>
struct Pool{
   Byte* mem;
   PoolInfo info;

   T* Alloc();
   T* Alloc(int index); // Returns nullptr if element already allocated at given position
   void Remove(T* elem);

   T* Get(int index); // Returns nullptr if element not allocated (call Alloc(int index) to allocate)
   T& GetOrFail(int index);
   Byte* GetMemoryPtr();

   int Size();
   void Clear(bool clearMemory = false);
   Pool<T> Copy();

   PoolIterator<T> begin();
   PoolIterator<T> end();

   PoolIterator<T> beginNonValid();

   int MemoryUsage();
};

#include "memory.inl"

#endif // INCLUDED_MEMORY
