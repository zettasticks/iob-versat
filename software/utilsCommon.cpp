#include "utils.hpp"

int SimpleHash(SizedString hashing)
{
   int res = 0;

   int prime = 5;
   for(int i = 0; i < hashing.size; i++){
      res += (int) hashing.str[i] * prime;
      res <<= 4;
      prime += 6; // Some not prime, but will find most of them
   }

   return res;
}

char* SizedStringToCStr(SizedString s){
   static char buffer[1024];

   Assert(s.size < 1023);

   memcpy(buffer,s.str,s.size);
   buffer[s.size] = '\0';

   return buffer;
}

// Misc
int RoundUpDiv(int dividend,int divisor){
   int div = dividend / divisor;

   if(dividend % divisor == 0){
      return div;
   } else {
      return (div + 1);
   }
}

int log2i(int value){
   int res = 0;

   if(value <= 0)
      return 0;

   value -= 1; // If value matches a power of 2, we still want to return the previous value

   while(value > 1){
      value /= 2;
      res++;
   }

   res += 1;

   return res;
}

int AlignNextPower2(int val){
   if(val <= 0){
      return 0;
   }

   int res = 1;
   while(res < val){
      res *= 2;
   }

   return res;
}

int RandomNumberBetween(int minimum,int maximum,int randomValue){
   int delta = maximum - minimum;

   if(delta <= 0){
      return minimum;
   }

   int res = minimum + (randomValue % (delta + 1));
   return res;
}

int RolloverRange(int min,int val,int max){
   if(val < min){
      val = max;
   } else if(val > max){
      val = min;
   }

   return val;
}

int Clamp(int min,int val,int max){
   if(val < min){
      return min;
   }
   if(val >= max){
      return max;
   }

   return val;
}

float Sqrt(float n){
   float high = 1.8446734e+19;
   float low = 0.0f;

   float middle;
   for(int i = 0; i < 256; i++){
      middle = (high / 2.0f) + (low / 2.0f); // Divide individually otherwise risk of overflowing the addition

      float squared = middle * middle;

      if(FloatEqual(squared,n)){
         break;
      } else if(squared > n){
         high = middle;
      } else {
         low = middle;
      }
   }

   return middle;
}

float Abs(float val){
   float res = val;
   if(val < 0.0f){
      res = -val;
   }
   return res;
}

int Abs(int val){
   int res = val;
   if(val < 0){
      res = -val;
   }
   return res;
}

int Abs(uint val){
   int conv = (int) val;
   int res = Abs(conv);
   return res;
}

bool FloatEqual(float f0,float f1,float epsilon){
   if(f0 == f1){
      return true;
   }

   float norm = Abs(f0) + Abs(f1);
   float diff = Abs(f0 - f1);

   bool equal = diff < norm * epsilon;

   return equal;
}

int PackInt(float val){
   Conversion conv = {};
   conv.f = val;

   return conv.i;
}

float PackFloat(int val){
   Conversion conv = {};
   conv.i = val;

   return conv.f;
}

float PackFloat(Byte sign,Byte exponent,int mantissa){
   Conversion conv = {};

   conv.ui = COND_BIT_MASK(31,sign) |
             (exponent << 23) |
             MASK_VALUE(mantissa,23);

   return conv.f;
}

int SwapEndianess(int val){
   unsigned char* view = (unsigned char*) &val;

   int res = (view[0] << 24) |
             (view[1] << 16) |
             (view[2] << 8)  |
             (view[3]);

   return res;
}

int NumberDigitsRepresentation(int number){
   int nDigits = 0;

   int num = number;
   if(num < 0){
      nDigits += 1;
      num *= -1;
   }

   while(num > 0){
      num /= 10;
      nDigits += 1;
   }

   return nDigits;
}

char GetHex(int value){
   if(value < 10){
      return '0' + value;
   } else{
      return 'a' + (value - 10);
   }
}

static uint seed = 1;
void SeedRandomNumber(uint val){
   if(val == 0){
      seed = 1;
   } else {
      seed = val;
   }
}

uint GetRandomNumber(){
   // Xorshift
   seed ^= seed << 13;
	seed ^= seed >> 17;
	seed ^= seed << 5;
   return seed;
}

void FixedStringCpy(char* dest,SizedString src){
   int i = 0;
   for(i = 0; i < src.size; i++){
      dest[i] = src.str[i];
   }
   dest[i] = '\0';
}

bool CompareString(SizedString str1,SizedString str2){
   if(str1.size != str2.size){
      return false;
   }

   for(int i = 0; i < str1.size; i++){
      if(str1.str[i] != str2.str[i]){
         return false;
      }
   }

   return true;
}

bool CompareString(const char* str1,SizedString str2){
   // Slower but do not care for now
   bool res = CompareString(MakeSizedString(str1),str2);
   return res;
}

bool CompareString(SizedString str1,const char* str2){
   bool res = CompareString(str2,str1);
   return res;
}

bool CompareString(const char* str1,const char* str2){
   bool res = (strcmp(str1,str2) == 0);
   return res;
}

char GetHexadecimalChar(int value){
   if(value < 10){
      return '0' + value;
   } else{
      return 'a' + (value - 10);
   }
}

unsigned char* GetHexadecimal(const unsigned char* text, int str_size){
   static unsigned char buffer[2048+1];
   int i;

   for(i = 0; i< str_size; i++){
      Assert(i * 2 < 2048);

      int ch = (int) ((unsigned char) text[i]);

      buffer[i*2] = GetHexadecimalChar(ch / 16);
      buffer[i*2+1] = GetHexadecimalChar(ch % 16);
   }

   buffer[(i)*2] = '\0';

   return buffer;
}

bool IsAlpha(char ch){
   if(ch >= 'a' && ch < 'z')
      return true;

   if(ch >= 'A' && ch <= 'Z')
      return true;

   if(ch >= '0' && ch <= '9')
      return true;

   return false;
}
