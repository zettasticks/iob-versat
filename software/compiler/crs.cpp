#include "crs.hpp"

#if 0

Do we need to make a immediate mode interface to build csrs?


Start();



End();

What do we want the code generation to look like?



#if 0
CSR_Builder b;

CSR_Info info = CSR_Terminate(b,out);

Assert(info.valid);


VEmitter *m;

for(CSR_Register reg : info.regs){
  m->Reg(reg.readName,reg.size);
  m->Reg(reg.writeName,reg.size);
}

for(CSR_Register reg : info.regs){
  
}
#endif

#endif
