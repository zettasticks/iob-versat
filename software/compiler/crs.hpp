#pragma once

#if 0

struct CSR_Builder{


};

struct CSR_Register{
  String readName;
  String writeName;
  int address;
  SYM_Expr size;
};

struct CSR_Info{

};

CSR_Builder* CSR_Start(int dataWidth,Arena* out);
CSR_Info* CSR_End(Arena* out);

#endif
