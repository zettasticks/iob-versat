enum SP_Type {
  SP_Type_NIL,
  SP_Type_NOT,
  SP_Type_SUB,
  SP_Type_LITERAL,
  SP_Type_FUNC_CALL,
  SP_Type_AND,
  SP_Type_OR,
  SP_Type_XOR,
  SP_Type_RHL,
  SP_Type_RHR,
  SP_Type_SHL,
  SP_Type_SHR,
  SP_Type_MUL,
  SP_Type_DIV,
  SP_Type_ADD,
  SP_Type_EXPR,
  SP_Type_RANGE,
  SP_Type_VAR,
  SP_Type_ACCESS,
  SP_Type_RANGE_DECL,
  SP_Type_DELAY_DECL,
  SP_Type_PORT_ACCESS,
  SP_Type_VAR_DECL,
  SP_Type_MODULE_INPUTS,
  SP_Type_MODIFIER_DEBUG,
  SP_Type_MODIFIER_STATIC,
  SP_Type_MODIFIER_SHARE,
  SP_Type_MODIFIER_SIM,
  SP_Type_MODIFIER_LIST,
  SP_Type_ID,
  SP_Type_PARAM,
  SP_Type_PARAM_LIST,
  SP_Type_VARIABLE_DECL,
  SP_Type_VAR_LIST,
  SP_Type_FOR_LOOP,
  SP_Type_GEN_LOOP,
  SP_Type_FUNCTION_CALL,
  SP_Type_EQUALITY,
  SP_Type_CONNECTION,
  SP_Type_PARAM_DECL,
  SP_Type_DECL_LIST,
  SP_Type_CON_LIST,
  SP_Type_FUNC_LIST,
  SP_Type_MODULE_DECL,
  SP_Type_FUNC_CONFIG,
  SP_Type_FUNC_STATE,
  SP_Type_FUNC_MEM,
  SP_Type_FUNC_TYPE_BUFFER,
  SP_Type_FUNC_TYPE_DYN,
  SP_Type_FUNC_TYPE_FIXED,
  SP_Type_STMT,
  SP_Type_STMT_LIST,
};
static String SP_Type_Name(SP_Type in){
  String res = {};
  bool didIt = 0;
  switch(in){
  case SP_Type_NIL: res = "NIL"; didIt = 1; break;
  case SP_Type_NOT: res = "NOT"; didIt = 1; break;
  case SP_Type_SUB: res = "SUB"; didIt = 1; break;
  case SP_Type_LITERAL: res = "LITERAL"; didIt = 1; break;
  case SP_Type_FUNC_CALL: res = "FUNC_CALL"; didIt = 1; break;
  case SP_Type_AND: res = "AND"; didIt = 1; break;
  case SP_Type_OR: res = "OR"; didIt = 1; break;
  case SP_Type_XOR: res = "XOR"; didIt = 1; break;
  case SP_Type_RHL: res = "RHL"; didIt = 1; break;
  case SP_Type_RHR: res = "RHR"; didIt = 1; break;
  case SP_Type_SHL: res = "SHL"; didIt = 1; break;
  case SP_Type_SHR: res = "SHR"; didIt = 1; break;
  case SP_Type_MUL: res = "MUL"; didIt = 1; break;
  case SP_Type_DIV: res = "DIV"; didIt = 1; break;
  case SP_Type_ADD: res = "ADD"; didIt = 1; break;
  case SP_Type_EXPR: res = "EXPR"; didIt = 1; break;
  case SP_Type_RANGE: res = "RANGE"; didIt = 1; break;
  case SP_Type_VAR: res = "VAR"; didIt = 1; break;
  case SP_Type_ACCESS: res = "ACCESS"; didIt = 1; break;
  case SP_Type_RANGE_DECL: res = "RANGE_DECL"; didIt = 1; break;
  case SP_Type_DELAY_DECL: res = "DELAY_DECL"; didIt = 1; break;
  case SP_Type_PORT_ACCESS: res = "PORT_ACCESS"; didIt = 1; break;
  case SP_Type_VAR_DECL: res = "VAR_DECL"; didIt = 1; break;
  case SP_Type_MODULE_INPUTS: res = "MODULE_INPUTS"; didIt = 1; break;
  case SP_Type_MODIFIER_DEBUG: res = "MODIFIER_DEBUG"; didIt = 1; break;
  case SP_Type_MODIFIER_STATIC: res = "MODIFIER_STATIC"; didIt = 1; break;
  case SP_Type_MODIFIER_SHARE: res = "MODIFIER_SHARE"; didIt = 1; break;
  case SP_Type_MODIFIER_SIM: res = "MODIFIER_SIM"; didIt = 1; break;
  case SP_Type_MODIFIER_LIST: res = "MODIFIER_LIST"; didIt = 1; break;
  case SP_Type_ID: res = "ID"; didIt = 1; break;
  case SP_Type_PARAM: res = "PARAM"; didIt = 1; break;
  case SP_Type_PARAM_LIST: res = "PARAM_LIST"; didIt = 1; break;
  case SP_Type_VARIABLE_DECL: res = "VARIABLE_DECL"; didIt = 1; break;
  case SP_Type_VAR_LIST: res = "VAR_LIST"; didIt = 1; break;
  case SP_Type_FOR_LOOP: res = "FOR_LOOP"; didIt = 1; break;
  case SP_Type_GEN_LOOP: res = "GEN_LOOP"; didIt = 1; break;
  case SP_Type_FUNCTION_CALL: res = "FUNCTION_CALL"; didIt = 1; break;
  case SP_Type_EQUALITY: res = "EQUALITY"; didIt = 1; break;
  case SP_Type_CONNECTION: res = "CONNECTION"; didIt = 1; break;
  case SP_Type_PARAM_DECL: res = "PARAM_DECL"; didIt = 1; break;
  case SP_Type_DECL_LIST: res = "DECL_LIST"; didIt = 1; break;
  case SP_Type_CON_LIST: res = "CON_LIST"; didIt = 1; break;
  case SP_Type_FUNC_LIST: res = "FUNC_LIST"; didIt = 1; break;
  case SP_Type_MODULE_DECL: res = "MODULE_DECL"; didIt = 1; break;
  case SP_Type_FUNC_CONFIG: res = "FUNC_CONFIG"; didIt = 1; break;
  case SP_Type_FUNC_STATE: res = "FUNC_STATE"; didIt = 1; break;
  case SP_Type_FUNC_MEM: res = "FUNC_MEM"; didIt = 1; break;
  case SP_Type_FUNC_TYPE_BUFFER: res = "FUNC_TYPE_BUFFER"; didIt = 1; break;
  case SP_Type_FUNC_TYPE_DYN: res = "FUNC_TYPE_DYN"; didIt = 1; break;
  case SP_Type_FUNC_TYPE_FIXED: res = "FUNC_TYPE_FIXED"; didIt = 1; break;
  case SP_Type_STMT: res = "STMT"; didIt = 1; break;
  case SP_Type_STMT_LIST: res = "STMT_LIST"; didIt = 1; break;
  }
  Assert(didIt);
  return res;
}
static bool SP_Type_IsExpr(SP_Type in){
  bool res = {};
  bool didIt = 0;
  switch(in){
  case SP_Type_NIL: res = 0; didIt = 1; break;
  case SP_Type_NOT: res = 1; didIt = 1; break;
  case SP_Type_SUB: res = 1; didIt = 1; break;
  case SP_Type_LITERAL: res = 1; didIt = 1; break;
  case SP_Type_FUNC_CALL: res = 1; didIt = 1; break;
  case SP_Type_AND: res = 1; didIt = 1; break;
  case SP_Type_OR: res = 1; didIt = 1; break;
  case SP_Type_XOR: res = 1; didIt = 1; break;
  case SP_Type_RHL: res = 1; didIt = 1; break;
  case SP_Type_RHR: res = 1; didIt = 1; break;
  case SP_Type_SHL: res = 1; didIt = 1; break;
  case SP_Type_SHR: res = 1; didIt = 1; break;
  case SP_Type_MUL: res = 1; didIt = 1; break;
  case SP_Type_DIV: res = 1; didIt = 1; break;
  case SP_Type_ADD: res = 1; didIt = 1; break;
  case SP_Type_EXPR: res = 1; didIt = 1; break;
  case SP_Type_RANGE: res = 0; didIt = 1; break;
  case SP_Type_VAR: res = 0; didIt = 1; break;
  case SP_Type_ACCESS: res = 0; didIt = 1; break;
  case SP_Type_RANGE_DECL: res = 0; didIt = 1; break;
  case SP_Type_DELAY_DECL: res = 0; didIt = 1; break;
  case SP_Type_PORT_ACCESS: res = 0; didIt = 1; break;
  case SP_Type_VAR_DECL: res = 0; didIt = 1; break;
  case SP_Type_MODULE_INPUTS: res = 0; didIt = 1; break;
  case SP_Type_MODIFIER_DEBUG: res = 0; didIt = 1; break;
  case SP_Type_MODIFIER_STATIC: res = 0; didIt = 1; break;
  case SP_Type_MODIFIER_SHARE: res = 0; didIt = 1; break;
  case SP_Type_MODIFIER_SIM: res = 0; didIt = 1; break;
  case SP_Type_MODIFIER_LIST: res = 0; didIt = 1; break;
  case SP_Type_ID: res = 0; didIt = 1; break;
  case SP_Type_PARAM: res = 0; didIt = 1; break;
  case SP_Type_PARAM_LIST: res = 0; didIt = 1; break;
  case SP_Type_VARIABLE_DECL: res = 0; didIt = 1; break;
  case SP_Type_VAR_LIST: res = 0; didIt = 1; break;
  case SP_Type_FOR_LOOP: res = 0; didIt = 1; break;
  case SP_Type_GEN_LOOP: res = 0; didIt = 1; break;
  case SP_Type_FUNCTION_CALL: res = 0; didIt = 1; break;
  case SP_Type_EQUALITY: res = 0; didIt = 1; break;
  case SP_Type_CONNECTION: res = 0; didIt = 1; break;
  case SP_Type_PARAM_DECL: res = 0; didIt = 1; break;
  case SP_Type_DECL_LIST: res = 0; didIt = 1; break;
  case SP_Type_CON_LIST: res = 0; didIt = 1; break;
  case SP_Type_FUNC_LIST: res = 0; didIt = 1; break;
  case SP_Type_MODULE_DECL: res = 0; didIt = 1; break;
  case SP_Type_FUNC_CONFIG: res = 0; didIt = 1; break;
  case SP_Type_FUNC_STATE: res = 0; didIt = 1; break;
  case SP_Type_FUNC_MEM: res = 0; didIt = 1; break;
  case SP_Type_FUNC_TYPE_BUFFER: res = 0; didIt = 1; break;
  case SP_Type_FUNC_TYPE_DYN: res = 0; didIt = 1; break;
  case SP_Type_FUNC_TYPE_FIXED: res = 0; didIt = 1; break;
  case SP_Type_STMT: res = 0; didIt = 1; break;
  case SP_Type_STMT_LIST: res = 0; didIt = 1; break;
  }
  Assert(didIt);
  return res;
}
