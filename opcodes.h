#ifndef CSCRIPT_OPCODES_H
#define CSCRIPT_OPCODES_H

enum opcodes
{
  op_halt,
  
  op_pushl,
  op_pushv,
  op_pop,

  op_mov,

  op_jmp,
  op_jz,
  op_jnz,
  op_call,
  op_ret,

  op_idx,

  op_inc,
  op_dec,

  op_add,
  op_sub,
  op_mul,
  op_div,
  op_mod,
  
  op_logor,
  op_logand,
  op_bitor,
  op_bitxor,
  op_bitand,
  
  op_eq,
  op_ne,
  op_lt,
  op_le,
  op_gt,
  op_ge,

  op_assign,
  op_assadd,
  op_asssub,
  op_assmul,
  op_assdiv,
  op_assmod,



};

#endif // CSCRIPT_OPCODES_H
