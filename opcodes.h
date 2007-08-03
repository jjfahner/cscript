#ifndef CSCRIPT_OPCODES_H
#define CSCRIPT_OPCODES_H

enum opcodes
{
  op_halt,        // Halt execution

  op_pushg,       // Push global on stack
  op_pushl,       // Push literal on stack
  op_pushv,       // Push variable on stack  
  op_pushi,       // Push stack0[stack1]
  op_pop,         // Pop topmost from stack

  op_jmp,         // Unconditional jump
  op_jz,          // Jump if stack top == 0
  op_jnz,         // Jump if stack top != 0
  op_call,        // Call function
  op_ret,         // Return from function
  op_stg,         // Grow stack frame
  op_sts,         // Shrink stack frame

  op_inc,         // Add 1 to value on top of stack
  op_dec,         // Subtract 1 to value on top of stack

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
