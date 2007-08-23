//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 JJ Fahner <jan-jaap@jan-jaap.net>
// This file is part of the cscript interpreter.
// CScript can be found at http://svn.jan-jaap.net/
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//////////////////////////////////////////////////////////////////////////
#ifndef CSCRIPT_OPCODES_H
#define CSCRIPT_OPCODES_H

enum opcodes
{
  op_halt,        // Halt execution

  op_pushl,       // Push literal on stack
  op_pushv,       // Push variable on stack  
  op_pushg,       // Push global on stack
  op_pushm,       // Push member on stack
  op_pushr,       // Push RT on stack
  op_pushi,       // Push stack0[stack1]
  op_pusha,       // Push stack top into array
  op_pop,         // Pop topmost from stack
  op_popr,        // Pop topmost into RT
  op_dup,         // Push copy of stack top onto stack
  op_swap,        // Swap top 2 stack entries

  op_jmp,         // Unconditional jump
  op_jz,          // Jump if stack top == 0
  op_jnz,         // Jump if stack top != 0
  op_je,          // Pop top, compare with top, jump if equal
  op_jne,         // Pop top, compare with top, jump if different
  op_call,        // Call function
  op_calln,       // Call native function
  op_ret,         // Return from function
  op_stackg,      // Grow stack frame
  op_stacks,      // Shrink stack frame
  op_stackt,      // Move stack top

  op_iters,       // Convert list to iterator
  op_iterv,       // Assign iterator value to variable
  op_itern,       // Move iterator forward

  op_store,       // Store top of stack in location

  op_negate,      // Negate value
  op_preinc,      // Add 1 to value on top of stack
  op_predec,      // Subtract 1 to value on top of stack
  op_postinc,     // Add 1 to value on top of stack
  op_postdec,     // Subtract 1 to value on top of stack
  op_not,         // Invert boolean lvalue

  op_new,         // Instantiate new object

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
  
  op_seq,
  op_sne,

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
