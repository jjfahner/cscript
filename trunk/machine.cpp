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
#include "opcodes.h"
#include "machine.h"
#include "native.h"

inline Byte NextByte(Byte*& code)
{
  Byte b = *code;
  code += 1;
  return b;
}

inline Word NextWord(Byte*& code)
{
  Word b = *(Word*)code;
  code += 2;
  return b;
}

inline Quad NextQuad(Byte*& code)
{
  Quad b = *(Quad*)code;
  code += 4;
  return b;
}

inline Variant ReadLiteral(Byte* code)
{
  Variant v;
  v.Read(code);
  return v;
}

void 
Machine::Execute(Byte* source, Quad offset)
{
  // Instruction pointers
  #define ipb (NextByte(code))
  #define ipw (NextWord(code))
  #define ipq (NextQuad(code))

  // Set code pointers
  Byte* base = source;
  Byte* code = source + offset;

  // Create stacks
  std::vector<VariantRef> stack;    // Temporaries
  std::stack<Quad>        tstack;   // Stack top
  std::stack<Quad>        rstack;   // Return stack

  // Resize stack
  stack.resize(10000);

  // Stack info
  Quad ST = 0;
  Quad SP = 0;

  // Stack manipulation
  #define PUSH(arg) stack[SP++] = arg
  #define POP(arg)  { arg = stack[--SP]; stack[SP].Clear(); }

  // Temporaries
  VariantRef P0, P1;
  #define R0 (*P0)
  #define R1 (*P1)
  Quad Q0;
  Word W0, W1;

  // Start of instruction
begin:

  // Execute single instruction
  switch(ipb)
  {
  case op_halt:
    return;

  case op_stackg:
    tstack.push(ST);
    ST = SP;
    SP = SP + ipq;
    break;

  case op_stacks:
    ST = tstack.top();
    SP = SP - ipq;
    tstack.pop();
    break;

  case op_stackt:
    Q0 = ipq;
    stack[SP - Q0 - 1] = stack[SP - 1];
    SP -= Q0;
    break;

  case op_pushl:
    PUSH(ReadLiteral(base + ipq));
    break;

  case op_pushv:
    PUSH(stack[ST + (int)ipq]);
    break;

  case op_pushg:
    PUSH(stack[ipq]);
    break;

  case op_pusha:
    POP(P1);
    POP(P0);
    R0.Append(P1);
    PUSH(P0);
    break;

  case op_store:
    POP(P0);
    stack[ST + (int)ipq] = R0;
    break;

  case op_pushi:
    POP(P1);
    POP(P0);
    PUSH((R0)[R1]);
    break;

  case op_pop:
    --SP;
    break;

  case op_jmp:
    code = base + ipq;
    break;

  case op_jz:
    Q0 = ipq;
    POP(P0);
    if(!R0) code = base + Q0;
    break;

  case op_jnz:
    Q0 = ipq;
    POP(P0);
    if(R0) code = base + Q0;
    break;

  case op_call:
    Q0 = ipq;
    rstack.push((Quad)(code - base));
    code = base + Q0;
    break;

  case op_calln:
    W0 = ipw;
    W1 = ipw;
    ExecNative(W0, W1, stack, SP);
    break;

  case op_ret:
    code = base + rstack.top();
    rstack.pop();
    break;

  case op_preinc:
    ++(*stack[SP-1]);
    break;
  case op_predec:
    --(*stack[SP-1]);
    break;

  case op_postinc:
    POP(P0);
    PUSH(R0);
    ++R0;
    break;
  case op_postdec:
    POP(P0);
    PUSH(R0);
    --R0;
    break;

  case op_add:
    POP(P1);
    POP(P0);
    PUSH(R0 + R1);
    break;

  case op_sub:
    POP(P1);
    POP(P0);
    PUSH(R0 - R1);
    break;

  case op_mul:
    POP(P1);
    POP(P0);
    PUSH(R0 * R1);
    break;

  case op_div:
    POP(P1);
    POP(P0);
    PUSH(R0 / R1);
    break;

  case op_mod:  
    POP(P1);
    POP(P0);
    PUSH(R0 % R1);
    break;

  case op_logor:
    POP(P1);
    POP(P0);
    PUSH(R0 || R1);
    break;

  case op_logand:
    POP(P1);
    POP(P0);
    PUSH(R0 && R1);
    break;

  case op_eq:
    POP(P1);
    POP(P0);
    PUSH(R0 == R1);
    break;

  case op_ne:
    POP(P1);
    POP(P0);
    PUSH(R0 != R1);
    break;

  case op_lt:
    POP(P1);
    POP(P0);
    PUSH(R0 < R1);
    break;

  case op_le:
    POP(P1);
    POP(P0);
    PUSH(R0 <= R1);
    break;

  case op_gt:
    POP(P1);
    POP(P0);
    PUSH(R0 > R1);
    break;

  case op_ge:
    POP(P1);
    POP(P0);
    PUSH(R0 >= R1);
    break;

  case op_assign:
    POP(P1);
    POP(P0);
    R0 = R1;
    PUSH(P0);
    break;

  case op_assadd:
    POP(P1);
    POP(P0);
    R0 += R1;
    PUSH(P0);
    break;

  case op_asssub:
    POP(P1);
    POP(P0);
    R0 -= R1;
    PUSH(P0);
    break;

  case op_assmul:
    POP(P1);
    POP(P0);
    R0 *= R1;
    PUSH(P0);
    break;

  case op_assdiv:
    POP(P1);
    POP(P0);
    R0 /= R1;
    PUSH(P0);
    break;

  case op_assmod:
    POP(P1);
    POP(P0);
    R0 %= R1;
    PUSH(P0);
    break;

//   case op_bitor:
//   case op_bitxor:
//   case op_bitand:  
//     break;

  default:
    goto invalid;
  }

  // Next instruction
  goto begin;

// Invalid instruction
invalid:

  std::cout << "Invalid instruction\n";
}
