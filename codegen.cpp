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
#include "codegen.h"
#include "opcodes.h"

CodeGenerator::CodeGenerator() :
m_code    (0),
m_size    (0),
m_used    (0)
{
}

CodeGenerator::~CodeGenerator()
{
  if(m_code)
  {
    free(m_code);
  }
}

void 
CodeGenerator::Write()
{
  std::ofstream ofs("out.csb", std::ios::binary);
  ofs.write((char*)m_code, m_used);
  ofs.close();
}

void 
CodeGenerator::Reserve(Quad size)
{
  if(m_size - m_used < size)
  {
    m_code = (Byte*)realloc(m_code, size * 2);
    m_size = size * 2;
  }
}

void 
CodeGenerator::PushData(Byte* data, Quad size)
{
  Reserve(m_used + size);
  memmove(m_code + m_used, data, size);
  m_used += size;
}

void 
CodeGenerator::PushByte(Byte data)
{
  Reserve(m_used + sizeof(data));
  *(Byte*)(m_code + m_used) = data;
  m_used += sizeof(Byte);
}

void 
CodeGenerator::PushWord(Word data)
{
  Reserve(m_used + sizeof(data));
  *(Word*)(m_code + m_used) = data;
  m_used += sizeof(Word);
}

void 
CodeGenerator::PushQuad(Quad data)
{
  Reserve(m_used + sizeof(data));
  *(Quad*)(m_code + m_used) = data;
  m_used += sizeof(Quad);
}

void 
CodeGenerator::FillQuad(Quad offset, Quad data)
{
  *(Quad*)(m_code + offset) = data;
}

Quad 
CodeGenerator::PushPatch()
{
  Quad pos = m_used;
  PushQuad(0);
  return pos;
}

void
CodeGenerator::FixPatch(Quad pos)
{
  *(Quad*)(m_code + pos) = m_used;
}

void 
CodeGenerator::PushLiteral(Variant const& value)
{
  m_literals.push_back(Literal(value, m_used));
  PushQuad(0);
}

void 
CodeGenerator::PushFrame()
{
}

void 
CodeGenerator::PopFrame()
{
}

void 
CodeGenerator::PushScope()
{
}

void 
CodeGenerator::PopScope()
{

}

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
CodeGenerator::Execute()
{
  // Instruction pointers
  #define ipb (NextByte(code))
  #define ipw (NextWord(code))
  #define ipq (NextQuad(code))

  // Set code pointers
  Byte* base = m_code;
  Byte* code = m_code;

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
  #define POP(arg)  arg = stack[--SP]

  // Temporaries
  VariantRef P0, P1;
  #define R0 (*P0)
  #define R1 (*P1)
  Quad Q0;

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
    *stack[SP - Q0 - 1] = *stack[SP - 1];
    SP -= Q0;
    break;

  case op_pushl:
    PUSH(ReadLiteral(base + ipq));
    break;

  case op_pushv:
    PUSH(stack[ST + (int)ipq]);
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
    if(Q0 == 0)
    {
      // Very dirty hack
      std::cout << (*stack[SP-2]).AsString();
      stack[SP-1] = Variant::Null;
    }
    else
    {
      rstack.push((Quad)(code - base));
      code = base + Q0;
    }
    break;

  case op_ret:
    code = base + rstack.top();
    rstack.pop();
    break;

  case op_inc:
  case op_dec:
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
