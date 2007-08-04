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
#include "tokens.h"
#include "machine.h"
#include "native.h"

StackMachine::StackMachine()
{
  // Create initial stack frame
  PushStackFrame();
}

StackMachine::~StackMachine()
{
  // Pop initial stack frame
  PopStackFrame();

  // Check stack
  if(m_varStack.size() != 0)
  {
    throw std::runtime_error("Stack not empty");
  }
}

inline Byte EatByte(Byte*& source)
{
  Byte b = *source;
  ++source;
  return b;
}

inline Word EatWord(Byte*& source)
{
  Word w = *(Word*)source;
  source += 2;
  return w;
}

inline Quad EatQuad(Byte*& source)
{
  Quad q = *(Quad*)source;
  source += 4;
  return q;
}

void 
StackMachine::AddVar(Quad id)
{
  m_varStack.top()[id] = VariantRef(new Variant);
}

void 
StackMachine::AddVar(Quad id, VariantRef const& value)
{
  m_varStack.top()[id] = value;
}

void
StackMachine::DelVar(Quad id)
{
  m_varStack.top().erase(id);
}

void 
StackMachine::PushVar(Quad id)
{
#ifdef _DEBUG
  if(m_varStack.top().count(id) == 0)
  {
    throw std::runtime_error("Variable not created");
  }
#endif
  m_stack.push(m_varStack.top()[id]);
}

void
StackMachine::PushLiteral(Byte* address)
{
  VariantRef ref(new Variant);
  ref->Read(address);
  m_stack.push(ref);
}

//
// Main loop
//
void 
StackMachine::Execute(Byte* base, Quad offset)
{
  // Create registers
  m_registers.clear();
  m_registers.resize(2);

  // Create register refs
  VariantRef& P0 = m_registers[0];
  VariantRef& P1 = m_registers[1];

  // Define aliasses
  #define R0 (*P0)
  #define R1 (*P1)

  // Code pointers
  Byte* code = base + offset;

  // Helpers
  Quad temp;
  Word w1, w2;

  // Execute code
  for(;;)
  {
    Byte ins = EatByte(code);
    switch(ins)
    {
    case TOK_HALT:
      return;

    case TOK_VAR:      
      AddVar(EatQuad(code));
      break;

    case TOK_VARINIT:
      PopStack(0);
      AddVar(EatQuad(code), P0);
      break;

    case TOK_LVALUE:
      PushVar(EatQuad(code));
      break;

    case TOK_RVALUE:
      PushLiteral(base + EatQuad(code));
      break;

    case TOK_INDEX:
      PopStack(1);
      PopStack(0);
      PushStack(R0[R1]);
      break;

    case TOK_ARRAY:
      PopStack(1);
      PopStack(0);
      R0.Append(P1);
      PushStack(P0);
      break;

    case TOK_POP:
      m_stack.pop();
      break;

    case TOK_UNDEF:
      DelVar(EatQuad(code));
      break;

    case TOK_JMP:
      code = base + EatQuad(code);
      break;

    case TOK_JZ:
      PopStack(0);
      temp = EatQuad(code);
      if(!R0.AsBool())
        code = base + temp;
      break;

    case TOK_JNZ:
      PopStack(0);
      temp = EatQuad(code);
      if(R0.AsBool())
        code = base + temp;
      break;

    case TOK_CALL:
      temp = EatQuad(code);
      PushRet((Quad)(code - base));      
      code = base + temp;
      PushStackFrame();
      break;

    case TOK_CALLN:
      w1 = EatWord(code);
      w2 = EatWord(code);
      //ExecNative(w1, *this, w2);
      break;        

    case TOK_RET:
      code = base + PopRet();
      PopStackFrame();
      break;

    case TOK_ADDOP:
      PopStack(1);
      PopStack(0);
      PushStack(R0 + R1);
      break;

    case TOK_SUBOP:
      PopStack(1);
      PopStack(0);
      PushStack(R0 - R1);
      break;

    case TOK_MULOP:
      PopStack(1);
      PopStack(0);
      PushStack(R0 * R1);
      break;

    case TOK_DIVOP:
      PopStack(1);
      PopStack(0);
      PushStack(R0 / R1);
      break;

    case TOK_MODOP:
      PopStack(1);
      PopStack(0);
      PushStack(R0 % R1);
      break;

    case TOK_LOGOR:
      PopStack(1);
      PopStack(0);
      PushStack(R0 || R1);
      break;

    case TOK_LOGAND:
      PopStack(1);
      PopStack(0);
      PushStack(R0 && R1);
      break;

    case TOK_GT:
      PopStack(1);
      PopStack(0);
      PushStack(R0 > R1);
     break;

    case TOK_GE:
      PopStack(1);
      PopStack(0);
      PushStack(R0 >= R1);
      break;

    case TOK_ST:
      PopStack(1);
      PopStack(0);
      PushStack(R0 < R1);
      break;

    case TOK_SE:
      PopStack(1);
      PopStack(0);
      PushStack(R0 <= R1);
      break;

    case TOK_EQUALS:
      PopStack(1);
      PopStack(0);
      PushStack(R0 == R1);
      break;

    case TOK_NEQUALS:
      PopStack(1);
      PopStack(0);
      PushStack(R0 != R1);
      break;

    case TOK_PREINC:
      PopStack(0);
      R0 += 1LL;
      PushStack(P1);
      break;

    case TOK_PRESUB:
      PopStack(0);
      R0 -= 1LL;
      PushStack(P1);
      break;

    case TOK_POSTINC:
      PopStack(0);
      PushStack(R0);
      R0 += 1LL;
      break;

    case TOK_POSTSUB:
      PopStack(0);
      PushStack(R0);
      R0 -= 1LL;
      break;

    case TOK_ASSIGN:
      PopStack(1);
      PopStack(0);
      R0 = R1;
      PushStack(P0);
      break;

    case TOK_ASSADD:
      PopStack(1);
      PopStack(0);
      R0 += R1;
      PushStack(P0);
      break;

    case TOK_ASSSUB:
      PopStack(1);
      PopStack(0);
      R0 -= R1;
      PushStack(P0);
      break;

    case TOK_ASSMUL:
      PopStack(1);
      PopStack(0);
      R0 *= R1;
      PushStack(P0);
      break;

    case TOK_ASSDIV:
      PopStack(1);
      PopStack(0);
      R0 /= R1;
      PushStack(P0);
      break;

    case TOK_ASSMOD:
      PopStack(1);
      PopStack(0);
      R0 %= R1;
      PushStack(P0);
      break;

    default:
      throw std::runtime_error("Invalid instruction");
    }
  }
}
