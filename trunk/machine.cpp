#include "machine.h"
#include "tokens.h"

#include <iostream>

StackMachine::StackMachine(ParseContext const& context) :
m_parseContext (context)
{
}

StackMachine::~StackMachine()
{
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
StackMachine::AddVar(Quad id, Variant const& value)
{
  m_variables[id] = value;
}

void
StackMachine::DelVar(Quad id)
{
  m_variables.erase(id);
}

void 
StackMachine::PushVar(Quad id)
{
  m_stack.push(&m_variables[id]);
}

void
StackMachine::PushLiteral(Quad id)
{
  m_stack.push(m_parseContext.GetLiteral(id));
}

inline void 
StackMachine::PopStack(size_t index)
{
  m_registers[index] = m_stack.top();
  m_stack.pop();
}

inline void 
StackMachine::PushStack(Variant const& value)
{
  m_stack.push(value);
}

// Alias registers
#define R0 (m_registers[0])
#define R1 (m_registers[1])

void StackMachine::Run()
{
  m_registers.clear();
  m_registers.resize(20);

  // Code pointers
  Byte* base = m_parseContext.GetCode();
  Byte* code = base;

  // Helper
  Quad temp;

  // Execute code
  for(;;)
  {
    Byte ins = EatByte(code);
    switch(ins)
    {
    case TOK_HALT:
      return;
    
    case TOK_VAR:      
      AddVar(EatQuad(code), Variant());
      break;

    case TOK_VARINIT:
      PopStack(0);
      AddVar(EatQuad(code), R0);
      break;

    case TOK_LVALUE:
      PushVar(EatQuad(code));
      break;

    case TOK_RVALUE:
      PushLiteral(EatQuad(code));
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
      R0.GetRef() += 1LL;
      PushStack(R0);
      break;

    case TOK_PRESUB:
      PopStack(0);
      R0.GetRef() -= 1LL;
      PushStack(R0);
      break;

    case TOK_POSTINC:
      PopStack(0);
      PushStack(R0.GetRef());
      R0.GetRef() += 1LL;
      break;

    case TOK_POSTSUB:
      PopStack(0);
      PushStack(R0.GetRef());
      R0.GetRef() -= 1LL;
      break;

    case TOK_ASSIGN:
      PopStack(1);
      PopStack(0);
      R0.GetRef() = R1.GetDeref();
      PushStack(R0);
      std::wcout << "-> " << R0.AsString() << std::endl;
      break;

    default:
      throw std::runtime_error("Invalid instruction");
    }
  }
}
