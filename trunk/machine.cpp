#include "machine.h"
#include "tokens.h"

#include <iostream>
#include <crtdbg.h>

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
StackMachine::AddVar(Quad id)
{
  m_variables[id] = VariantRef(new Variant);
}

void 
StackMachine::AddVar(Quad id, VariantRef const& value)
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
#ifdef _DEBUG
  if(m_variables.count(id) == 0)
  {
    throw std::runtime_error("Variable not created");
  }
#endif
  m_stack.push(m_variables[id]);
}

void
StackMachine::PushLiteral(Quad id)
{
  m_stack.push(VariantRef(new Variant(m_parseContext.GetLiteral(id))));
}

inline void 
StackMachine::PopStack(size_t index)
{
#ifdef _DEBUG
  if(m_stack.size() == 0)
  {
    throw std::runtime_error("Stack underflow");
  }
#endif
  m_registers[index] = m_stack.top();
  m_stack.pop();
}

inline void 
StackMachine::PushStack(VariantRef const& value)
{
  m_stack.push(value);
}

inline void 
StackMachine::PushStack(Variant const& value)
{
  m_stack.push(VariantRef(new Variant(value)));
}

//
// Alias registers
//
#ifdef _DEBUG
#define R0 (*m_registers.at(0))
#define P0 ( m_registers.at(0))
#define R1 (*m_registers.at(1))
#define P1 ( m_registers.at(1))
#else
#define R0 (*m_registers[0])
#define P0 ( m_registers[0])
#define R1 (*m_registers[1])
#define P1 ( m_registers[1])
#endif

//
// Main loop
//
void StackMachine::Run()
{
  // Create registers
  m_registers.clear();
  m_registers.resize(2);
  m_registers[0] = VariantRef(new Variant());
  m_registers[1] = VariantRef(new Variant());

  // Code pointers
  Byte* base = m_parseContext.GetCode();
  Byte* code = base;

  // Helper
  Quad temp;

  // Instruction count
  size_t inscount = 0;

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

    case TOK_PRINT:
      PopStack(0);
      std::wcout << R0.AsString();
      break;

    default:
      throw std::runtime_error("Invalid instruction");
    }

    // Check heap
    //_CrtCheckMemory();

    // Update instruction count
    ++inscount;
  }
}
