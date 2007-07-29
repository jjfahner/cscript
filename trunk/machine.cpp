#include "tokens.h"
#include "machine.h"
#include "native.h"
#include <iostream>

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

inline void 
StackMachine::PushStackFrame()
{
  m_varStack.push(StackFrame());
}

inline void 
StackMachine::PopStackFrame()
{
  m_varStack.pop();
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

inline void 
StackMachine::PushRet(Quad offset)
{
  m_return.push(offset);
}

inline Quad 
StackMachine::PopRet()
{
  Quad top = m_return.top();
  m_return.pop();
  return top;
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
      ExecNative(w1, *this, w2);
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

    default:
      throw std::runtime_error("Invalid instruction");
    }
  }
}
