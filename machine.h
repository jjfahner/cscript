#ifndef CSCRIPT_MACHINE_H
#define CSCRIPT_MACHINE_H

#include "types.h"
#include "var.h"

class StackMachine 
{
public:

  //
  // Construction
  //
  StackMachine();

  //
  // Destruction
  //
  ~StackMachine();

  //
  // Execute code
  //
  void Execute(Byte* code, Quad offset = 0);

  //
  // Variable management
  //
  void AddVar(Quad id);
  void AddVar(Quad id, VariantRef const& value);
  void DelVar(Quad id);
  void PushVar(Quad id);

  //
  // Native calls
  //
  inline void NativeCall(Quad id);

  //
  // Literal management
  //
  inline void PushLiteral(Byte* address);

  //
  // Push a new stackframe
  //
  inline void PushStackFrame();
  inline void PopStackFrame();

  //
  // Size of stack
  //
  inline Quad StackSize() const;

  //
  // Top of stack
  //
  inline VariantRef const& StackTop() const;

  //
  // Pop top of stack
  //
  inline void PopStack(size_t index);
  inline VariantRef PopStack();

  //
  // Push onto stack
  //
  inline void PushStack(VariantRef const& value);
  inline void PushStack(Variant const& value);

  //
  // Return stack
  //
  inline void PushRet(Quad offset);
  inline Quad PopRet();

protected:

  //
  // Variables
  //
  typedef std::map<Quad, VariantRef> StackFrame;
  typedef std::stack<StackFrame> VarStack;
  VarStack m_varStack;

  //
  // Stack
  //
  std::stack<VariantRef> m_stack;

  //
  // Register
  //
  std::vector<VariantRef> m_registers;

  //
  // Return stack
  //
  std::stack<Quad> m_return;

};

inline Quad 
StackMachine::StackSize() const
{
  return (Quad)m_stack.size();
}

inline VariantRef const& 
StackMachine::StackTop() const
{
#ifdef _DEBUG
  if(m_stack.size() == 0)
  {
    throw std::runtime_error("Attempt to read top from empty stack");
  }
#endif
  return m_stack.top();
}

inline VariantRef 
StackMachine::PopStack()
{
  VariantRef ref = StackTop();
  m_stack.pop();
  return ref;
}

#endif // #ifndef CSCRIPT_MACHINE_H