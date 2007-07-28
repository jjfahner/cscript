#ifndef CSCRIPT_MACHINE_H
#define CSCRIPT_MACHINE_H

#include <string>
#include <stack>
#include <vector>

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

protected:

  //
  // Variable management
  //
  void AddVar(Quad id);
  void AddVar(Quad id, VariantRef const& value);
  void DelVar(Quad id);
  void PushVar(Quad id);

  //
  // Literal management
  //
  void PushLiteral(Byte* address);

  //
  // Variables
  //
  typedef std::map<Quad, VariantRef> StackFrame;
  typedef std::stack<StackFrame> VarStack;
  VarStack m_varStack;

  //
  // Push a new stackframe
  //
  inline void PushStackFrame();
  inline void PopStackFrame();

  //
  // Pop top of stack into register
  //
  inline void PopStack(size_t index);

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

#endif // #ifndef CSCRIPT_MACHINE_H
