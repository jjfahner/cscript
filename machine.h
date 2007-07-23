#ifndef CSCRIPT_MACHINE_H
#define CSCRIPT_MACHINE_H

#include <string>
#include <stack>
#include <vector>

#include "context.h"

class StackMachine 
{
public:

  //
  // Construction
  //
  StackMachine(ParseContext const& context);

  //
  // Destruction
  //
  ~StackMachine();

  //
  // Execute code
  //
  void Run();

protected:

  //
  // Variable management
  //
  void AddVar(Quad id, Variant const& value);
  void DelVar(Quad id);
  void PushVar(Quad id);

  //
  // Literal management
  //
  void PushLiteral(Quad id);

  //
  // Members
  //
  ParseContext const& m_parseContext;

  //
  // Variables
  //
  typedef std::map<Quad, Variant> Variables;
  Variables m_variables;

  //
  // Pop top of stack into register
  //
  inline void PopStack(size_t index);

  //
  // Push onto stack
  //
  inline void PushStack(Variant const& value);

  //
  // Stack
  //
  std::stack<Variant> m_stack;

  //
  // Register
  //
  std::vector<Variant> m_registers;

};

#endif // #ifndef CSCRIPT_MACHINE_H
