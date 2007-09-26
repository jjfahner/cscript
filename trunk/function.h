#ifndef CSCRIPT_FUNCTION_H
#define CSCRIPT_FUNCTION_H

#include "types.h"
#include "var.h"
#include "native.h"

class Evaluator;
typedef std::vector<VariantRef> Arguments;

//////////////////////////////////////////////////////////////////////////
//
// Base class for functions
//

class Function
{
public:

  //
  // Construction
  //
  Function (String name) :
  m_name (name)
  {
  }

  //
  // Destruction
  //
  virtual ~Function()
  {
  }

  //
  // Function name
  //
  virtual String GetName() const
  {
    return m_name;
  }

  //
  // Parameter list
  //
  virtual AstList const* GetParameters() const = 0;

  //
  // Execute function 
  //
  virtual VariantRef Execute(Evaluator& evaluator, Arguments const& args) = 0;

protected:

  //
  // Members
  //
  String m_name;

};

//////////////////////////////////////////////////////////////////////////
//
// Script function
//

class ScriptFunction : public Function
{
public:

  //
  // Construction
  // 
  ScriptFunction(String name, Ast* node) :
  Function (name),
  m_node   (node)
  {
  }

  //
  // Retrieve node
  //
  Ast* GetNode() const
  {
    return m_node;
  }

  //
  // Parameter list
  //
  virtual AstList const* GetParameters() const;

  //
  // Execution
  //
  virtual VariantRef Execute(Evaluator& evaluator, Arguments const& args);

protected:

  //
  // Members
  //
  Ast* m_node;

};

//////////////////////////////////////////////////////////////////////////
//
// Native function
//

class NativeFunction : public Function
{
public:

  //
  // Construction
  //
  NativeFunction(String decl, NativeCall call);

  //
  // Parameter list
  //
  virtual AstList const* GetParameters() const
  {
    return m_pars;
  }

  //
  // Execution
  //
  virtual VariantRef Execute(Evaluator& evaluator, Arguments const& args);

protected:

  //
  // Members
  //
  NativeCall  m_call;
  AstList*    m_pars;

};

//////////////////////////////////////////////////////////////////////////
//
// Native function
//

class ExternFunction : public Function
{
public:

  //
  // Construction
  //
  ExternFunction(String name) :
  Function (name)
  {
  }

  //
  // Parameter list
  //
  virtual AstList const* GetParameters() const
  {
    throw std::runtime_error("Not implemented");
  }

  //
  // Execution
  //
  virtual VariantRef Execute(Evaluator& evaluator, Arguments const& args);

};

#endif // CSCRIPT_FUNCTION_H
