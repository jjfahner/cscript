#ifndef CSCRIPT_SCOPE_H
#define CSCRIPT_SCOPE_H

#include "types.h"
#include "var.h"
#include "class.h"

class Instance;

struct Function
{
  Instance*   m_inst;
  Ast*        m_code;

  Function(Ast* code = 0, Instance* inst = 0) :
  m_code  (code),
  m_inst  (inst)
  {
  }
};

//////////////////////////////////////////////////////////////////////////
//
// Default scope implementation
//

class Scope
{
public:

  //
  // Types
  //
  typedef std::map<String, VariantRef> Variables;
  typedef std::map<String, Ast*      > Functions;
  typedef std::map<String, Class*    > Classes;

  //
  // Construction
  //
  Scope(Scope* parent = 0) : m_parent (parent)
  {
  }

  //
  // Destruction
  //
  virtual ~Scope()
  {
  }

  //
  // Parent scope
  //
  virtual Scope* GetParent() const
  {
    return m_parent;
  }
  virtual void SetParent(Scope* parent)
  {
    m_parent = parent;
  }


  //////////////////////////////////////////////////////////////////////////
  //
  // Variables
  //

  //
  // Number of variables
  //
  virtual size_t GetVarCount() const
  {
    return m_vars.size();
  }

  //
  // Add a variable
  //
  virtual void AddVar(String const& name, VariantRef const& value)
  {
    if(m_vars.count(name))
    {
      throw std::runtime_error("Variable already declared");
    }
    m_vars[name] = value;
  }

  //
  // Retrieve a variable
  //
  virtual bool FindVar(String const& name, VariantRef& ref) const
  {
    if(FindVarLocal(name, ref))
    {
      return true;
    }
    if(m_parent)
    {
      return m_parent->FindVar(name, ref);
    }
    return false;
  }

  //
  // Retrieve list of variables
  //
  Variables const& GetVariables() const
  {
    return m_vars;
  }

  //////////////////////////////////////////////////////////////////////////
  //
  // Functions
  //

  //
  // Add a function
  //
  virtual void AddFun(String const& name, Ast* fun)
  {
    throw std::runtime_error("Invalid scope for function declaration");
  }

  //
  // Retrieve a function
  //
  virtual bool FindFun(String const& name, Function& fun) const 
  {
    if(FindFunLocal(name, fun))
    {
      return true;
    }
    if(m_parent)
    {
      return m_parent->FindFun(name, fun);
    }
    return false;
  }

  //
  // Retrieve list of functions
  //
  virtual Functions const& GetFunctions() const
  {
    static Functions functions;
    return functions;
  }

  //////////////////////////////////////////////////////////////////////////
  //
  // Classes
  //

  //
  // Add a new class
  //
  virtual void AddClass(Class* node)
  {
    throw std::runtime_error("Invalid context for class declaration");
  }

  //
  // Find a class by name
  //
  virtual bool FindClass(String const& name, Class*& node)
  {
    if(FindClassLocal(name, node))
    {
      return true;
    }
    if(m_parent)
    {
      return m_parent->FindClass(name, node);
    }
    return false;
  }

  //
  // Retrieve list of classes
  //
  virtual Classes const& GetClasses() const
  {
    static Classes classes;
    return classes;
  }

protected:

  //
  // Retrieve a variable from local scope
  //
  virtual bool FindVarLocal(String const& name, VariantRef& ref) const
  {
    Variables::const_iterator it = m_vars.find(name);
    if(it == m_vars.end())
    {
      return false;
    }
    ref = it->second;
    return true;
  }

  //
  // Retrieve a function from local scope
  //
  virtual bool FindFunLocal(String const& name, Function& node) const
  {
    return false;
  }

  //
  // Retrieve a class from local scope
  //
  virtual bool FindClassLocal(String const& name, Class*& node) const
  {
    return false;
  }

  //
  // Members
  //
  Scope*    m_parent;
  Variables m_vars;

};

//////////////////////////////////////////////////////////////////////////
//
// Global scope
//

class GlobalScope : public Scope
{
public:

  //////////////////////////////////////////////////////////////////////////
  //
  // Functions
  //

  //
  // Add a function
  //
  virtual void AddFun(String const& name, Ast* fun)
  {
    if(m_funs.count(name))
    {
      throw std::runtime_error("Function already declared");
    }
    m_funs[name] = fun;
  }

  //
  // Retrieve list of functions
  //
  virtual Functions const& GetFunctions() const
  {
    return m_funs;
  }

  //
  // Add class to this scope
  //
  virtual void AddClass(Class* c)
  {
    if(m_classes.count(c->GetName()))
    {
      throw std::runtime_error("Class already declared");
    }
    m_classes[c->GetName()] = c;
  }

  //
  // Retrieve list of classes
  //
  virtual Classes const& GetClasses() const
  {
    return m_classes;
  }

protected:

  //
  // Retrieve a function
  //
  virtual bool FindFunLocal(String const& name, Function& fun) const
  {
    Functions::const_iterator it = m_funs.find(name);
    if(it == m_funs.end())
    {
      return false;
    }
    fun.m_inst = 0;
    fun.m_code = it->second;
    return true;
  }

  //
  // Retrieve a class
  //
  virtual bool FindClassLocal(String const& name, Class*& node) const
  {
    Classes::const_iterator it = m_classes.find(name);
    if(it == m_classes.end())
    {
      return false;
    }
    node = it->second;
    return true;
  }


  //
  // Members
  //
  Functions m_funs;
  Classes   m_classes;

};

//////////////////////////////////////////////////////////////////////////
//
// Class scope
//
class ClassScope : public Scope
{
public:

  //
  // Construction
  //
  ClassScope(Scope* parent, class Instance* inst) :
  Scope  (parent),
  m_inst (inst)
  {
  }

  //
  // Class instance
  //
  Instance* GetInstance() const
  {
    return m_inst;
  }

  //
  // Number of variables
  //
  virtual size_t GetVarCount() const
  {
    return m_inst->GetVarCount();
  }

  //
  // Variables may not be added to class scopes
  //
  virtual void AddVar(String const& name, VariantRef const& value)
  {
    throw std::runtime_error("Cannot add variables to class scope");
  }

protected:

  //
  // Retrieve a local variable
  //
  virtual bool FindVarLocal(String const& name, VariantRef& ref) const
  {
    return m_inst->FindVar(name, ref);
  }

  //
  // Retrieve a local function
  //
  virtual bool FindFunLocal(String const& name, Function& fun) const
  {
    Ast* node = 0;
    if(m_inst->FindFun(name, fun.m_code))
    {
      fun.m_inst = m_inst;
      return true;
    }
    return false;
  }

  //
  // Members
  //
  class Instance* m_inst;

};

#endif // CSCRIPT_RTSCOPE_H
