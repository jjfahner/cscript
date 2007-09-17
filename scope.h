#ifndef CSCRIPT_SCOPE_H
#define CSCRIPT_SCOPE_H

#include "types.h"
#include "var.h"

//////////////////////////////////////////////////////////////////////////
//
// Runtime class
//

class Class
{
public:

  //
  // Construction
  //
  Class(String const& name) : m_name (name)
  {
  }

  //
  // Add a member variable
  //
  virtual void AddVar(String const& name, Ast* node)
  {
    if(m_vars.count(name))
    {
      throw std::runtime_error("Variable already declared");
    }
    m_vars[name] = node;
  }

  //
  // Add a member function
  //
  virtual void AddFun(String const& name, Ast* node)
  {
    if(m_funs.count(name))
    {
      throw std::runtime_error("Variable already declared");
    }
    m_funs[name] = node;
  }

  //
  // Find a member function
  //
  virtual bool FindFun(String const& name, Ast*& fun) const 
  {
    NamedNodeMap::const_iterator it = m_funs.find(name);
    if(it == m_funs.end())
    {
      return false;
    }
    fun = it->second;
    return true;
  }

  //
  // Construct an instance
  //
  class Instance* CreateInstance(class Evaluator& eval) const;

protected:

  //
  // Types
  //
  typedef std::map<String, Ast*> NamedNodeMap;

  //
  // Members
  //
  String        m_name;
  NamedNodeMap  m_vars;
  NamedNodeMap  m_funs;

};

//////////////////////////////////////////////////////////////////////////
//
// Runtime class instance
//

class Instance : public Variant::Resource
{
public:

  //
  // Construction
  //
  Instance(Class const* c) : m_class (c)
  {
  }

  //
  // Number of instance variables
  //
  virtual size_t GetVarCount() const
  {
    return m_vars.size();
  }

  //
  // Retrieve a variable
  //
  virtual bool FindVar(String const& name, VariantRef& ref) const
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
  // Retrieve a function
  //
  virtual bool FindFun(String const& name, Ast*& fun) const 
  {
    return m_class->FindFun(name, fun);
  }

protected:

  //
  // Types
  //
  typedef std::map<String, VariantRef> Variables;

  //
  // Members
  //
  Class const*  m_class;
  Variables     m_vars;

  //
  // Class required access for instantiation
  //
  friend class Class;

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
  // Retrieve parent scope
  //
  virtual Scope* GetParent() const
  {
    return m_parent;
  }

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
  virtual bool FindFun(String const& name, Ast*& fun) const 
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

protected:

  //
  // Retrieve a variable from local scope only
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
  // Retrieve a function from local scope only
  //
  virtual bool FindFunLocal(String const& name, Ast*& fun) const
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

protected:

  //
  // Retrieve a function
  //
  virtual bool FindFunLocal(String const& name, Ast*& fun) const
  {
    Functions::const_iterator it = m_funs.find(name);
    if(it == m_funs.end())
    {
      return false;
    }
    fun = it->second;
    return true;
  }

  //
  // Members
  //
  Functions m_funs;

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
  virtual bool FindFunLocal(String const& name, Ast*& fun) const
  {
    return m_inst->FindFun(name, fun);
  }

  //
  // Members
  //
  class Instance* m_inst;

};

//////////////////////////////////////////////////////////////////////////
//
// Function call scope
//

class CallScope : public Scope
{
public:

  //
  // Types
  //
  typedef std::vector<VariantRef> Arguments;

  //
  // Construction
  //
  CallScope(Scope* parent) : Scope(parent)
  {
  }

  //
  // Set parent scope
  //
  void SetParent(Scope* parent)
  {
    m_parent = parent;
  }

  //
  // Add an argument
  //
  virtual void AddArg(VariantRef ref)
  {
    m_args.push_back(ref);
  }

  //
  // Number of arguments
  //
  virtual size_t GetArgCount() const
  {
    return m_args.size();
  }

  //
  // Retrieve argument
  //
  virtual VariantRef const& GetArg(size_t index) const
  {
    return m_args.at(index);
  }

  //
  // Retrieve arguments
  //
  virtual Arguments const& GetArgs() const
  {
    return m_args;
  }

protected:

  //
  // Members
  //
  Arguments m_args;

};


#endif // CSCRIPT_RTSCOPE_H
