#ifndef CSCRIPT_CLASS_H
#define CSCRIPT_CLASS_H

#include "types.h"
#include "var.h"

class Function;

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
  // Class name
  //
  String const& GetName() const
  {
    return m_name;
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
  virtual void AddFun(String const& name, Function* node)
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
  virtual bool FindFun(String const& name, Function*& fun) const 
  {
    FunctionMap::const_iterator it = m_funs.find(name);
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
  typedef std::map<String, Function*> FunctionMap;
  typedef std::map<String, Ast*>      NamedNodeMap;

  //
  // Members
  //
  String        m_name;
  NamedNodeMap  m_vars;
  FunctionMap   m_funs;

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
  virtual bool FindFun(String const& name, Function*& fun) const 
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

#endif // CSCRIPT_CLASS_H
