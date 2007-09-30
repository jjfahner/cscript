//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 JJ Fahner <jan-jaap@jan-jaap.net>
// This file is part of the cscript interpreter.
// CScript can be found at http://svn.jan-jaap.net/
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//////////////////////////////////////////////////////////////////////////
#ifndef CSCRIPT_CLASS_H
#define CSCRIPT_CLASS_H

#include "types.h"
#include "var.h"

class Function;
class Evaluator;

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
  class Instance* CreateInstance(Evaluator* eval) const;

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
