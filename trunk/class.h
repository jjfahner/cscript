//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "typeinfo.h"

class Evaluator;
class MemberFunction;
class ConversionOperator;
class Constructor;

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
  Class(String const& name) : 
  m_name (name),
  m_constructor (0)
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
  // Fetch constructors
  //
  virtual Constructor* GetConstructor() const
  {
    return m_constructor;
  }

  //
  // Add a constructor
  //
  virtual void SetConstructor(Constructor* constructor)
  {
    if(m_constructor)
    {
      throw std::runtime_error("Class already has a constructor");
    }
    m_constructor = constructor;
  }
  
  //
  // Add a member variable
  //
  virtual void AddVariable(String const& name, Ast* node)
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
  virtual void AddFunction(String const& name, MemberFunction* node)
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
  virtual bool FindFun(String const& name, MemberFunction*& fun) const 
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
  // Add a conversion operator
  //
  virtual void AddConversion(ConversionOperator* node);

  //
  // Finc a conversion operator
  //
  virtual bool FindConversion(TypeInfo const& type, ConversionOperator*& node) const;

  //
  // Construct an instance
  //
  class Instance* CreateInstance(Evaluator* eval) const;

protected:

  //
  // Types
  //
  typedef std::map<String,    Ast*>                NamedNodeMap;
  typedef std::map<String,    MemberFunction*>     FunctionMap;
  typedef std::map<TypeInfo,  ConversionOperator*> ConversionMap;

  //
  // Members
  //
  String        m_name;
  Constructor*  m_constructor;
  NamedNodeMap  m_vars;
  FunctionMap   m_funs;
  ConversionMap m_conv;

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
  // Class
  //
  Class const* GetClass() const
  {
    return m_class;
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
  virtual bool FindFun(String const& name, MemberFunction*& fun) const 
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