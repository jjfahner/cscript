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
#include "value.h"
#include "typeinfo.h"
#include "variable.h"

class Evaluator;
class Function;
class MemberFunction;
class MemberVariable;
class ConversionOperator;
class Constructor;
class Destructor;
class Instance;

//////////////////////////////////////////////////////////////////////////
//
// Runtime class
//

class Class
{
public:

  //
  // Types
  //
  typedef std::map<String,    Ast*>                NamedNodeMap;
  typedef std::map<String,    MemberFunction*>     FunctionMap;
  typedef std::map<TypeInfo,  ConversionOperator*> ConversionMap;

  //
  // Construction
  //
  Class(String const& name) : 
  m_name (name),
  m_constructor (0),
  m_destructor (0)
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
  // Fetch the destructor
  //
  virtual Destructor* GetDestructor() const
  {
    return m_destructor;
  }

  //
  // Set the destructor
  //
  virtual void SetDestructor(Destructor* destructor)
  {
    if(m_destructor)
    {
      throw std::runtime_error("Class already has a destructor");
    }
    m_destructor = destructor;
  }

  //
  // Retrieve variables
  //
  NamedNodeMap const& GetVariables() const
  {
    return m_vars;
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
  // Retrieve methods
  //
  virtual FunctionMap const& GetMethods() const
  {
    return m_funs;
  }

  //
  // Add a member function
  //
  virtual void AddMethod(String const& name, MemberFunction* node)
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
  virtual bool FindMethod(String const& name, Function*& fun) const;

  //
  // Add a conversion operator
  //
  virtual void AddConversion(ConversionOperator* node);

  //
  // Find a conversion operator
  //
  virtual bool FindConversion(TypeInfo const& type, ConversionOperator*& node) const;

protected:

  //
  // Members
  //
  String        m_name;
  Constructor*  m_constructor;
  Destructor*   m_destructor;
  NamedNodeMap  m_vars;
  FunctionMap   m_funs;
  ConversionMap m_conv;

};

#endif // CSCRIPT_CLASS_H
