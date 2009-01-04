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
#ifndef CSCRIPT_SCOPE_H
#define CSCRIPT_SCOPE_H

#include "types.h"
#include "value.h"
#include "variable.h"
#include "function.h"

class Instance;
class Function;
class Class;

//////////////////////////////////////////////////////////////////////////
//
// Default scope implementation
//

class Scope : public Object
{
public:

  //
  // Types
  //
  typedef std::map<String, Class*> Classes;

  //
  // Construction
  //
  Scope(Scope* parent = 0);

  //
  // Scope Id
  //
  int GetScopeId()
  {
    return m_scopeId;
  }

  //
  // Scope name
  //
  String GetTypeName() const
  {
    char si[10];
    _itoa(m_scopeId, si, 10);
    return Object::GetTypeName() + " with id " + si;
  }

  //
  // Parent scope
  //
  virtual Scope* GetParent() const
  {
    return m_parent;
  }

  //
  // Attach/detach from parent scope
  //
  void Attach();
  void Detach();

  //////////////////////////////////////////////////////////////////////////
  //
  // Variables
  //

  //
  // Add a variable
  //
  virtual void AddVar(String const& name, Value const& value)
  {
    if(m_variables.count(name))
    {
      throw std::runtime_error("Variable already declared");
    }
    m_variables[name] = new RWVariable(value);
  }

  //
  // Retrieve a variable
  //
  virtual bool FindVar(String const& name, RValue*& ptr) const
  {
    ptr = 0;
    if(FindVarLocal(name, ptr))
    {
      return true;
    }
    if(m_parent)
    {
      return m_parent->FindVar(name, ptr);
    }
    return false;
  }

  //
  // Add variables to list
  //
  void AddObjects(Objects& objects) const
  {
    Variables::const_iterator it, ie;
    it = m_variables.begin();
    ie = m_variables.end();
    for(; it != ie; ++it)
    {
      if(it->second->Type() == Value::tObject)
      {
        objects.insert(&it->second->GetObject());
      }
    }
    if(m_parent)
    {
      m_parent->AddObjects(objects);
    }
  }

  //////////////////////////////////////////////////////////////////////////
  //
  // Functions
  //

  //
  // Add a function
  //
  virtual void AddFun(Function* fun)
  {
    throw std::runtime_error("Invalid scope for function declaration");
  }

  //
  // Retrieve a function
  //
  virtual bool FindMethod(String const& name, Function*& fun) const 
  {
    if(FindFunLocal(name, fun))
    {
      return true;
    }
    if(m_parent)
    {
      return m_parent->FindMethod(name, fun);
    }
    return false;
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
  // Don't allow explicit destruction
  //
  virtual ~Scope() {}

  //
  // Retrieve a variable from local scope
  //
  virtual bool FindVarLocal(String const& name, RValue*& ptr) const
  {
    Variables::const_iterator it = m_variables.find(name);
    if(it == m_variables.end())
    {
      return false;
    }
    ptr = it->second;
    return true;
  }

  //
  // Retrieve a function from local scope
  //
  virtual bool FindFunLocal(String const& name, Function*& node) const
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
  Scope* m_parent;
  int m_scopeId;
  static int g_scopeId;
};

//////////////////////////////////////////////////////////////////////////
//
// Global scope. Despite the name, GlobalScope is not *the* top-level
// scope, but merely *one* of the top-level scopes.
//

class GlobalScope : public Scope
{
public:

  //
  // Construction
  //
  GlobalScope(Scope* parent = 0) : Scope(parent)
  {
  }

  //////////////////////////////////////////////////////////////////////////
  //
  // Functions
  //

  //
  // Add a function
  //
  virtual void AddFun(Function* fun);

  //
  // Add class to this scope
  //
  virtual void AddClass(Class* c);

protected:

  //
  // Retrieve a function
  //
  virtual bool FindFunLocal(String const& name, Function*& fun) const
  {
    Variables::const_iterator it = m_variables.find(name);
    if(it == m_variables.end())
    {
      return false;
    }

    if(it->second->Type() != Value::tObject)
    {
      return false;
    }

    fun = dynamic_cast<Function*>(&it->second->GetObject());
    return fun != 0;
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
  // Variables may not be added to class scopes
  //
  virtual void AddVar(String const& name, Value const& value)
  {
    throw std::runtime_error("Cannot add variables to class scope");
  }

protected:

  //
  // Retrieve a local variable
  //
  virtual bool FindVarLocal(String const& name, RValue*& ref) const;

  //
  // Retrieve a local function
  //
  virtual bool FindFunLocal(String const& name, Function*& fun) const;

  //
  // Members
  //
  Instance* m_inst;

};

#endif // CSCRIPT_RTSCOPE_H
