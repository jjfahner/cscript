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
  typedef std::map<String, RValue*>   Variables;
  typedef std::map<String, Function*> Functions;
  typedef std::map<String, Class*>    Classes;

  //
  // Construction
  //
  Scope(Scope* parent = 0) : m_parent (0)
  {
    if(parent)
    {
      SetParent(parent);
    }
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
    if(m_parent)
    {
      delete m_members["__parent"];
    }
    m_parent = parent;
    m_members["__parent"] = new ROVariable(parent);
  }

  //
  // Retrieve a variable
  //
  virtual bool Lookup(String const& name, RValue*& ptr) const
  {
    ptr = 0;
    if(Object::Find(name, ptr))
    {
      return true;
    }
    if(m_parent)
    {
      return m_parent->Lookup(name, ptr);
    }
    return false;
  }

  virtual void Add(String const& name, RValue* value)
  {
    if(Contains(name))
    {
      throw std::runtime_error("Variable already declared");
    }
    m_members[name] = value;
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

  using Object::Find;

  //
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
  GlobalScope(Scope* parent = 0) :
  Scope (parent)
  {
  }

  //
  // Add class to this scope
  //
  virtual void AddClass(Class* c);

protected:

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
  ClassScope(Scope* parent, Object* inst) :
  Scope  (parent),
  m_inst (inst)
  {
  }

  //
  // Object instance
  //
  Object* GetObject() const
  {
    return m_inst;
  }

  //
  // Retrieve a variable
  //
  virtual bool Lookup(String const& name, RValue*& ptr) const
  {
    if(m_inst->Find(name, ptr))
    {
      return true;
    }
    return Scope::Lookup(name, ptr);
  }

  // TODO implement fixed-flag on Object::m_members
//   //
//   // Variables may not be added to class scopes
//   //
//   virtual void AddVar(String const& name, Value const& value)
//   {
//     throw std::runtime_error("Cannot add variables to class scope");
//   }

protected:

  //
  // Members
  //
  Object* m_inst;

};

#endif // CSCRIPT_RTSCOPE_H
