//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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

class Function;

//////////////////////////////////////////////////////////////////////////
//
// Default scope implementation
//

class Scope : public Object
{
public:

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
  // Retrieve a variable without owner
  //
  virtual bool Lookup(String const& name, RValue*& ptr)
  {
    Object* owner;
    return Lookup(name, ptr, owner);
  }

  //
  // Retrieve a variable with owner
  //
  virtual bool Lookup(String const& name, RValue*& ptr, Object*& owner)
  {
    ptr = 0;
    owner = 0;
    if(Object::Find(name, ptr))
    {
      return true;
    }
    if(m_parent)
    {
      return m_parent->Lookup(name, ptr, owner);
    }
    return false;
  }

protected:

  //
  // Hide Find function, is replaced by Lookup
  //
  using Object::Find;

  //
  // Members
  //
  Scope*    m_parent;

};

//////////////////////////////////////////////////////////////////////////
//
// Object scope
//
class ObjectScope : public Scope
{
public:

  //
  // Construction
  //
  ObjectScope(Scope* parent, Object* inst) :
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
  virtual bool Lookup(String const& name, RValue*& ptr, Object*& owner)
  {
    if(m_inst->Find(name, ptr))
    {
      owner = m_inst;
      return true;
    }
    return Scope::Lookup(name, ptr, owner);
  }

protected:

  //
  // Members
  //
  Object* m_inst;

};

#endif // CSCRIPT_RTSCOPE_H
