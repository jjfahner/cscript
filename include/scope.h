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

#include <cscript.h>
#include <value.h>
#include <variable.h>
#include <function.h>

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
  Scope(Scope* parent = 0)
  {
    if(parent)
    {
      SetParent(parent);
    }
  }

  //
  // Parent scope
  //
  virtual Scope* GetParent()
  {
    RValue* parent;
    if(Find("__parent", parent))
    {
      return dynamic_cast<Scope*>(parent->GetObject());
    }
    return 0;
  }
  virtual void SetParent(Scope* parent)
  {
    GetLValue("__parent") = parent;
  }

  //
  // Retrieve a variable without owner
  //
  virtual bool Lookup(String const& name, RValue*& ptr)
  {
    Object* owner;
    return Lookup(name, ptr, owner, false);
  }

  //
  // Retrieve a variable with owner
  //
  virtual bool Lookup(String const& name, RValue*& ptr, Object*& owner, bool scopeIsOwner = false)
  {
    ptr = 0;
    owner = 0;
    
    if(Object::Find(name, ptr))
    {
      if(scopeIsOwner)
      {
        owner = this;
      }
      return true;
    }
    
    if(Scope* parent = GetParent())
    {
      return parent->Lookup(name, ptr, owner, scopeIsOwner);
    }

    return false;
  }

protected:

  //
  // Hide Find function, is replaced by Lookup
  //
  using Object::Find;

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
  virtual bool Lookup(String const& name, RValue*& ptr, Object*& owner, bool scopeIsOwner = false)
  {
    // Find instance member
    if(m_inst->Find(name, ptr))
    {
      owner = m_inst;
      return true;
    }

    // Look in parent scope
    return Scope::Lookup(name, ptr, owner, scopeIsOwner);
  }

protected:

  //
  // MemberMap
  //
  Object* m_inst;

};

//////////////////////////////////////////////////////////////////////////
//
// Namespace scope
//
class NamespaceScope : public Scope
{
public:

  //
  // Construction
  //
  NamespaceScope(Scope* parent, String name) :
  Scope  (parent),
  m_name (name)
  {
    if(parent)
    {
      SetParent(parent);
    }
  }

  //
  // Set parent
  //
  virtual void SetParent(Scope* parent)
  {
    // Parent mandatory for namespace scopes
    if(parent == 0)
    {
      throw std::runtime_error("Namespace must have a parent");
    }

    // Remove namespace scope from current parent
    if(Scope* parent = GetParent())
    {
      parent->Remove(m_name);
    }

    // Delegate to scope implementation
    Scope::SetParent(parent);

    // Reinsert into new parent
    parent->GetLValue(m_name) = this;
  }

  //
  // Retrieve name
  //
  String GetName() const
  {
    return m_name;
  }

private:

  //
  // Members
  //
  String m_name;

};

#endif // CSCRIPT_RTSCOPE_H
