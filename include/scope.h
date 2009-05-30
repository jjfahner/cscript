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
#include <function.h>

//////////////////////////////////////////////////////////////////////////
//
// Default scope implementation
//

class Scope : public Object
{
public:

  static const GCString& parentName;

  //
  // Variable map
  //
  typedef std::map<String, Value> VarMap;
  typedef VarMap::iterator Iter;

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
  // Retrieve parent scope or 0
  //
  virtual Scope* GetParent()
  {
    Iter it = m_vars.find(parentName);
    return it == m_vars.end() ? 0 : 
      dynamic_cast<Scope*>(
        it->second.GetObject());
  }

  //
  // Set parent pointer
  //
  virtual void SetParent(Scope* parent)
  {
    m_vars[parentName] = parent;
  }

  //
  // Add a new variable
  //
  virtual Value const& Add(String const& key, Value const& value, bool replace = false)
  {
    // Find in this scope
    Iter it = m_vars.find(key);
    if(it == m_vars.end())
    {
      m_vars[key] = value;
      return value;
    }

    // Replace
    if(replace)
    {
      it->second = value;
      return value;
    }

    // Duplicate variable
    throw std::runtime_error("Variable already declared");
  }

  //
  // Retrieve an existing variable
  //
  virtual Value Get(Value const& key)
  {
    // Find in this scope
    Iter it = m_vars.find(key);
    if(it != m_vars.end())
    {
      return it->second;
    }

    // Find in parent scope
    if(Scope* parent = GetParent())
    {
      return parent->Get(key);
    }
    
    // Unknown variable
    throw std::runtime_error("Variable not found");
  }

  //
  // Try to retrieve a variable
  //
  virtual bool TryGet(Value const& key, Value& value)
  {
    // Find in this scope
    Iter it = m_vars.find(key);
    if(it != m_vars.end())
    {
      value = it->second;
      return true;
    }

    // Find in parent scope
    if(Scope* parent = GetParent())
    {
      return parent->TryGet(key, value);
    }

    // Unknown variable
    return false;
  }

  //
  // Set an existing variable
  //
  virtual Value const& Set(Value const& key, Value const& value)
  {
    // Find in this scope
    Iter it = m_vars.find(key);
    if(it != m_vars.end())
    {
      return it->second = value;
    }

    // Find in parent scope
    if(Scope* parent = GetParent())
    {
      return parent->Set(key, value);
    }

    // Unknown variable
    throw std::runtime_error("Variable not found");
  }

  //
  // Try to set a variable
  //
  virtual bool TrySet(Value const& key, Value const& value)
  {
    // Find in this scope
    Iter it = m_vars.find(key);
    if(it != m_vars.end())
    {
      it->second = value;
      return true;
    }

    // Find in parent scope
    if(Scope* parent = GetParent())
    {
      return parent->TrySet(key, value);
    }

    // Unknown variable
    return false;
  }

  //
  // Unset a variable
  //
  virtual void Unset(Value const& key)
  {
    // Find in this scope
    Iter it = m_vars.find(key);
    if(it != m_vars.end())
    {
      m_vars.erase(it);
      return;
    }

    // Find in parent scope
    if(Scope* parent = GetParent())
    {
      parent->Unset(key);
      return;
    }
  }

protected:

  //
  // Override GC::MarkObjects
  //
  virtual void MarkObjects(GC::ObjectVec& grey)
  {
    // Mark object members
    Object::MarkObjects(grey);
    
    // Mark map contents
    for(Iter mi = m_vars.begin(); mi != m_vars.end(); ++mi)
    {
      if(GC::Object* o = mi->second.GetGCObject()) {
        GC::Mark(grey, o);
      }
    }
  }

  //
  // Members
  //
  VarMap m_vars;  

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
    // Store in map for GC reference
    Add("__object", inst);
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
  virtual Value Get(Value const& key)
  {
    // Find in object
    Value value;
    if(m_inst->TryGet(key, value))
    {
      return value;
    }

    // Continue in scope
    return Scope::Get(key);
  }

  //
  // Try to retrieve a variable
  //
  virtual bool TryGet(Value const& key, Value& value)
  {
    // Lookup in object
    if(m_inst->TryGet(key, value))
    {
      return true;
    }

    // Continue in scope
    return Scope::TryGet(key, value);
  }

  //
  // Set a variable
  //
  virtual Value const& Set(Value const& key, Value const& value)
  {
    // Try to set in instance
    if(m_inst->TrySet(key, value))
    {
      return value;
    }

    // Continue in scope
    return Scope::Set(key, value);
  }

  //
  // Try to set a variable
  //
  virtual bool TrySet(Value const& key, Value const& value)
  {
    // Try to set in instance
    if(m_inst->TrySet(key, value))
    {
      return true;
    }

    // Continue in scope
    return Scope::TrySet(key, value);
  }

protected:

  //
  // Members
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
      parent->Unset(m_name);
    }

    // Delegate to scope implementation
    Scope::SetParent(parent);

    // Reinsert into new parent
    parent->Add(m_name, this, true);
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
