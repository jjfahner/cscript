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
#include <ident.h>
#include <value.h>
#include <function.h>

//////////////////////////////////////////////////////////////////////////
//
// Default scope implementation
//

class Scope : public Object
{
public:

  //
  // Variable map
  //
  typedef std::map<IdentId, Value> VarMap;
  typedef VarMap::iterator Iter;

  //
  // Construction
  //
  Scope(Scope* parent = 0, Object* object = 0) :
  m_parent  (0),
  m_object  (0),
  m_refs    (0)
  {
    if(parent)
    {
      SetParent(parent);
    }
    if(object)
    {
      SetObject(object);
    }
  }

  //
  // Reference the scope
  //
  void AddRef()
  {
    // Set own ref
    ++m_refs;

    // Set parent ref
    if(m_parent)
    {
      return m_parent->AddRef();
    }
  }
  
  //
  // Is Scope reffed
  //
  bool HasRefs() const
  {
    return m_refs != 0;
  }

  //
  // Clear the scope
  //
  virtual void Clear()
  {
    m_parent = 0;
    m_object = 0;
    m_refs   = 0;
    m_vars.clear();
  }

  //
  // Retrieve parent scope
  //
  virtual Scope* GetParent() const
  {
    return m_parent;
  }

  //
  // Set parent pointer
  //
  virtual void SetParent(Scope* parent)
  {
    m_parent = parent;
  }

  //
  // Retrieve object
  //
  virtual Object* GetObject() const
  {
    return m_object;
  }

  //
  // Set object
  //
  virtual void SetObject(Object* object)
  {
    m_object = object;
  }

  //
  // Add a new variable
  //
  virtual Value const& Add(Value const& vKey, Value const& value, bool replace = false)
  {
    Identifier key(vKey);

    // Assign or replace
    if (replace || m_vars.count(key) == 0)
    {
      m_vars[key] = value;
      return value;
    }

    // Duplicate variable
    throw std::runtime_error("Variable already declared");
  }

  //
  // Try to retrieve a variable
  //
  virtual bool TryGet(Value const& key, Value& value)
  {
    // Find in local scope
    if(m_vars.size())
    {
      Iter it = m_vars.find(Identifier(key));
      if(it != m_vars.end())
      {
        value = it->second;
        return true;
      }
    }

    // Find in object scope
    if(m_object && m_object->TryGet(key, value))
    {
      return true;
    }

    // Find in parent scope
    if(m_parent)
    {
      return m_parent->TryGet(key, value);
    }

    // Unknown variable
    return Object::TryGet(key, value);
  }

  //
  // Try to set a variable
  //
  virtual bool TrySet(Value const& key, Value const& value)
  {
    // Find in local scope
    if(m_vars.size())
    {
      Iter it = m_vars.find(Identifier(key));
      if(it != m_vars.end())
      {
        it->second = value;
        return true;
      }
    }

    // Find in object scope
    if(m_object && m_object->TrySet(key, value))
    {
      return true;
    }

    // Find in parent scope
    if(m_parent)
    {
      return m_parent->TrySet(key, value);
    }

    // Unknown variable
    return Object::TrySet(key, value);
  }

  //
  // Unset a variable
  //
  virtual bool Unset(Value const& key)
  {
    // Unset in local scope
    if(m_vars.size())
    {
      Iter it = m_vars.find(Identifier::Lookup(key.GetString()));
      if(it != m_vars.end())
      {
        m_vars.erase(it);
        return true;
      }
    }

    // Unset in object scope
    if(m_object && m_object->Unset(key))
    {
      return true;
    }

    // Unset in parent scope
    if(Scope* parent = GetParent())
    {
      return parent->Unset(key);
    }

    // Failed
    return Object::Unset(key);
  }

protected:

  //
  // Override GC::MarkObjects
  //
  virtual void MarkObjects(GCObjectVec& grey)
  {
    // Mark Object
    Object::MarkObjects(grey);

    // Mark members
    if(m_parent) GC::Mark(grey, m_parent);
    if(m_object) GC::Mark(grey, m_object);
    
    // Mark map contents
    for(Iter mi = m_vars.begin(); 
        mi != m_vars.end(); ++mi)
    {
      GC::Mark(grey, mi->second);
    }
  }

  //
  // Members
  //
  Scope*    m_parent;
  Object*   m_object;
  int       m_refs;
  VarMap    m_vars;

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
  // Clear the scope
  //
  virtual void Clear()
  {
    m_name.clear();
    Scope::Clear();
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
