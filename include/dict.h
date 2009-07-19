//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_NATIVE_ARRAY_H
#define CSCRIPT_NATIVE_ARRAY_H

#include <native.h>
#include <object.h>
#include <enumerator.h>

#include <iterator>
#include <map>

__native_construct class Dictionary : public Object
{
public:

  //
  // Native call mapping
  //
  DEF_NATIVE_EVAL(Dictionary, Object);
  IMPL_NATIVE_GET(Dictionary, Object);
  IMPL_NATIVE_SET(Dictionary, Object);

  //
  // Map type
  //
  typedef std::map<Value, Value> ImplType;
  typedef ImplType::iterator Iter;

  ////////////////////////////////////////////////////////////////////////
  //
  // Native methods
  //

  //
  // Clear the map
  //
  __native_method void Clear()
  {
    m_map.clear();
  }

  //
  // Map size
  //
  __native_roprop int64 Length() const
  {
    return m_map.size();
  }

  //
  // Does the map contain a key
  //
  __native_method bool Contains(ValueCRef key) const
  {
    return m_map.count(key) > 0;
  }

  //
  // Insert an item
  //
  __native_method bool Insert(ValueCRef key, ValueCRef value)
  {
    return m_map.insert(std::make_pair(key, value)).second;
  }

  //
  // Remove an item
  //
  __native_method bool Remove(ValueCRef key)
  {
    Iter it = m_map.find(key);
    if(it != m_map.end())
    {
      m_map.erase(it);
      return true;
    }
    return false;
  }

  ////////////////////////////////////////////////////////////////////////
  //
  // Iterator class
  //

  class Enum : public Enumerator
  {
    Dictionary* m_dict;
    Iter m_it;
    Iter m_ie;
  
  public:

    Enum(Dictionary* dict) : 
    m_dict (dict)
    {
      Reset();
    }

    virtual void Reset() 
    {
      m_it = m_dict->m_map.begin();
      m_ie = m_dict->m_map.end();
    }

    virtual bool GetNext(Value& value) 
    {
      if(m_it == m_ie) 
      {
        return false;
      }
      
      value = m_it->second;

      ++m_it;
      
      return true;
    }

    virtual bool GetNext(Value& key, Value& value)
    {
      if(m_it == m_dict->m_map.end())
      {
        return false;
      }
      
      key   = m_it->first;
      value = m_it->second;
      
      ++m_it;
      
      return true;
    }

    virtual void MarkObjects(GCObjectVec& grey)
    {
      GC::Mark(grey, m_dict);
    }

  };

  ////////////////////////////////////////////////////////////////////////
  //
  // Overrides
  //

  //
  // Implement Object::Get
  //
  virtual Value Get(Value const& key)
  {
    return m_map[key];
  }

  //
  // Try to retrieve a member by key
  //
  virtual bool TryGet(Value const& key, Value& value)
  {
    ImplType::iterator it = m_map.find(key);
    if(it != m_map.end())
    {
      value = it->second;
      return true;
    }
    return NativeTryGet(key, value);
  }

  //
  // Set a member
  //
  virtual Value const& Set(Value const& key, Value const& value)
  {
    m_map[key] = value;
    return value;
  }

  //
  // Try to set a member
  //
  virtual bool TrySet(Value const& key, Value const& value)
  {
    m_map[key] = value;
    return true;
  }

  //
  // Implement Object::GetEnumerator
  //
  virtual Enumerator* GetEnumerator()
  {
    return new Enum(this);
  }

  //
  // Implement GC::MarkObjects
  //
  virtual void MarkObjects(GCObjectVec& grey)
  {
    // Mark object members
    Object::MarkObjects(grey);

    // Mark map contents
    for(Iter it = m_map.begin(); it != m_map.end(); ++it)
    {
      GC::Mark(grey, it->first);
      GC::Mark(grey, it->second);
    }
  }

private:

  //
  // Members
  //
  ImplType m_map;

};

#endif // CSCRIPT_NATIVE_ARRAY_H
