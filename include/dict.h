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

class Dictionary : public Object
{
public:

  //
  // Native call mapping
  //
  DEF_NATIVE_CALLS(Dictionary, Object)

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
    Iter m_it;
    Iter m_ie;
  
  public:

    Enum(Iter it, Iter ie) : m_it (it), m_ie (ie) {
    }

    virtual void Reset() {
      m_it = m_ie;
    }

    virtual bool GetNext(Value& value) {
      if(m_it == m_ie) {
        return false;
      }
      value = (++m_it)->second;
      return true;
    }

    virtual bool GetNext(Value& key, Value& value)
    {
      if(m_it == m_ie) 
      {
        return false;
      }
      
      key   = m_it->first;
      value = m_it->second;
      
      ++m_it;
      
      return true;
    }
  };

  ////////////////////////////////////////////////////////////////////////
  //
  // Overrides
  //

  //
  // Implement Object::Get
  //
//   virtual Value const& Get(ValueCRef key)
//   {
//     return m_map[key];
//   }

  //
  // Implement Object::Set
  //
//   virtual Value const& Set(Value const& key, Value const& value)
//   {
//     m_map[key] = value;
//     return value;
//   }

  //
  // Implement Object::GetEnumerator
  //
  virtual Enumerator* GetEnumerator()
  {
    return new Enum(m_map.begin(), m_map.end());
  }

  //
  // Implement GC::Mark
  //
  virtual void Mark(GC::ObjectVec& grey)
  {
    // Mark object members
    Object::MarkObjects(grey);

    // Mark map contents
    for(Iter it = m_map.begin(); it != m_map.end(); ++it)
    {
      if(GC::Object* o = it->first.GetGCObject()) {
        grey.push_back(o);
      }
      if(GC::Object* o = it->second.GetGCObject()) {
        grey.push_back(o);
      }
    }
  }

private:

  //
  // Members
  //
  ImplType m_map;

};

#endif // CSCRIPT_NATIVE_ARRAY_H
