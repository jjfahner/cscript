//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
  DEF_NATIVE_CALLS(Dictionary, Object);

  //
  // Map type
  //
  typedef std::map<Value, Value> ImplType;
  typedef ImplType::iterator Iter;

  //
  // List type
  //
  virtual DataType* GetType();

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

  //
  // Retrieve a value by index
  //
  virtual bool TryGetAt(Value const& key, Value& value)
  {
    value = m_map[key];
    return true;
  }

  //
  // Try to set a pair
  //
  virtual bool TrySetAt(Value const& key, Value const& value)
  {
    m_map[key] = value;
    return true;
  }

  //
  // Implement Object::GetEnumerator
  //
  virtual Enumerator* GetEnumerator();

protected:

  //
  // Implement GC::MarkObjects
  //
  virtual void MarkObjects(GCObjectVec& grey);

private:

  friend class DictEnumerator;

  //
  // Members
  //
  ImplType m_map;

};

#endif // CSCRIPT_NATIVE_ARRAY_H
