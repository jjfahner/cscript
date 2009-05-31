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
#ifndef CSCRIPT_LISTOBJECT_H
#define CSCRIPT_LISTOBJECT_H

#include <cscript.h>
#include <enumerator.h>
#include <stubs.h>

#include <vector>

class List : public Object
{
public:

  DEF_NATIVE_EVAL(List, Object);
  IMPL_NATIVE_GET(List, Object);
  IMPL_NATIVE_SET(List, Object);

  //
  // The implementation list type
  //
  typedef std::vector<Value> ImplType;

  //
  // Iterator type
  //
  typedef ImplType::iterator Iterator;

  //
  // Retrieve enumerator
  //
  virtual Enumerator* GetEnumerator();

  //
  // Iterators
  //
  Iterator Begin() 
  {
    return m_list.begin();
  }
  Iterator End() 
  {
    return m_list.end();
  }

  //
  // Length of list
  //
  __native_roprop int64 Length()
  {
    return m_list.size();
  }

  //
  // Clear the list
  //
  __native_method void Clear()
  {
    m_list.clear();
  }

  //
  // Add to end of list
  //
  __native_method ValueCRef Append(ValueCRef value)
  {
    m_list.push_back(value);
    return value;
  }

  //
  // Append list contents
  //
  void Append(List* list)
  {
    m_list.insert(
      m_list.end(), 
      list->m_list.begin(), 
      list->m_list.end());
  }

  //
  // Retrieve item at front
  //
  __native_roprop ValueCRef Head()
  {
    if(m_list.size())
    {
      return m_list.front();
    }
    throw std::runtime_error("List is empty");
  }

  //
  // Retrieve item at back
  //
  __native_roprop ValueCRef Tail()
  {
    if(m_list.size())
    {
      return m_list.back();
    }
    throw std::runtime_error("List is empty");
  }

  //
  // Retrieve item at specified index
  //
  __native_method ValueCRef At(int64 index)
  {
    return GetAt(index);
  }

  //
  // Retrieve at specified index
  //
  Value& GetAt(int64 index)
  {
    if(index >= m_list.size())
    {
      m_list.resize((size_t)index + 1);
    }
    return m_list[(size_t)index];
  }

  //
  // Retrieve a member by key
  //
  virtual Value Get(Value const& key)
  {
    if(key.Type() == Value::tInt)
    {
      return GetAt(key.GetInt());
    }
    return NativeGet(key);
  }

  //
  // Try to retrieve a member by key
  //
  virtual bool TryGet(Value const& key, Value& value)
  {
    if(key.Type() == Value::tInt)
    {
      value = GetAt(key.GetInt());
      return true;
    }
    return NativeTryGet(key, value);
  }

  //
  // Set a member
  //
  virtual Value const& Set(Value const& key, Value const& value)
  {
    if(key.Type() == Value::tInt)
    {
      return GetAt(key.GetInt()) = value;
    }
    return NativeSet(key, value);
  }

  //
  // Try to set a member
  //
  virtual bool TrySet(Value const& key, Value const& value)
  {
    if(key.Type() == Value::tInt)
    {
      GetAt(key.GetInt()) = value;
      return true;
    }
    return NativeTrySet(key, value);
  }

protected:

  //
  // Implement garbage collector hook
  //
  virtual void MarkObjects(GC::ObjectVec& grey) 
  {
    // Mark object members
    Object::MarkObjects(grey);

    // Mark list entries
    Iterator it = m_list.begin();
    Iterator ie = m_list.end();
    for(; it != ie; ++it)
    {
      if(GC::Object* o = it->GetGCObject())
      {
        GC::Mark(grey, o);
      }
    }
  }

  //
  // The list instance
  // 
  ImplType m_list;

};

#endif // CSCRIPT_LISTOBJECT_H
